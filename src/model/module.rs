// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::{BTreeMap, BTreeSet};
use std::io::{self, Read, Write};
use std::string::FromUtf8Error;
use std::vec::IntoIter;

use aluvm::data::encoding::{Decode, DecodeError, Encode, EncodeError, MaxLenByte, MaxLenWord};
use aluvm::data::{ByteStr, FloatLayout, IntLayout, Layout, MaybeNumber, Number, NumberLayout};
use aluvm::libs::constants::{ISAE_SEGMENT_MAX_LEN, LIBS_SEGMENT_MAX_COUNT};
use aluvm::libs::{LibId, LibSeg, LibSegOverflow, LibSite};
use amplify::IoError;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum CallTableError {
    /// library `{0}` is not found
    LibNotFound(LibId),

    /// call table for library `{0}` is not found
    LibTableNotFound(LibId),

    /// routine reference #`{1}` entry in library `{0}` call table is not found
    RoutineNotFound(LibId, u16),

    /// number of external routine calls exceeds maximal number of jumps allowed by VM's `cy0`
    TooManyRoutines,

    /// number of external libraries exceeds maximum
    TooManyLibs,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct CallRef {
    pub routine: String,
    pub sites: BTreeSet<u16>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct CallTable(BTreeMap<LibId, Vec<CallRef>>);

impl CallTable {
    pub fn get_mut(&mut self, site: LibSite) -> Result<&mut CallRef, CallTableError> {
        self.0
            .get_mut(&site.lib)
            .ok_or(CallTableError::LibTableNotFound(site.lib))?
            .get_mut(site.pos as usize)
            .ok_or(CallTableError::RoutineNotFound(site.lib, site.pos))
    }

    pub fn find_or_insert(&mut self, id: LibId, routine: &str) -> Result<u16, CallTableError> {
        if self.0.len() >= u16::MAX as usize {
            return Err(CallTableError::TooManyRoutines);
        }
        if self.0.len() >= LIBS_SEGMENT_MAX_COUNT {
            return Err(CallTableError::TooManyLibs);
        }
        let vec = self.0.entry(id).or_default();
        let pos =
            vec.iter_mut().position(|callref| callref.routine == routine).unwrap_or_else(|| {
                let callref = CallRef { routine: routine.to_owned(), sites: bset![] };
                vec.push(callref);
                vec.len() - 1
            });
        Ok(pos as u16)
    }

    pub fn routines(&self) -> IntoIter<&str> {
        self.0
            .iter()
            .flat_map(|(id, routines)| {
                routines.into_iter().map(|call_ref| call_ref.routine.as_str())
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn call_refs(&self) -> IntoIter<(LibId, &str, &BTreeSet<u16>)> {
        self.0
            .iter()
            .flat_map(|(id, routines)| {
                routines
                    .into_iter()
                    .map(move |call_ref| (*id, call_ref.routine.as_str(), &call_ref.sites))
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum DataType {
    ByteStr(Option<Vec<u8>>),
    Int(IntLayout, MaybeNumber),
    Float(FloatLayout, MaybeNumber),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Variable {
    pub info: String,
    pub data: DataType,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Module {
    pub isae: String,
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    pub libs: LibSeg,
    pub vars: Vec<Variable>,
    pub imports: CallTable,
    /// Map of local routine names to code offsets
    pub exports: BTreeMap<String, u16>,
}

#[derive(Clone, Eq, PartialEq, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum ModuleError {
    /// end of data is reached before the complete module read
    /// \n
    /// details: {0}
    #[from]
    #[from(io::Error)]
    Io(IoError),

    /// length of ISA extensions segment is {0} exceeds limit
    IsaeLengthLimExceeded(usize),

    /// module contains too many libraries exceeding per-module lib limit
    #[from(LibSegOverflow)]
    LibCountLimExceeded,

    /// input variable description has a non-UTF8 encoding
    /// \n
    /// details: {0}
    VarNonUtf8(FromUtf8Error),

    /// routine symbol name has a non-UTF8 encoding
    /// \n
    /// details: {0}
    RoutineNonUtf8(FromUtf8Error),

    /// external call symbol has a non-UTF8 encoding
    /// \n
    /// details: {0}
    ExternalNonUtf8(FromUtf8Error),

    /// unknown type byte `{0}` for input variable having description "{1}"
    VarUnknownType(u8, String),

    /// wrong sign integer layout byte `{0}` for input variable having description "{1}"
    VarWrongSignByte(u8, String),

    /// layout size ({layout_bytes} bytes) does not match {data_bytes} size of the default value
    /// for variable with description "{info}"
    VarWrongLayout { layout_bytes: u16, data_bytes: u16, info: String },

    /// unknown float layout type `{0}` for input variable having description "{1}"
    VarWrongFloatType(u8, String),
}

impl Encode for DataType {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        match self {
            DataType::ByteStr(bytestr) => {
                return Ok(0xFF_u8.encode(&mut writer)?
                    + bytestr.as_ref().map(ByteStr::with).encode(&mut writer)?)
            }
            DataType::Int(layout, default) => (Layout::from(*layout), default),
            DataType::Float(layout, default) => (Layout::from(*layout), default),
        }
        .encode(&mut writer)
    }
}

impl Decode for DataType {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(match u8::decode(&mut reader)? {
            0xFF => {
                let data: Vec<u8> = MaxLenWord::decode(&mut reader)?.release();
                let inner = if data.is_empty() { None } else { Some(data) };
                DataType::ByteStr(inner)
            }

            i if i <= 1 => DataType::Int(
                IntLayout { signed: i == 1, bytes: u16::decode(&mut reader)? },
                MaybeNumber::decode(&mut reader)?,
            ),

            f => DataType::Float(
                FloatLayout::with(f).ok_or(DecodeError::FloatLayout(f))?,
                MaybeNumber::decode(&mut reader)?,
            ),
        })
    }
}

impl Encode for Variable {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.info.encode(&mut writer)? + self.data.encode(&mut writer)?)
    }
}

impl Decode for Variable {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(Variable { info: Decode::decode(&mut reader)?, data: Decode::decode(&mut reader)? })
    }
}

impl Encode for CallRef {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.routine.encode(&mut writer)? + MaxLenWord::new(&self.sites).encode(&mut writer)?)
    }
}

impl Decode for CallRef {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(CallRef {
            routine: Decode::decode(&mut reader)?,
            sites: MaxLenWord::decode(&mut reader)?.release(),
        })
    }
}

impl Encode for CallTable {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        let len = self.0.len() as u8;
        let mut count = len.encode(&mut writer)?;
        for (lib, map) in &self.0 {
            count += lib.encode(&mut writer)?;
            count += MaxLenWord::new(map).encode(&mut writer)?;
        }
        Ok(count)
    }
}

impl Decode for CallTable {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let len = u8::decode(&mut reader)?;
        let mut table = bmap! {};
        for _ in 0..len {
            table.insert(LibId::decode(&mut reader)?, MaxLenWord::decode(&mut reader)?.release());
        }
        Ok(CallTable(table))
    }
}

impl Module {
    pub fn read(mut reader: impl Read) -> Result<Module, ModuleError> {
        let mut byte = [0u8; 1];
        let mut word = [0u8; 2];

        let mut isae = String::with_capacity(ISAE_SEGMENT_MAX_LEN);
        loop {
            reader.read_exact(&mut byte)?;
            if byte[0] == 0 {
                break;
            }
            isae.push(byte[0].into());
        }

        if isae.len() > ISAE_SEGMENT_MAX_LEN {
            return Err(ModuleError::IsaeLengthLimExceeded(isae.len()));
        }

        reader.read_exact(&mut word)?;
        let len = u16::from_le_bytes(word);
        let mut code = vec![0u8; len as usize];
        reader.read_exact(&mut code)?;

        reader.read_exact(&mut word)?;
        let len = u16::from_le_bytes(word);
        let mut data = vec![0u8; len as usize];
        reader.read_exact(&mut data)?;

        reader.read_exact(&mut byte)?;
        let mut libs = Vec::with_capacity(byte[0] as usize);
        let mut id = [0u8; 32];
        for _ in 0..byte[0] {
            reader.read_exact(&mut id)?;
            libs.push(LibId::from_bytes(id));
        }
        let libs = LibSeg::with(libs.into_iter())?;

        reader.read_exact(&mut byte)?;
        let mut vars = Vec::with_capacity(byte[0] as usize);
        for _ in 0..byte[0] {
            reader.read_exact(&mut byte)?;
            let mut info = vec![0u8; byte[0] as usize];
            reader.read_exact(&mut info)?;
            let info = String::from_utf8(info).map_err(|err| ModuleError::VarNonUtf8(err))?;
            reader.read_exact(&mut byte)?;
            let data = match byte[0] {
                0 => {
                    reader.read_exact(&mut byte)?;
                    if byte[0] == 0 {
                        DataType::ByteStr(None)
                    } else {
                        let mut default = vec![0u8; byte[0] as usize];
                        reader.read_exact(&mut default)?;
                        DataType::ByteStr(Some(default))
                    }
                }
                1 => {
                    reader.read_exact(&mut byte)?;
                    let signed = match byte[0] {
                        0 => false,
                        1 => true,
                        wrong => return Err(ModuleError::VarWrongSignByte(wrong, info)),
                    };
                    reader.read_exact(&mut word)?;
                    let bytes = u16::from_le_bytes(word);
                    reader.read_exact(&mut word)?;
                    let layout = IntLayout { signed, bytes };
                    let len = u16::from_le_bytes(word);
                    if len == 0 {
                        DataType::Int(layout, MaybeNumber::none())
                    } else {
                        let mut number = vec![0u8; len as usize];
                        reader.read_exact(&mut number)?;
                        DataType::Int(
                            layout,
                            Number::with(&number, layout)
                                .ok_or(ModuleError::VarWrongLayout {
                                    layout_bytes: layout.bytes,
                                    data_bytes: len,
                                    info: info.clone(),
                                })?
                                .into(),
                        )
                    }
                }
                2 => {
                    reader.read_exact(&mut byte)?;
                    let layout = match byte[0] {
                        x if x == FloatLayout::BFloat16 as u8 => FloatLayout::BFloat16,
                        x if x == FloatLayout::IeeeHalf as u8 => FloatLayout::IeeeHalf,
                        x if x == FloatLayout::IeeeSingle as u8 => FloatLayout::IeeeSingle,
                        x if x == FloatLayout::IeeeDouble as u8 => FloatLayout::IeeeDouble,
                        x if x == FloatLayout::IeeeQuad as u8 => FloatLayout::IeeeQuad,
                        x if x == FloatLayout::IeeeOct as u8 => FloatLayout::IeeeOct,
                        x if x == FloatLayout::X87DoubleExt as u8 => FloatLayout::X87DoubleExt,
                        x if x == FloatLayout::FloatTapered as u8 => FloatLayout::FloatTapered,
                        wrong => return Err(ModuleError::VarWrongFloatType(wrong, info)),
                    };
                    let len = u16::from_le_bytes(word);
                    if len == 0 {
                        DataType::Float(layout, MaybeNumber::none())
                    } else {
                        let mut number = vec![0u8; len as usize];
                        reader.read_exact(&mut number)?;
                        DataType::Float(
                            layout,
                            Number::with(&number, layout)
                                .ok_or(ModuleError::VarWrongLayout {
                                    layout_bytes: layout.bytes(),
                                    data_bytes: len,
                                    info: info.clone(),
                                })?
                                .into(),
                        )
                    }
                }
                unknown => return Err(ModuleError::VarUnknownType(unknown, info.clone())),
            };
            let v = Variable { info, data };
            vars.push(v);
        }

        reader.read_exact(&mut word)?;
        let len = u16::from_le_bytes(word);
        let mut exports = BTreeMap::new();
        for _ in 0..len {
            reader.read_exact(&mut byte)?;
            let mut name = vec![0u8; byte[0] as usize];
            reader.read_exact(&mut name)?;
            let name = String::from_utf8(name).map_err(|err| ModuleError::RoutineNonUtf8(err))?;

            reader.read_exact(&mut word)?;
            let offset = u16::from_le_bytes(word);
            exports.insert(name, offset);
        }

        reader.read_exact(&mut word)?;
        let len = u16::from_le_bytes(word);
        let mut imports = CallTable::default();
        /* TODO: Rewrite
        for _ in 0..len {
            reader.read_exact(&mut byte)?;
            let mut data = vec![0u8; byte[0] as usize];
            reader.read_exact(&mut data)?;
            let routine =
                String::from_utf8(data).map_err(|err| ModuleError::ExternalNonUtf8(err))?;
            reader.read_exact(&mut word);
            let count = u16::from_le_bytes(word);
            let mut sites = bset! {};
            for _ in 0..count {
                reader.read_exact(&mut word);
                let site = u16::from_le_bytes(word);
                sites.insert(site);
            }
            imports.push(CallRef { routine, sites });
        }
         */

        Ok(Module { isae, code, data, vars, libs, imports, exports })
    }

    pub fn write(&self, mut writer: impl Write) -> io::Result<()> {
        writer.write(self.isae.as_bytes())?;
        writer.write(&[0u8])?;
        writer.write(&(self.code.len() as u16).to_le_bytes())?;
        writer.write(&self.code)?;
        writer.write(&(self.data.len() as u16).to_le_bytes())?;
        writer.write(&self.data)?;
        writer.write(&[self.libs.into_iter().count() as u8])?;
        for id in &self.libs {
            writer.write(id.as_bytes())?;
        }
        // TODO: Control maximal number of variables
        writer.write(&[self.vars.iter().count() as u8])?;
        for v in &self.vars {
            // TODO: Control length of variable info
            writer.write(&[v.info.len() as u8])?;
            writer.write(v.info.as_bytes())?;
            match &v.data {
                DataType::ByteStr(None) => {
                    writer.write(&[0u8])?;
                    writer.write(&[0u8, 0u8])?;
                }
                DataType::ByteStr(Some(default)) => {
                    writer.write(&[0u8])?;
                    // TODO: Control length of default data
                    writer.write(&(default.len() as u16).to_le_bytes())?;
                    writer.write(&default)?;
                }
                DataType::Int(layout, default) => {
                    writer.write(&[1u8])?;
                    writer.write(&[layout.signed as u8])?;
                    writer.write(&layout.bytes.to_le_bytes())?;
                    match **default {
                        Some(default) => {
                            writer.write(&default.len().to_le_bytes())?;
                            writer.write(default.as_ref())?;
                        }
                        None => {
                            writer.write(&[0u8, 0u8])?;
                        }
                    }
                }
                DataType::Float(layout, default) => {
                    writer.write(&[2u8])?;
                    writer.write(&[*layout as u8])?;
                    match **default {
                        Some(default) => {
                            writer.write(&default.len().to_le_bytes())?;
                            writer.write(default.as_ref())?;
                        }
                        None => {
                            writer.write(&[0u8, 0u8])?;
                        }
                    }
                }
            }
        }
        // TODO: Control that the number of routines does not exceeds u16::MAX
        writer.write(&(self.exports.len() as u16).to_le_bytes())?;
        for (name, offset) in &self.exports {
            // TODO: Control length of routine names
            writer.write(&[name.len() as u8])?;
            writer.write(name.as_bytes())?;
            writer.write(&offset.to_le_bytes())?;
        }
        /* TODO: Rewrite
        // TODO: Control that the number of external symbols does not exceeds u16::MAX
        writer.write(&(self.imports.len() as u16).to_le_bytes())?;
        for routne_ref in &self.imports {
            // TODO: Control number of external calls
            let name = routne_ref.name;
            writer.write(&[name.len() as u8])?;
            writer.write(name.as_bytes())?;
            writer.write(&(routne_ref.sites.len() as u16).to_le_bytes())?;
            for site in routne_ref.sites {
                writer.write(&site.to_le_bytes())?;
            }
        }
         */
        Ok(())
    }
}