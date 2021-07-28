// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::BTreeMap;
use std::io::{self, Read, Write};
use std::string::FromUtf8Error;

use aluvm::data::{FloatLayout, IntLayout, MaybeNumber, Number, NumberLayout};
use aluvm::libs::constants::ISAE_SEGMENT_MAX_LEN;
use aluvm::libs::{LibId, LibSeg, LibSegOverflow};
use amplify::hex::ToHex;
use amplify::{IoError, Wrapper};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Symbols {
    /// External routine names
    pub externals: Vec<String>,
    /// Map of local routine names to code offsets
    pub routines: BTreeMap<String, u16>,
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
    pub symbols: Symbols,
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
        let mut routines = BTreeMap::new();
        for _ in 0..len {
            reader.read_exact(&mut byte)?;
            let mut name = vec![0u8; byte[0] as usize];
            reader.read_exact(&mut name)?;
            let name = String::from_utf8(name).map_err(|err| ModuleError::RoutineNonUtf8(err))?;

            reader.read_exact(&mut word)?;
            let offset = u16::from_le_bytes(word);
            routines.insert(name, offset);
        }

        reader.read_exact(&mut word)?;
        let len = u16::from_le_bytes(word);
        let mut externals = Vec::with_capacity(byte[0] as usize);
        for _ in 0..len {
            reader.read_exact(&mut byte)?;
            let mut name = vec![0u8; byte[0] as usize];
            reader.read_exact(&mut name)?;
            let name = String::from_utf8(name).map_err(|err| ModuleError::ExternalNonUtf8(err))?;
            externals.push(name);
        }

        Ok(Module { isae, code, data, libs, vars, symbols: Symbols { externals, routines } })
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
            writer.write(id.as_inner())?;
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
        writer.write(&(self.symbols.routines.len() as u16).to_le_bytes())?;
        for (name, offset) in &self.symbols.routines {
            // TODO: Control length of routine names
            writer.write(&[name.len() as u8])?;
            writer.write(name.as_bytes())?;
            writer.write(&offset.to_le_bytes())?;
        }
        // TODO: Control that the number of external symbols does not exceeds u16::MAX
        writer.write(&(self.symbols.externals.len() as u16).to_le_bytes())?;
        for name in &self.symbols.externals {
            // TODO: Control length of external calls
            writer.write(&[name.len() as u8])?;
            writer.write(name.as_bytes())?;
        }
        Ok(())
    }
}
