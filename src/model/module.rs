// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{self, Display, Formatter, Write as WriteTrait};
use std::io::{self, Read, Write};
use std::string::FromUtf8Error;
use std::vec::IntoIter;

use aluvm::data::encoding::{Decode, DecodeError, Encode, EncodeError, MaxLenWord};
use aluvm::data::{ByteStr, FloatLayout, IntLayout, MaybeNumber, Number, NumberLayout};
use aluvm::library::constants::LIBS_SEGMENT_MAX_COUNT;
use aluvm::library::{Lib, LibId, LibSegOverflow, LibSite};
use amplify::hex::format_hex;
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
    #[inline]
    pub fn count(&self) -> u16 { self.0.len() as u16 }

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

    pub fn routines(&self) -> IntoIter<(LibId, &str)> {
        self.0
            .iter()
            .flat_map(|(id, routines)| {
                routines.iter().map(move |call_ref| (*id, call_ref.routine.as_str()))
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn call_refs(&self) -> IntoIter<(LibId, &str, &BTreeSet<u16>)> {
        self.0
            .iter()
            .flat_map(|(id, routines)| {
                routines
                    .iter()
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
    pub(crate) inner: Lib,
    pub vars: Vec<Variable>,
    pub imports: CallTable,
    /// Map of local routine names to code offsets
    pub exports: BTreeMap<String, u16>,
}

impl Module {
    #[inline]
    pub fn as_static_lib(&self) -> &Lib { &self.inner }
}

impl Display for CallRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "`{}` called from", self.routine)?;
        for offset in &self.sites {
            write!(f, " 0x{:04X}", offset)?;
        }
        Ok(())
    }
}

impl Display for CallTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (line, (lib, map)) in self.0.iter().enumerate() {
            if line > 0 {
                write!(f, "{:1$}", "", f.width().unwrap_or_default())?;
            }
            writeln!(f, "{}:", lib)?;
            for call_ref in map {
                writeln!(f, "{:2$}- {}", "", call_ref, f.width().unwrap_or_default())?;
            }
        }
        if self.0.is_empty() {
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DataType::ByteStr(Some(default)) => {
                f.write_str("bytes = ")?;
                format_hex(default, f)
            }
            DataType::ByteStr(None) => f.write_str("bytes"),
            DataType::Int(layout, default) if default.is_none() => {
                write!(f, "{}", layout)
            }
            DataType::Float(layout, default) if default.is_none() => {
                write!(f, "{}", layout)
            }
            DataType::Int(layout, default) => {
                write!(f, "{} = {}", layout, default.unwrap())
            }
            DataType::Float(layout, default) => {
                write!(f, "{} = {}", layout, default)
            }
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}; {}", self.data, self.info)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)?;

        f.write_str("VARS:   ")?;
        for (line, v) in self.vars.iter().enumerate() {
            if line > 0 {
                write!(f, "{:8}", "")?;
            }
            writeln!(f, "{}", v)?;
        }
        if self.vars.is_empty() {
            f.write_char('\n')?;
        }

        write!(f, "IMPORT: {:8}", self.imports)?;

        f.write_str("EXPORT: ")?;
        for (line, (routine, offset)) in self.exports.iter().enumerate() {
            if line > 0 {
                write!(f, "{:8}", "")?;
            }
            writeln!(f, "0x{:04X}\t{}", offset, routine)?;
        }
        if self.exports.is_empty() {
            f.write_char('\n')?;
        }

        Ok(())
    }
}

/// TODO: use in decoding (currently unused, had left after refactoring)
#[derive(Clone, Eq, PartialEq, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum ModuleError {
    /// end of data is reached before the complete module read
    ///
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
    ///
    /// details: {0}
    VarNonUtf8(FromUtf8Error),

    /// routine symbol name has a non-UTF8 encoding
    ///
    /// details: {0}
    RoutineNonUtf8(FromUtf8Error),

    /// external call symbol has a non-UTF8 encoding
    ///
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
            DataType::ByteStr(bytestr) => Ok(0xFF_u8.encode(&mut writer)?
                + bytestr.as_ref().map(ByteStr::with).encode(&mut writer)?),
            DataType::Int(layout, default) if default.is_some() => {
                let mut count = layout.encode(&mut writer)?;
                count += 1u8.encode(&mut writer)?;
                let len = layout.bytes as usize;
                count += len;
                writer.write_all(default.unwrap().as_ref())?;
                Ok(count)
            }
            DataType::Int(layout, _) => Ok(layout.encode(&mut writer)? + 1u8.encode(&mut writer)?),
            DataType::Float(layout, default) if default.is_some() => {
                let mut count = layout.encode(&mut writer)?;
                count += 1u8.encode(&mut writer)?;
                let len = layout.bytes() as usize;
                count += len;
                writer.write_all(default.unwrap().as_ref())?;
                Ok(count)
            }
            DataType::Float(layout, _) => {
                Ok(layout.encode(&mut writer)? + 1u8.encode(&mut writer)?)
            }
        }
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
                let inner = match u8::decode(&mut reader)? {
                    0 => None,
                    1 => Some(MaxLenWord::decode(&mut reader)?.release()),
                    unknown => return Err(DecodeError::InvalidBool(unknown)),
                };
                DataType::ByteStr(inner)
            }

            i if i <= 1 => {
                let bytes = u16::decode(&mut reader)?;
                let layout = IntLayout { signed: i == 1, bytes };
                let default = match u8::decode(&mut reader)? {
                    0 => MaybeNumber::none(),
                    1 => {
                        let mut buf = vec![0u8; bytes as usize];
                        reader.read_exact(&mut buf)?;
                        Number::with(buf, layout).into()
                    }
                    unknown => return Err(DecodeError::InvalidBool(unknown)),
                };
                DataType::Int(layout, default)
            }

            f => {
                let layout = FloatLayout::with(f).ok_or(DecodeError::FloatLayout(f))?;
                let default = match u8::decode(&mut reader)? {
                    0 => MaybeNumber::none(),
                    1 => {
                        let mut buf = vec![0u8; layout.bytes() as usize];
                        reader.read_exact(&mut buf)?;
                        Number::with(buf, layout).into()
                    }
                    unknown => return Err(DecodeError::InvalidBool(unknown)),
                };
                DataType::Float(layout, default)
            }
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

impl Encode for Module {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.inner.encode(&mut writer)?
            + self.imports.encode(&mut writer)?
            + MaxLenWord::new(&self.exports).encode(&mut writer)?
            + MaxLenWord::new(&self.vars).encode(&mut writer)?)
    }
}

impl Decode for Module {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(Module {
            inner: Decode::decode(&mut reader)?,
            imports: Decode::decode(&mut reader)?,
            exports: MaxLenWord::decode(&mut reader)?.release(),
            vars: MaxLenWord::decode(&mut reader)?.release(),
        })
    }
}
