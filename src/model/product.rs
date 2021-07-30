// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter, Write as WriteTrait};
use std::io::{self, Read, Write};

use aluvm::data::encoding::{Decode, DecodeError, Encode, EncodeError, MaxLenWord};
use aluvm::libs::{Lib, LibId};

use crate::module::Variable;

pub const MAGIC_DYLIB: [u8; 10] = *b"ALU dyLib\0";
pub const MAGIC_DYBIN: [u8; 10] = *b"ALU dyBin\0";

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum EntryPoint {
    LibTable(BTreeMap<String, u16>),
    BinMain(u16),
}

impl Display for EntryPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EntryPoint::LibTable(table) => {
                for (routine, offset) in table {
                    writeln!(f, "\n{:04X} => {}", offset, routine)?;
                }
            }
            EntryPoint::BinMain(entry) => writeln!(f, "0x{:04X}", entry)?,
        }
        Ok(())
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct DyInner {
    pub name: String,
    pub org: String,
    pub(crate) inner: Lib,
    pub vars: Vec<Variable>,
}

impl DyInner {
    #[inline]
    pub fn lib_id(&self) -> LibId { self.inner.id() }

    #[inline]
    pub fn as_static_lib(&self) -> &Lib { &self.inner }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct DyLib {
    pub(crate) inner: DyInner,
    pub exports: BTreeMap<String, u16>,
}

impl DyLib {
    #[inline]
    pub fn lib_id(&self) -> LibId { self.inner.lib_id() }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct DyBin {
    pub(crate) inner: DyInner,
    pub entry_point: u16,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Product {
    Lib(DyLib),
    Bin(DyBin),
}

impl Product {
    pub const BIN_EXTENSION: &'static str = "rex";
    pub const LIB_EXTENSION: &'static str = "ald";

    pub fn file_extension(&self) -> &'static str {
        match self {
            Product::Lib(_) => Self::LIB_EXTENSION,
            Product::Bin(_) => Self::BIN_EXTENSION,
        }
    }
}

impl Display for DyInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "NAME:   {}@{}", self.name, self.org)?;
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
        Ok(())
    }
}

impl Display for DyLib {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)?;
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

impl Display for DyBin {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)?;
        writeln!(f, "ENTRY:  0x{:04X}", self.entry_point)
    }
}

impl Display for Product {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Product::Lib(lib) => Display::fmt(lib, f),
            Product::Bin(bin) => Display::fmt(bin, f),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum DyError {
    #[from]
    #[display(inner)]
    Decode(DecodeError),

    /// wrong magic bytes in file header (expected `{expected}`, got `{found}`)
    WrongMagic { expected: String, found: String },

    /// library {library} has incorrect data not matching its id
    WrongLibId { library: LibId, found: LibId },
}

impl From<io::Error> for DyError {
    #[inline]
    fn from(err: io::Error) -> Self { DecodeError::from(err).into() }
}

impl Encode for DyInner {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.name.encode(&mut writer)?
            + self.org.encode(&mut writer)?
            + self.inner.encode(&mut writer)?
            + MaxLenWord::new(&self.vars).encode(&mut writer)?)
    }
}

impl Decode for DyInner {
    type Error = DecodeError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(DyInner {
            name: Decode::decode(&mut reader)?,
            org: Decode::decode(&mut reader)?,
            inner: Decode::decode(&mut reader)?,
            vars: MaxLenWord::decode(&mut reader)?.release(),
        })
    }
}

impl Encode for DyLib {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        writer.write(&MAGIC_DYLIB)?;
        Ok(10
            + self.lib_id().encode(&mut writer)?
            + self.inner.encode(&mut writer)?
            + MaxLenWord::new(&self.exports).encode(&mut writer)?)
    }
}

impl Decode for DyLib {
    type Error = DyError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let id = DyLib::decode_id(&mut reader)?;
        let inner = DyInner::decode(&mut reader)?;
        let inner_id = inner.lib_id();
        if inner_id != id {
            return Err(DyError::WrongLibId { library: id, found: inner_id });
        }
        Ok(DyLib { inner, exports: MaxLenWord::decode(&mut reader)?.release() })
    }
}

impl DyLib {
    pub fn decode_id(mut reader: impl Read) -> Result<LibId, DyError> {
        let mut magic = [0u8; 10];
        reader.read_exact(&mut magic)?;
        if magic != MAGIC_DYLIB {
            return Err(DyError::WrongMagic {
                expected: String::from_utf8(MAGIC_DYLIB.to_vec()).unwrap_or_default(),
                found: String::from_utf8(magic[..9].to_owned()).unwrap_or_default(),
            });
        }
        LibId::decode(&mut reader).map_err(DyError::from)
    }
}

impl Encode for DyBin {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        writer.write(&MAGIC_DYBIN)?;
        Ok(10 + self.inner.encode(&mut writer)? + self.entry_point.encode(&mut writer)?)
    }
}

impl Decode for DyBin {
    type Error = DyError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let mut magic = [0u8; 10];
        reader.read_exact(&mut magic)?;
        if magic != MAGIC_DYBIN {
            return Err(DyError::WrongMagic {
                expected: String::from_utf8(MAGIC_DYBIN.to_vec()).unwrap_or_default(),
                found: String::from_utf8(magic[..9].to_owned()).unwrap_or_default(),
            });
        }
        Ok(DyBin { inner: Decode::decode(&mut reader)?, entry_point: Decode::decode(&mut reader)? })
    }
}

impl Encode for Product {
    type Error = EncodeError;

    fn encode(&self, writer: impl Write) -> Result<usize, Self::Error> {
        match self {
            Product::Lib(lib) => lib.encode(writer),
            Product::Bin(bin) => bin.encode(writer),
        }
    }
}

impl Decode for Product {
    type Error = DyError;

    fn decode(mut reader: impl Read) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let mut magic = [0u8; 10];
        reader.read_exact(&mut magic)?;
        match magic {
            bin if bin == MAGIC_DYBIN => Ok(Product::Bin(DyBin {
                inner: Decode::decode(&mut reader)?,
                entry_point: Decode::decode(&mut reader)?,
            })),
            lib if lib == MAGIC_DYLIB => Ok(Product::Lib(DyLib {
                inner: Decode::decode(&mut reader)?,
                exports: MaxLenWord::decode(&mut reader)?.release(),
            })),
            unknown => Err(DyError::WrongMagic {
                expected: String::from_utf8(MAGIC_DYBIN.to_vec()).unwrap_or_default(),
                found: String::from_utf8(unknown[..9].to_owned()).unwrap_or_default(),
            }),
        }
    }
}
