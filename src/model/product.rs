// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter};
use std::io::Write;

use aluvm::data::encoding::{Encode, EncodeError, MaxLenWord};
use aluvm::data::ByteStr;
use aluvm::libs::LibSeg;

use crate::module::Variable;

// TODO: Move shared part into AluVM structure (`Lib`)

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
    pub isae: String,
    pub code: ByteStr,
    pub data: ByteStr,
    pub libs: LibSeg,
    pub vars: Vec<Variable>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct DyLib {
    pub(crate) inner: DyInner,
    pub exports: BTreeMap<String, u16>,
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

impl Display for DyInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "NAME: {}@{}", self.name, self.org)?;
        f.write_str("ISAE: ")?;
        f.write_str(&self.isae)?;
        f.write_str("\nCODE: \n")?;
        write!(f, "{:#}", self.code)?;
        f.write_str("\nDATA: \n")?;
        write!(f, "{:#}", self.data)?;
        f.write_str("\nLIBS: ")?;
        Display::fmt(&self.libs, f)
        // TODO: Print vars information
    }
}

impl Display for DyLib {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)?;
        f.write_str("\nEXPORTS: ")?;
        for (routine, offset) in &self.exports {
            writeln!(f, "\n{:04X} => {}", offset, routine)?;
        }
        Ok(())
    }
}

impl Display for DyBin {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("\nENTRY: ")?;
        writeln!(f, "0x{:04X}", self.entry_point)
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

impl Encode for DyInner {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.name.encode(&mut writer)?
            + self.org.encode(&mut writer)?
            + self.isae.encode(&mut writer)?
            + self.code.encode(&mut writer)?
            + self.data.encode(&mut writer)?
            + self.libs.encode(&mut writer)?
            + MaxLenWord::new(&self.vars).encode(&mut writer)?)
    }
}

impl Encode for DyLib {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.inner.encode(&mut writer)? + MaxLenWord::new(&self.exports).encode(&mut writer)?)
    }
}

impl Encode for DyBin {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.inner.encode(&mut writer)? + self.entry_point.encode(&mut writer)?)
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
