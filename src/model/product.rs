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

impl Encode for EntryPoint {
    type Error = EncodeError;

    fn encode(&self, writer: impl Write) -> Result<usize, Self::Error> {
        match self {
            EntryPoint::BinMain(entry) => entry.encode(writer).map_err(EncodeError::from),
            EntryPoint::LibTable(table) => MaxLenWord::new(table).encode(writer),
        }
    }
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
pub struct Product {
    pub name: String,
    pub org: String,
    pub isae: String,
    pub code: ByteStr,
    pub data: ByteStr,
    pub libs: LibSeg,
    pub vars: Vec<Variable>,
    pub entry_point: EntryPoint,
}

impl Display for Product {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "NAME: {}@{}", self.name, self.org)?;
        f.write_str("ISAE: ")?;
        f.write_str(&self.isae)?;
        f.write_str("\nCODE: \n")?;
        write!(f, "{:#}", self.code)?;
        f.write_str("\nDATA: \n")?;
        write!(f, "{:#}", self.data)?;
        f.write_str("\nLIBS: ")?;
        Display::fmt(&self.libs, f)?;
        // TODO: Print vars information
        f.write_str("\nENTRY: ")?;
        Display::fmt(&self.entry_point, f)
    }
}

impl Encode for Product {
    type Error = EncodeError;

    fn encode(&self, mut writer: impl Write) -> Result<usize, Self::Error> {
        Ok(self.name.encode(&mut writer)?
            + self.org.encode(&mut writer)?
            + self.isae.encode(&mut writer)?
            + self.code.encode(&mut writer)?
            + self.data.encode(&mut writer)?
            + self.libs.encode(&mut writer)?
            + MaxLenWord::new(&self.vars).encode(&mut writer)?
            + self.entry_point.encode(&mut writer)?)
    }
}
