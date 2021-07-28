// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::BTreeMap;
use std::io::{self, Write};

use aluvm::data::{FloatLayout, IntLayout, MaybeNumber};
use aluvm::libs::LibSeg;
use amplify::Wrapper;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Symbols {
    /// External routine names
    pub externals: Vec<String>,
    /// Map of local routine names to code offsets
    pub routines: BTreeMap<String, u16>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum DataType {
    String(String),
    Int(IntLayout, MaybeNumber),
    Float(FloatLayout, MaybeNumber),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Input {
    pub name: String,
    pub details: String,
    pub data: DataType,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Module {
    pub isae: String,
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    pub libs: LibSeg,
    pub input: Vec<Input>,
    pub symbols: Symbols,
}

impl Module {
    pub fn write(&self, writer: &mut impl Write) -> io::Result<()> {
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
