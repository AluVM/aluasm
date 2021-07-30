// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::path::{Path, PathBuf};

use aluvm::data::ByteStr;
use aluvm::isa::{ControlFlowOp, Instr};
use aluvm::libs::{Cursor, LibId, Write};

use crate::issues::{self, Issues, LinkingError, LinkingWarning};
use crate::module::Module;
use crate::product::{DyBin, DyInner, DyLib, EntryPoint, Product};
use crate::{InstrError, LinkerError};

impl Module {
    pub fn link_bin(
        &self,
        name: String,
        org: String,
        lib_man: &mut LibManager,
    ) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        let entry_point = self.exports.get(".MAIN").copied().unwrap_or_else(|| {
            issues.push_error_nospan(LinkingError::BinaryNoMain);
            0
        });

        let product =
            self.link(name, org, EntryPoint::BinMain(entry_point), lib_man, &mut issues)?;
        Ok((product, issues))
    }

    pub fn link_lib(
        &self,
        name: String,
        org: String,
        lib_man: &mut LibManager,
    ) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        if self.exports.get(".MAIN").is_some() {
            issues.push_warning_nospan(LinkingWarning::LibraryWithMain);
        }

        let product =
            self.link(name, org, EntryPoint::LibTable(self.exports.clone()), lib_man, &mut issues)?;
        Ok((product, issues))
    }

    fn link(
        &self,
        name: String,
        org: String,
        entry_point: EntryPoint,
        lib_man: &mut LibManager,
        issues: &mut Issues<issues::Linking>,
    ) -> Result<Product, LinkerError> {
        let isae = self.isae.clone();
        let code = ByteStr::try_from(self.code.borrow())
            .map_err(|err| LinkerError::CodeSegmentOversized(err.value))?;
        let data = ByteStr::try_from(self.data.borrow())
            .map_err(|err| LinkerError::DataSegmentOversized(err.value))?;
        let libs = self.libs.clone();
        let vars = self.vars.clone();

        for (libid, routine, map) in self.imports.call_refs() {
            let lib = match lib_man.get(libid) {
                Some(lib) => lib,
                None => {
                    issues.push_error_nospan(LinkingError::LibraryAbsent(libid, name.clone()));
                    continue;
                }
            };

            let pos = match lib.exports.get(routine) {
                Some(pos) => *pos,
                None => {
                    issues.push_error_nospan(LinkingError::LibraryNoRoutine {
                        libid,
                        routine: routine.to_owned(),
                        module: name.clone(),
                    });
                    continue;
                }
            };

            let mut code = self.code.clone();
            let mut cursor = Cursor::with(&mut code, self.data.clone(), &self.libs);
            for p in map {
                cursor
                    .edit(*p, |instr| match instr {
                        Instr::ControlFlow(
                            ControlFlowOp::Call(ref mut site) | ControlFlowOp::Exec(ref mut site),
                        ) => {
                            site.pos = pos;
                            Ok(())
                        }
                        _ => Err(InstrError::Changed("call", instr.clone())),
                    })
                    .map_err(|err| LinkerError::with(err, pos))?;
            }
        }

        let inner = DyInner { name, org, isae, code, data, libs, vars };
        Ok(match entry_point {
            EntryPoint::LibTable(exports) => Product::Lib(DyLib { inner, exports }),
            EntryPoint::BinMain(entry_point) => Product::Bin(DyBin { inner, entry_point }),
        })
    }
}

#[derive(Debug)]
pub struct LibManager {
    map: BTreeMap<LibId, PathBuf>,
    cache: BTreeMap<LibId, DyLib>,
}

impl LibManager {
    pub fn new(path: impl AsRef<Path>) -> Self {
        // TODO:
        LibManager { map: bmap! {}, cache: bmap! {} }
    }

    pub fn get(&mut self, id: LibId) -> Option<&DyLib> {
        if let Some(lib) = self.cache.get(&id) {
            return Some(lib);
        }
        // TODO:
        None
    }
}
