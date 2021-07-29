// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::borrow::Borrow;
use std::convert::TryFrom;

use aluvm::data::ByteStr;

use crate::issues::{self, Issues, LinkingError, LinkingWarning};
use crate::module::Module;
use crate::product::{EntryPoint, Product};
use crate::LinkerError;

impl Module {
    pub fn link_bin(&self) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        let entry_point = self.exports.get(".MAIN").copied().unwrap_or_else(|| {
            issues.push_error_nospan(LinkingError::BinaryNoMain);
            0
        });

        let product = self.link(EntryPoint::BinMain(entry_point), &mut issues)?;
        Ok((product, issues))
    }

    pub fn link_lib(&self) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        if self.exports.get(".MAIN").is_some() {
            issues.push_warning_nospan(LinkingWarning::LibraryWithMain);
        }

        // TODO: Generate table;
        let mut table = bmap! {};

        let product = self.link(EntryPoint::LibTable(table), &mut issues)?;
        Ok((product, issues))
    }

    fn link(
        &self,
        entry_point: EntryPoint,
        issues: &mut Issues<issues::Linking>,
    ) -> Result<Product, LinkerError> {
        let isae = self.isae.clone();
        let code = ByteStr::try_from(self.code.borrow())
            .map_err(|err| LinkerError::CodeSegmentOversized(err.value))?;
        let data = ByteStr::try_from(self.data.borrow())
            .map_err(|err| LinkerError::DataSegmentOversized(err.value))?;
        let libs = self.libs.clone();
        let vars = self.vars.clone();

        Ok(Product { isae, code, data, libs, vars, entry_point })
    }
}
