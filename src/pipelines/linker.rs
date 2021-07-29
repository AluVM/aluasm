// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use crate::issues::{self, Issues, LinkingError, LinkingWarning};
use crate::module::Module;
use crate::product::{Entry, Product};
use crate::LinkerError;

impl Module {
    pub fn link_bin(&self) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        let entry_point = self.symbols.routines.get(".MAIN").copied().unwrap_or_else(|| {
            issues.push_error_nospan(LinkingError::BinaryNoMain);
            0
        });

        let product = self.link(Entry::BinMain(entry_point), &mut issues)?;
        Ok((product, issues))
    }

    pub fn link_lib(&self) -> Result<(Product, Issues<issues::Linking>), LinkerError> {
        let mut issues = Issues::default();

        if self.symbols.routines.get(".MAIN").is_some() {
            issues.push_warning_nospan(LinkingWarning::LibraryWithMain);
        }

        // TODO: Generate table;
        let mut table = bmap! {};

        let product = self.link(Entry::LibTable(table), &mut issues)?;
        Ok((product, issues))
    }

    fn link(
        &self,
        entry: Entry,
        issues: &mut Issues<issues::Linking>,
    ) -> Result<Product, LinkerError> {
        let isae = Default::default();
        let code = Default::default();
        let data = Default::default();
        let libs = Default::default();
        let vars = Default::default();

        let dylib = Product { isae, code, data, libs, vars, entry };

        Ok(dylib)
    }
}
