// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::process::exit;

use aluasm::module::Module;
use aluasm::{BuildError, MainError};
use clap::{AppSettings, Clap};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Clap)]
#[clap(
    name = "alulink",
    bin_name = "alulink",
    author,
    version,
    about,
    setting = AppSettings::ColoredHelp
)]
pub struct Args {
    /// Set verbosity level
    ///
    /// Can be used multiple times to increase verbosity
    #[clap(short, long, global = true, parse(from_occurrences))]
    pub verbose: u8,

    /// Build directory with object files
    #[clap(short = 'O', long, global = true, default_value = "./build/objects")]
    pub obj_dir: PathBuf,
}

fn main() {
    let args = Args::parse();
    match read_all_objects(args) {
        Ok(_) => exit(0),
        Err(err) => {
            eprintln!("{}", err);
            exit(1)
        }
    }
}

fn read_all_objects(args: Args) -> Result<Vec<Module>, MainError> {
    let obj_dir = args.obj_dir.to_string_lossy().to_string();
    if args.obj_dir.is_file() {
        Err(BuildError::ObjDirIsFile(obj_dir.clone()))?;
    }

    let mut vec = vec![];
    for entry in fs::read_dir(&args.obj_dir)
        .map_err(|err| BuildError::ObjDirFail(obj_dir.clone(), err.into()))?
    {
        let path = entry.map_err(|err| BuildError::ObjDirFail(obj_dir.clone(), err.into()))?.path();
        if path.is_dir() {
            continue;
        }
        vec.push(read_object(path, &args)?);
    }

    Ok(vec)
}

fn read_object(path: PathBuf, _args: &Args) -> Result<Module, MainError> {
    let file_name =
        path.file_name().unwrap_or(OsStr::new("<noname>")).to_string_lossy().to_string();

    eprintln!(
        "\x1B[1;32m  Loading\x1B[0m {} ({})",
        file_name,
        path.canonicalize().unwrap_or_default().display()
    );

    let fd = File::open(path).map_err(|err| BuildError::FileNotFound {
        file: file_name.clone(),
        details: Box::new(err),
    })?;

    Module::read(fd).map_err(|err| MainError::Module(err, file_name))
}
