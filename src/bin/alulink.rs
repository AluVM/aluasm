// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

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

    /// Output executable binary
    #[clap(long, global = true, conflicts_with = "lib")]
    pub bin: bool,

    /// Output library (default)
    #[clap(long, global = true)]
    pub lib: bool,

    /// Build directory with object files. Defaults to `{build-dir}/objects}` (see --build-dir
    /// argument)
    #[clap(short = 'O', long, global = true, default_value = "${ALU_BUILD_DIR}/objects")]
    pub obj_dir: PathBuf,

    /// Directory containing library files. Defaults to `{build-dir}/libs}` (see --build-dir
    /// argument)
    #[clap(short = 'L', long, global = true, default_value = "${ALU_BUILD_DIR}/libs")]
    pub lib_dir: PathBuf,

    /// Adds specific library
    #[clap(short = 'l', global = true)]
    pub libs: Vec<PathBuf>,

    /// Path to a directory used for building process
    #[clap(short = 'B', long, global = true, default_value = "./build/")]
    pub build_dir: PathBuf,

    /// Object file to link
    #[clap(required = true)]
    pub file: PathBuf,
}

fn main() {
    let args = Args::parse();
    link(&args).unwrap_or_else(|err| {
        eprintln!("{}", err);
        exit(1)
    });
    eprintln!("\x1B[1;32m Finished\x1B[0m successfully");
}

fn link(args: &Args) -> Result<(), MainError> {
    let product_name = args
        .file
        .file_name()
        .ok_or(BuildError::NotFile(args.file.to_string_lossy().to_string()))?
        .to_string_lossy()
        .to_string();
    eprintln!("\x1B[1;32m  Linking\x1B[0m {}", product_name,);

    let module = read_object(&args.file, &args)?;

    let (product, issues) = if args.bin { module.link_bin()? } else { module.link_lib()? };

    if issues.has_errors() {
        return Err(MainError::Linking(
            product_name,
            issues.count_errors(),
            issues.count_warnings(),
            issues.to_string(),
        ));
    }
    eprintln!("{}", issues);

    Ok(())
}

fn read_all_objects(args: &Args) -> Result<Vec<Module>, MainError> {
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
        vec.push(read_object(&path, &args)?);
    }

    Ok(vec)
}

fn read_object(path: &PathBuf, _args: &Args) -> Result<Module, MainError> {
    let file_name = path
        .file_name()
        .ok_or(BuildError::NotFile(path.to_string_lossy().to_string()))?
        .to_string_lossy()
        .to_string();

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
