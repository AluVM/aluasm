// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::exit;

use aluasm::linker::LibManager;
use aluasm::module::Module;
use aluasm::product::Product;
use aluasm::{BuildError, MainError};
use aluvm::data::encoding::{Decode, Encode};
use clap::{AppSettings, Clap};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Clap)]
#[clap(
    name = "alink",
    bin_name = "alink",
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

    /// Build directory with object files. Defaults to `{build-dir}/objects` (see --build-dir
    /// argument)
    #[clap(short = 'O', long, global = true, default_value = "${ALU_BUILD_DIR}/objects")]
    pub obj_dir: PathBuf,

    /// Directories containing library files (--build-dir included in the list automatically)
    #[clap(short = 'L', long = "lib-dir", global = true)]
    pub lib_dirs: Vec<PathBuf>,

    /// Adds specific library
    #[clap(short = 'l', global = true)]
    pub libs: Vec<PathBuf>,

    /// Directory to put linked binary files (products). Defaults to --build-dir
    #[clap(short = 'T', long, global = true, default_value = "${ALU_BUILD_DIR}")]
    pub target_dir: PathBuf,

    /// Path to a directory used for building process
    #[clap(short = 'B', long, global = true, default_value = "./build/")]
    pub build_dir: PathBuf,

    /// Name of the product file to generate (without extension). Defaults to the same name as the
    /// object file; required if multiple object files are used.
    #[clap(short = 'n', long)]
    pub product_name: Option<String>,

    /// Organization name to use for the product generation; should use domain name notation
    #[clap(short, long = "org")]
    pub org_name: String,

    /// Object file name to link. The object file is looked up at --obj-dir path and extension is
    /// automatically added if necessary.
    #[clap(required = true)]
    pub file: String,
}

impl Args {
    pub fn processed(mut self) -> Self {
        let build_dir = self.build_dir.clone();
        Args::processed_path(&mut self.obj_dir, &build_dir);
        Args::processed_path(&mut self.target_dir, &build_dir);
        self
    }

    fn processed_path(path: &mut PathBuf, build_dir: &PathBuf) {
        *path = path.iter().fold(PathBuf::new(), |mut path, comp| {
            if comp == "${ALU_BUILD_DIR}" {
                path.push(build_dir);
            } else {
                path.push(comp);
            }
            path
        });
    }
}

fn main() {
    let args: Args = Args::parse().processed();

    link(&args).unwrap_or_else(|err| {
        eprintln!("{}\n", err);
        exit(1)
    });
    eprintln!("\x1B[1;32m Finished\x1B[0m successfully\n");
}

fn link(args: &Args) -> Result<(), MainError> {
    fs::create_dir_all(&args.target_dir).map_err(|err| BuildError::OutputDir {
        dir: args.target_dir.to_string_lossy().to_string(),
        details: Box::new(err),
    })?;

    let product_name = args.product_name.as_ref().cloned().unwrap_or(args.file.to_owned());
    let org_name = args.org_name.clone();
    eprintln!("\x1B[1;32m  Linking\x1B[0m {}\x1B[1;34m@{}\x1B[0m", product_name, org_name);

    let mut path = args.obj_dir.clone();
    path.push(&args.file);
    path.set_extension("ao");
    let module = read_object(&path, &args)?;

    let mut libs = enumerate_libs(&args.target_dir)?;
    for path in &args.lib_dirs {
        libs.extend(enumerate_libs(path)?);
    }
    libs.extend(args.libs.iter().cloned());
    let mut manager = LibManager::with(libs)?;

    let (product, issues) = if args.bin {
        module.link_bin(product_name.clone(), org_name.clone(), &mut manager)?
    } else {
        module.link_lib(product_name.clone(), org_name.clone(), &mut manager)?
    };

    if issues.has_errors() {
        return Err(MainError::Linking(
            product_name,
            issues.count_errors(),
            issues.count_warnings(),
            issues.to_string(),
        ));
    }
    eprint!("{}", issues);

    if args.verbose >= 2 {
        eprintln!("\x1B[0;35m Printing\x1B[0m product dump:");
        println!("{}", product);
    }

    if let Product::Lib(ref lib) = product {
        eprintln!("\x1B[1;32m   Lib ID\x1B[0m: \x1B[1;34m{}\x1B[0m", lib.lib_id());
    }

    let mut target_path = args.target_dir.clone();
    target_path.push(format!("{}@{}", product_name, org_name));
    target_path.set_extension(product.file_extension());
    let target_name = target_path.to_string_lossy().to_string();
    let file = File::create(&target_path).map_err(|err| BuildError::ProductFileCreation {
        file: target_name.clone(),
        details: Box::new(err),
    })?;
    eprintln!("\x1B[1;32m   Saving\x1B[0m {} to `{}`", product.name(), target_path.display());
    product.encode(file).map_err(|err| BuildError::ProductFileWrite {
        file: target_name,
        details: Box::new(err),
    })?;

    Ok(())
}

fn enumerate_libs(lib_dir: impl AsRef<Path>) -> Result<Vec<PathBuf>, BuildError> {
    let lib_dir = lib_dir.as_ref();
    let lib_dir_name = lib_dir.to_string_lossy().to_string();
    if lib_dir.is_file() {
        return Err(BuildError::LibDirIsFile(lib_dir_name));
    }

    let mut vec = vec![];
    for entry in fs::read_dir(&lib_dir)
        .map_err(|err| BuildError::ObjDirFail(lib_dir_name.clone(), err.into()))?
    {
        let path =
            entry.map_err(|err| BuildError::LibDirFail(lib_dir_name.clone(), err.into()))?.path();
        if path.is_dir() {
            continue;
        }
        if path.extension().unwrap_or_default().to_string_lossy() != Product::LIB_EXTENSION {
            continue;
        }
        vec.push(path);
    }
    Ok(vec)
}

// TODO: Reserved for the future where linked will be able to use multiple modules for single
//       product
#[allow(dead_code)]
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

    let module = Module::decode(fd).map_err(|err| MainError::Module(err, file_name.clone()))?;

    Ok(module)
}
