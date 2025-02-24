//! Build the man pages for `empress`

use std::{
    path::{Path, PathBuf},
    sync::LazyLock,
};

use clap::{CommandFactory, Parser};
use clap_mangen::Man;

mod opts {
    #![expect(warnings, clippy::all)]

    include!("../../../src/opts.rs");

    impl std::str::FromStr for Offset {
        type Err = std::convert::Infallible;

        fn from_str(_: &str) -> Result<Self, Self::Err> { unreachable!() }
    }
}

#[derive(Parser)]
struct Opts {
    /// Output gzip-compressed man pages
    #[arg(short = 'z', long)]
    gzip: bool,
}

static DIR: LazyLock<PathBuf> = LazyLock::new(|| "man".into());

fn render_page(
    cmd: clap::Command,
    section: &str,
    include: impl IntoIterator<Item: AsRef<Path>>,
    mut out: impl std::io::Write,
) -> std::io::Result<()> {
    let has_arguments = cmd.get_arguments().any(|a| !a.is_hide_set());
    let has_subcommands = cmd.get_subcommands().any(|a| !a.is_hide_set());
    let has_extra_section = cmd.get_after_long_help().is_some() || cmd.get_after_help().is_some();
    let has_version = cmd.get_version().is_some() || cmd.get_long_version().is_some();
    let has_author = cmd.get_author().is_some();
    let man = Man::new(cmd).section(section);

    // TODO: i would like to use a single roff write for this at some point
    man.render_title(&mut out)?;
    man.render_name_section(&mut out)?;
    man.render_synopsis_section(&mut out)?;
    man.render_description_section(&mut out)?;

    if has_arguments {
        man.render_options_section(&mut out)?;
    }

    if has_subcommands {
        man.render_subcommands_section(&mut out)?;
    }

    if has_extra_section {
        man.render_extra_section(&mut out)?;
    }

    for extra in include {
        std::io::copy(&mut std::fs::File::open(extra)?, &mut out)?;
    }

    if has_version {
        man.render_version_section(&mut out)?;
    }

    if has_author {
        man.render_authors_section(&mut out)?;
    }

    Ok(())
}

fn render(
    opts: &Opts,
    cmd: clap::Command,
    name: impl IntoIterator<Item: AsRef<str>> + Clone,
    include: impl IntoIterator<Item: AsRef<Path>>,
) {
    const SECTION: &str = "1";
    let name = name
        .into_iter()
        .map(|s| s.as_ref().to_owned())
        .collect::<Vec<_>>();
    let disp = name.join("-");
    let name = name.join(" ");

    let dir = DIR.join(format!("man{SECTION}"));
    std::fs::create_dir_all(&dir).unwrap();

    let path = dir.join(format!(
        "{disp}.{SECTION}{}",
        if opts.gzip { ".gz" } else { "" }
    ));
    let mut out = std::fs::File::create_new(&path).unwrap();
    let cmd = cmd.name(name).display_name(disp);

    if opts.gzip {
        let mut out = flate2::write::GzEncoder::new(out, flate2::Compression::best());
        render_page(cmd, SECTION, include, &mut out).unwrap();
        out.finish().unwrap();
    } else {
        render_page(cmd, SECTION, include, &mut out).unwrap();
    }

    println!("Rendered {}", path.display());
}

fn main() {
    let opts = Opts::parse();

    // Back out of crates/xtask-build-man into the workspace root
    std::env::set_current_dir(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join(".."),
    )
    .unwrap();

    std::fs::create_dir_all(&*DIR).unwrap();

    for entry in std::fs::read_dir(&*DIR).unwrap() {
        let entry = entry.unwrap();
        if entry.path().is_dir() {
            std::fs::remove_dir_all(entry.path()).unwrap();
        } else {
            std::fs::remove_file(entry.path()).unwrap();
        }
    }

    let cmd = opts::Opts::command().disable_help_subcommand(true);
    let ver = cmd.get_version().unwrap().to_owned();
    let author = cmd.get_author().unwrap().to_owned();

    for cmd in cmd.get_subcommands().filter(|c| !c.is_hide_set()) {
        let id = cmd.get_name();
        let cmd = cmd.clone().version(&ver).author(&author);
        let name = ["empress", id];

        match id {
            "now-playing" => render(&opts, cmd, name, ["etc/now-playing-syntax.man"]),
            _ => render(&opts, cmd, name, None::<&Path>),
        };
    }

    render(&opts, cmd, ["empress"], None::<&Path>);
}
