#![deny(clippy::pedantic, clippy::expect_used, clippy::unwrap_used)]

use std::{fs, path::PathBuf};

use color_eyre::eyre::{Result, WrapErr};
use fuzzy_matcher::{FuzzyMatcher, skim::SkimMatcherV2};
use hashbrown::HashSet;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag_no_case, take_till1, take_until, take_until1},
    character::complete::{char, digit1, space0, space1},
    combinator::{eof, rest},
    error::Error,
    multi::many0,
    sequence::{delimited, preceded, terminated},
};
use rayon::prelude::*;
use serde::{Serialize, Serializer};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Alfred script filter output: <https://www.alfredapp.com/help/workflows/inputs/script-filter/json/>
///
/// No `cache` block: Alfred would serve cached items instead of re-running
/// the filter, bypassing skim matching. The binary is fast enough to run
/// on every keystroke.
#[derive(Serialize)]
struct ScriptFilter<'a> {
    items: Vec<Item<'a>>,
}

#[derive(Serialize)]
struct Item<'a> {
    uid: &'a str,
    title: &'a str,
    subtitle: Subtitle<'a>,
    arg: &'a str,
}

/// Serializes as `"ssh to <host>"` without building an intermediate String.
struct Subtitle<'a>(&'a str);

impl Serialize for Subtitle<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.collect_str(&format_args!("ssh to {}", self.0))
    }
}

/// Hosts and canonical domains collected from one ssh config file.
#[derive(Default)]
struct ConfigHosts<'a> {
    hosts: Vec<&'a [u8]>,
    canonical_domains: Vec<&'a [u8]>,
}

/// Matches a directive of the form `<name> <args...>` (case-insensitive)
/// applied to a single extracted line, returning the argument text.
fn directive<'a>(
    name: &'static str,
) -> impl Parser<&'a [u8], Output = &'a [u8], Error = Error<&'a [u8]>> {
    preceded((space0, tag_no_case(name), space1), rest)
}

/// Consumes one line, not including the newline. `take_until` finds the
/// newline via memchr (SIMD), unlike `not_line_ending`'s per-byte scan.
fn take_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((terminated(take_until("\n"), char('\n')), rest)).parse(input)
}

/// Matches a `known_hosts` `[host]:port` entry, returning the bare host.
fn bracketed_host(input: &[u8]) -> IResult<&[u8], &[u8]> {
    terminated(
        delimited(char('['), take_until1("]"), char(']')),
        (char(':'), digit1, eof),
    )
    .parse(input)
}

/// True for entries made only of digits, dots, and colons (IPv4/IPv6).
fn is_address(entry: &[u8]) -> bool {
    entry
        .iter()
        .all(|&c| c.is_ascii_digit() || c == b'.' || c == b':')
}

/// One config argument: double-quoted (ssh quoting has no escapes) or bare.
fn config_arg(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        delimited(char('"'), take_until("\""), char('"')),
        take_till1(|c: u8| c.is_ascii_whitespace()),
    ))
    .parse(input)
}

fn split_args(args: &[u8]) -> Vec<&[u8]> {
    many0(preceded(space0, config_arg))
        .parse(args)
        .map(|(_, parsed)| parsed)
        .unwrap_or_default()
        .into_iter()
        .filter(|arg| !arg.is_empty())
        .collect()
}

fn parse_config(content: &[u8]) -> ConfigHosts<'_> {
    let mut parsed = ConfigHosts::default();
    let mut input = content;
    while !input.is_empty() {
        let Ok((remaining, line)) = take_line(input) else {
            break;
        };
        input = remaining;
        if let Ok((_, args)) = directive("Host").parse(line) {
            let hosts = split_args(args)
                .into_iter()
                .filter(|host| !host.iter().any(|&c| c == b'*' || c == b'?'));
            parsed.hosts.extend(hosts);
        } else if let Ok((_, args)) = directive("CanonicalDomains").parse(line) {
            parsed.canonical_domains.extend(split_args(args));
        }
    }
    parsed
}

fn parse_known_hosts(content: &[u8]) -> Vec<&[u8]> {
    let mut hosts = Vec::new();
    let mut input = content;
    while !input.is_empty() {
        let Ok((remaining, line)) = take_line(input) else {
            break;
        };
        input = remaining;
        let line = line.trim_ascii_start();
        // Skip comments, blanks, and hashed entries.
        if line.is_empty() || line[0] == b'#' || line[0] == b'|' {
            continue;
        }
        let Some(host_field) = line.split(|b: &u8| b.is_ascii_whitespace()).next() else {
            continue;
        };
        for entry in host_field.split(|&b| b == b',') {
            let entry = match bracketed_host(entry) {
                Ok((_, host)) => host,
                Err(_) => entry,
            };
            if entry.is_empty() || is_address(entry) {
                continue;
            }
            hosts.push(entry);
        }
    }
    hosts
}

fn glob_files(pattern: &str) -> Vec<PathBuf> {
    glob::glob(pattern)
        .map(|paths| paths.filter_map(Result::ok).collect())
        .unwrap_or_default()
}

/// Reads every file, skipping unreadable ones.
fn read_files(paths: &[PathBuf]) -> Vec<Vec<u8>> {
    paths
        .par_iter()
        .filter_map(|path| fs::read(path).ok())
        .collect()
}

fn collect_hosts<'a>(
    config_contents: &'a [Vec<u8>],
    known_contents: &'a [Vec<u8>],
) -> Vec<&'a [u8]> {
    let (configs, known): (Vec<ConfigHosts>, Vec<Vec<&[u8]>>) = rayon::join(
        || {
            config_contents
                .par_iter()
                .map(|c| parse_config(c))
                .collect()
        },
        || {
            known_contents
                .par_iter()
                .map(|c| parse_known_hosts(c))
                .collect()
        },
    );

    // Dedupe, preserving order: config hosts first, then known_hosts.
    let mut seen = HashSet::new();
    let mut hosts: Vec<&[u8]> = Vec::new();
    for &host in configs
        .iter()
        .flat_map(|c| &c.hosts)
        .chain(known.iter().flatten())
    {
        if seen.insert(host) {
            hosts.push(host);
        }
    }

    let suffixes: Vec<Vec<u8>> = configs
        .iter()
        .flat_map(|c| &c.canonical_domains)
        .map(|domain| {
            let mut suffix = Vec::with_capacity(domain.len() + 1);
            suffix.push(b'.');
            suffix.extend_from_slice(domain);
            suffix
        })
        .collect();

    // Strip canonical domain suffixes, preferring short names.
    let mut result = Vec::new();
    for &host in &hosts {
        let short = suffixes.iter().find_map(|suffix| {
            host.strip_suffix(suffix.as_slice())
                .filter(|short| !short.is_empty())
        });
        match short {
            Some(short) => {
                if seen.insert(short) {
                    result.push(short);
                }
            }
            None => result.push(host),
        }
    }
    result
}

/// Fuzzy-filter hosts against a query using skim's matching algorithm,
/// best matches first (like `sk --filter`). Whitespace splits the query
/// into terms that must all match; scores are summed. Ties keep
/// collection order.
fn fuzzy_filter<'a>(hosts: Vec<&'a str>, query: &str) -> Vec<&'a str> {
    let matcher = SkimMatcherV2::default();
    let terms: Vec<&str> = query.split_whitespace().collect();
    let mut scored: Vec<(i64, &str)> = hosts
        .into_iter()
        .filter_map(|host| {
            terms
                .iter()
                .map(|term| matcher.fuzzy_match(host, term))
                .sum::<Option<i64>>()
                .map(|score| (score, host))
        })
        .collect();
    scored.sort_by_key(|&(score, _)| std::cmp::Reverse(score));
    scored.into_iter().map(|(_, host)| host).collect()
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let home = std::env::var("HOME").wrap_err("HOME environment variable is not set")?;
    let query = std::env::args().nth(1).unwrap_or_default();

    let config_files = glob_files(&format!("{home}/.ssh/config.d/*"));
    let known_files = glob_files(&format!("{home}/.ssh/known_hosts*"));

    // Size the pool to the work: spinning up a thread per core costs more
    // than parsing a handful of files saves.
    let threads = (config_files.len() + known_files.len()).clamp(1, 4);
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()
        .ok();

    let (config_contents, known_contents) =
        rayon::join(|| read_files(&config_files), || read_files(&known_files));

    // Hosts must be UTF-8 from here on (fuzzy matching, JSON); skip any that aren't.
    let mut hosts: Vec<&str> = collect_hosts(&config_contents, &known_contents)
        .into_iter()
        .filter_map(|host| std::str::from_utf8(host).ok())
        .collect();
    if !query.is_empty() {
        hosts = fuzzy_filter(hosts, &query);
    }

    let output = ScriptFilter {
        items: hosts
            .iter()
            .map(|&host| Item {
                uid: host,
                title: host,
                subtitle: Subtitle(host),
                arg: host,
            })
            .collect(),
    };
    // One buffer sized for the whole document, one write syscall.
    let mut stdout = std::io::BufWriter::with_capacity(1 << 20, std::io::stdout().lock());
    serde_json::to_writer(&mut stdout, &output)?;
    std::io::Write::write_all(&mut stdout, b"\n")?;
    Ok(())
}
