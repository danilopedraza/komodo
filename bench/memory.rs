//! Linux sampled high-water RSS; whole-process measurements, not allocation counts.
use std::{
    env, fs,
    io::{self, ErrorKind},
    path::{Path, PathBuf},
    process::Command,
    thread,
    time::Duration,
};

fn measure(binary: &Path, workload: &str, root: &Path) -> io::Result<u64> {
    let mut child = Command::new(binary)
        .arg(format!("bench/{workload}.komodo"))
        .current_dir(root)
        .env("KOMODO_STD", root.join("std"))
        .spawn()?;
    let status_path = format!("/proc/{}/status", child.id());
    let mut peak = 0;
    loop {
        match fs::read_to_string(&status_path) {
            Ok(status) => {
                if let Some(value) = status.lines().find_map(|line| {
                    line.strip_prefix("VmHWM:")?
                        .split_whitespace()
                        .next()?
                        .parse::<u64>()
                        .ok()
                }) {
                    peak = peak.max(value);
                }
            }
            Err(error) if error.kind() == ErrorKind::NotFound => {}
            Err(error) => {
                let _ = child.kill();
                let _ = child.wait();
                return Err(error);
            }
        }
        if let Some(status) = child.try_wait()? {
            if !status.success() {
                return Err(io::Error::other(format!("{workload}: {status}")));
            }
            return if peak == 0 {
                Err(io::Error::other(
                    "process exited before an RSS sample was available",
                ))
            } else {
                Ok(peak)
            };
        }
        thread::sleep(Duration::from_millis(1));
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args_os().skip(1);
    let baseline = PathBuf::from(args.next().ok_or("missing baseline binary")?);
    let candidate = PathBuf::from(args.next().ok_or("missing candidate binary")?);
    if !cfg!(target_os = "linux") {
        return Err("RSS sampling requires Linux /proc".into());
    }
    if !baseline.is_absolute() || !candidate.is_absolute() {
        return Err("binary paths must be absolute".into());
    }
    let root =
        PathBuf::from(env::var_os("KOMODO_BENCH_ROOT").ok_or("run via nix run .#bench-memory")?);
    let requested = args
        .map(|arg| arg.into_string())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| "workload names must be Unicode")?;
    let workloads = if requested.is_empty() {
        vec![
            "capture-retention".into(),
            "recursive-retention".into(),
            "snapshots".into(),
        ]
    } else {
        requested
    };
    println!("workload,binary,run,sampled_peak_rss_kib");
    for workload in workloads {
        if !workload
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
        {
            return Err(
                "workload names may contain only ASCII letters, digits, and hyphens".into(),
            );
        }
        for (label, binary) in [("baseline", &baseline), ("candidate", &candidate)] {
            let mut samples = Vec::with_capacity(5);
            for run in 1..=5 {
                let peak = measure(binary, &workload, &root)?;
                samples.push(peak);
                println!("{workload},{label},{run},{peak}");
            }
            samples.sort_unstable();
            println!("{workload},{label},median,{}", samples[2]);
        }
    }
    Ok(())
}
