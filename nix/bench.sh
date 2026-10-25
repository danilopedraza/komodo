# Compare both revisions with the current pinned build recipe and toolchain.
previous=${1:?Usage: nix run .#bench -- PREVIOUS_COMMIT}
current=$(git rev-parse HEAD)
previous=$(git rev-parse --verify "$previous^{commit}")
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
root=$(git rev-parse --show-toplevel)
binaries=()
for revision in "$previous" "$current"; do
  source_dir="$work/$revision"
  mkdir -p "$source_dir"
  git archive "$revision" | tar -x -C "$source_dir"
  # Historical revisions need not have a flake. Use the same recipe for both.
  cp "$root/flake.nix" "$root/flake.lock" "$source_dir/"
  mkdir -p "$source_dir/nix"
  cp -r "$root/nix/." "$source_dir/nix/"
  binary=$(nix build "path:$source_dir#komodo" --no-link --print-out-paths)
  binaries+=("$binary/bin/komodo")
done

# Use the current workloads and standard library for both binaries, including
# workloads that did not exist at the baseline revision.
RESULTS_DIR="$work/results" bash "$root/bench/compare.sh" "${binaries[@]}"

for index in 0 1; do
  revision=$previous
  if [[ "$index" == 1 ]]; then
    revision=$current
  fi
  # Bencher expects one result per workload, named identically for both hashes.
  report="$work/$index.json"
  jq -s --argjson index "$index" \
    '{results: [.[] | .results[$index] | .command |= sub("^(baseline|candidate): "; "")]}' \
    "$work"/results/*.json > "$report"
  args=(--project komodo --branch main --hash "$revision" --testbed ci-runner
    --adapter shell_hyperfine --file "$report")
  if [[ "$index" == 1 ]]; then
    args+=(--start-point main --start-point-hash "$previous" --threshold-measure latency
      --threshold-test percentage --threshold-upper-boundary 0.25 --err)
  fi
  bencher run "${args[@]}"
done
