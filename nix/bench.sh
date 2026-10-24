# Compare both revisions with the current pinned build recipe and toolchain.
previous=${1:?Usage: nix run .#bench -- PREVIOUS_COMMIT}
current=$(git rev-parse HEAD)
previous=$(git rev-parse --verify "$previous^{commit}")
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
root=$(git rev-parse --show-toplevel)
for revision in "$previous" "$current"; do
  source_dir="$work/$revision"
  mkdir -p "$source_dir"
  git archive "$revision" | tar -x -C "$source_dir"
  # Historical revisions need not have a flake. Use the same recipe for both.
  cp "$root/flake.nix" "$root/flake.lock" "$source_dir/"
  mkdir -p "$source_dir/nix"
  cp -r "$root/nix/." "$source_dir/nix/"
  binary=$(nix build "path:$source_dir#komodo" --no-link --print-out-paths)
  export KOMODO_STD="$source_dir/std/"
  for file in "$source_dir"/bench/*.komodo; do
    args=(--project komodo --branch main --hash "$revision" --testbed ci-runner
      --adapter shell_hyperfine --file "$work/results.json")
    if [[ "$revision" == "$current" ]]; then
      args+=(--start-point main --start-point-hash "$previous" --threshold-measure latency
        --threshold-test percentage --threshold-upper-boundary 0.25 --err)
    fi
    bencher run "${args[@]}" \
      "hyperfine --command-name '$(basename "$file" .komodo)' --export-json '$work/results.json' '$binary/bin/komodo $file'"
  done
done
