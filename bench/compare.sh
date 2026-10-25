#!/usr/bin/env bash
# Compare two already-built release binaries without checking out revisions.
set -euo pipefail

if [[ $# != 2 ]]; then
    echo "Usage: bash bench/compare.sh /absolute/path/to/baseline /absolute/path/to/candidate" >&2
    exit 1
fi

cd "$(dirname "$0")/.."
export KOMODO_STD="$PWD/std"
printf -v baseline '%q' "$1"
printf -v candidate '%q' "$2"
if [[ -n ${RESULTS_DIR:-} ]]; then
    mkdir -p "$RESULTS_DIR"
fi
for workload in calls closures loops polyominoes; do
    echo "$workload"
    export_args=()
    if [[ -n ${RESULTS_DIR:-} ]]; then
        export_args=(--export-json "$RESULTS_DIR/$workload.json")
    fi
    hyperfine --shell bash --warmup 3 --runs "${RUNS:-15}" \
        "${export_args[@]}" \
        --command-name "baseline: $workload" "$baseline bench/$workload.komodo" \
        --command-name "candidate: $workload" "$candidate bench/$workload.komodo"
done
