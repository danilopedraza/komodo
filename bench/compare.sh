#!/usr/bin/env bash
# Compare two already-built release binaries without checking out revisions.
set -euo pipefail

if [[ $# != 2 ]]; then
    echo "Usage: bash bench/compare.sh /absolute/path/to/baseline /absolute/path/to/candidate" >&2
    exit 1
fi

if [[ -n ${RESULTS_DIR:-} && $RESULTS_DIR != /* ]]; then
    RESULTS_DIR="$PWD/$RESULTS_DIR"
fi
if [[ -n ${BASELINE_STD:-} && $BASELINE_STD != /* ]]; then
    BASELINE_STD="$PWD/$BASELINE_STD"
fi
if [[ -n ${CANDIDATE_STD:-} && $CANDIDATE_STD != /* ]]; then
    CANDIDATE_STD="$PWD/$CANDIDATE_STD"
fi
cd "$(dirname "$0")/.."
printf -v baseline_std '%q' "${BASELINE_STD:-$PWD/std}"
printf -v candidate_std '%q' "${CANDIDATE_STD:-$PWD/std}"
printf -v baseline '%q' "$1"
printf -v candidate '%q' "$2"
if [[ -n ${RESULTS_DIR:-} ]]; then
    mkdir -p "$RESULTS_DIR"
fi
for workload in calls closures loops polyominoes indexing snapshots literals blocks scope-mutation tail-calls capture-retention recursive-retention; do
    echo "$workload"
    export_args=()
    if [[ -n ${RESULTS_DIR:-} ]]; then
        export_args=(--export-json "$RESULTS_DIR/$workload.json")
    fi
    hyperfine --shell bash --warmup 3 --runs "${RUNS:-15}" \
        "${export_args[@]}" \
        --command-name "baseline: $workload" "KOMODO_STD=$baseline_std $baseline bench/$workload.komodo" \
        --command-name "candidate: $workload" "KOMODO_STD=$candidate_std $candidate bench/$workload.komodo"
done
