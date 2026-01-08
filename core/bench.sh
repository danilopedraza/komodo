# Example of use:
# BENCHER_API_TOKEN=<token> KOMODO_STD=$PWD/../std ./bench.sh $PWD/../bench 480c8d7a87199251658890d05ce65f6e075a05b6

set -e

benchmarks_dir=$1
second_to_last_push_hash=$2

git checkout $second_to_last_push_hash

cargo build --release

echo "Running benchmarks..."
for file in $(find "$benchmarks_dir" -type f -name "*.komodo"); do
    bencher run \
    --project komodo \
    --branch main \
    --start-point-reset \
    --hash $(git rev-parse HEAD) \
    --token $BENCHER_API_TOKEN \
    --testbed ci-runner \
    --adapter shell_hyperfine \
    --file results-current.json \
    "hyperfine --export-json results-current.json 'cargo run --release --quiet $file'"
done

git checkout main
git checkout HEAD

cargo build --release


for file in $(find "$benchmarks_dir" -type f -name "*.komodo"); do
    bencher run \
    --project komodo \
    --branch main \
    --start-point main \
    --start-point-hash $second_to_last_push_hash \
    --start-point-reset \
    --hash $second_to_last_push_hash \
    --token $BENCHER_API_TOKEN \
    --testbed ci-runner \
    --threshold-measure latency \
    --threshold-test percentage \
    --threshold-upper-boundary 0.25 \
    --thresholds-reset \
    --err \
    --adapter shell_hyperfine \
    --file results-previous.json \
    "hyperfine --export-json results-previous.json 'cargo run --release --quiet $file'"
done
