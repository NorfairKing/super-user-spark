set -e # abort on error

./scripts/code_health.sh
./scripts/build.sh
./scripts/test.sh
./scripts/install.sh
