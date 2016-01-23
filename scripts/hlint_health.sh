source scripts/lib.sh
h () {
  hlint src \
  --ignore "Use hierarchical imports" \
  --ignore "Use camelCase" \
  --ignore "Redundant do" \
  --ignore "Redundant $"
}
check "Hlint" h
