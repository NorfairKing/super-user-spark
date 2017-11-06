source scripts/lib.sh
bld_ () {
  stack clean && stack build --pedantic
}
check "Pedantic checking" bld_
