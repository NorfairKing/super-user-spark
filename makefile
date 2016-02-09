aGll: build test

build: FORCE
	stack build

test: FORCE
	stack test --test-arguments="--seed=42" # No flaky tests!

install: FORCE
	stack install

pedantic:
	stack clean
	stack build \
    --pedantic \
    --fast \
    --jobs=8 \
    --ghc-options="\
        -fforce-recomp \
        -O0 \
        -Wall \
        -Werror \
        -fwarn-unused-imports \
        -fwarn-incomplete-patterns \
        -fwarn-unused-do-bind \
        -fno-warn-name-shadowing \
        -fno-warn-overlapping-patterns \
        -fno-warn-orphans" \
		--test

love:
	@echo "not war"
	
DIRTY_EXT = *.o *.hi *.bin

FORCE:

clean:
	rm -f $(BIN) $(DIRTY_EXT)
