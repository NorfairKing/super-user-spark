NAME = spark
SRC = $(SRC_DIR)/Main.hs
BIN = $(NAME)

SRC_DIR = src
TEST_DIR = test

GHC = ghc
GHC_OPTIMISATION = -O2

GHC_FLAGS = \
	-fwarn-unused-imports \
	-fwarn-incomplete-patterns \
	-Wall \
	-fno-warn-unused-do-bind \
	-fno-warn-name-shadowing
GHC_SRC_DIRS = \
	-i$(SRC_DIR) \
	-ibenchmarks \
	-i$(TEST_DIR)
GHC_OPTIONS = \
	-threaded \
	$(GHC_OPTIMISATION) \
	$(GHC_FLAGS) \
	$(GHC_SRC_DIRS)



all: bin test

build:
	stack build

test:
	stack test

install:
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
        -fno-warn-orphans"


love:
	@echo "not war"
	
DIRTY_EXT = *.o *.hi *.bin

clean:
	rm -f $(BIN) $(DIRTY_EXT)
