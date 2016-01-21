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
	
bin:
	$(GHC) $(GHC_OPTIONS) -o $(BIN) --make $(SRC)

thorough:
	$(GHC) $(GHC_OPTIONS) -fforce-recomp -o $(BIN) --make $(SRC)

TEST_SRC = $(TEST_DIR)/MainTest.hs
TEST_BIN = $(NAME)_test

test: test_bin
	./$(TEST_BIN)

test_bin:
	$(GHC) $(GHC_OPTIONS) -o $(TEST_BIN) --make $(TEST_SRC)

love:
	@echo "not war"
	
DIRTY_EXT = *.o *.hi *.bin

clean:
	rm -f $(BIN) $(DIRTY_EXT)
