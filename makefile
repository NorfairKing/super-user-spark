NAME = spark
SRC = $(SRC_DIR)/Main.hs
BIN = $(NAME)

GHC = ghc
GHC_OPTIMISATION = -O2
GHC_FLAGS = 	-fwarn-unused-imports \
				-fforce-recomp \
				-fwarn-incomplete-patterns \
				-Wall \
				-fno-warn-unused-do-bind \
				-fno-warn-name-shadowing
GHC_SRC_DIRS = 	-i$(SRC_DIR) \
			   	-ibenchmarks \
			   	-itests
GHC_OPTIONS = 	-threaded \
				$(GHC_OPTIMISATION) \
				$(GHC_FLAGS) \
				$(GHC_SRC_DIRS)


SRC_DIR = src

all: bin
	
bin:
	$(GHC) $(GHC_OPTIONS) -o $(BIN) --make $(SRC)

love:
	@echo "not war"
	
DIRTY_EXT = *.o *.hi *.bin

clean:
	rm -f $(BIN) $(DIRTY_EXT)
