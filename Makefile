# TODO just switch to cmake
COMPILER ?= /opt/homebrew/Cellar/llvm/17.0.1/bin/clang++
CXXFLAGS = -std=c++2b -O3 -Wall -Wpedantic
BUILD_DIR = ./build
SRC_FILES = main.cpp
HEADER_FILES = chess.h util.h

all: $(BUILD_DIR)/athena

$(BUILD_DIR)/athena: $(BUILD_DIR) $(SRC_FILES) $(HEADER_FILES)
	$(COMPILER) $(CXXFLAGS) $(SRC_FILES) -o $(BUILD_DIR)/athena

run: $(BUILD_DIR)/athena
	$(BUILD_DIR)/athena

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)