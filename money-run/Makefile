# must be on the PATH
OPHIS := ophis

TARGET := $(shell basename $(CURDIR)).nes
SOURCE := $(wildcard *.asm)

$(TARGET): $(SOURCE)
	$(OPHIS) $(SOURCE) -o $(TARGET)

.PHONY: clean
clean:
	rm $(TARGET)
