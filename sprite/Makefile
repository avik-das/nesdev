# must be on the PATH
OPHIS := ophis

TARGET := sprite
SRC := *.asm
OUT := $(TARGET).nes

$(OUT): $(SRC)
	$(OPHIS) $(SRC) $(OUT)

.PHONY: clean
clean:
	rm $(OUT)
