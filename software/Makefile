.PHONY: clean

CPU := msp430g2553

TOOLCHAIN_PATH := /opt/toolchains/msp430-ada-toolchain
OUTPUT_PATH := obj
export PATH := $(PATH):$(TOOLCHAIN_PATH)/bin

SRC_FILE := watch.ads

FLAGS = --RTS=rts/msp430f2xx -mmcu=$(CPU) \
	-Os -gdwarf-2 -gnatwp -gnatwu -gnatn \
	-gnatp -gnatVn -gnatef -fverbose-asm  \
	-frename-registers -fdata-sections \
	-ffunction-sections  -I$(TOOLCHAIN_PATH)/msp430/lib/cpus \
	-I$(TOOLCHAIN_PATH)/msp430/lib/packages/ \
	-largs -Wl,--gc-sections

watch.elf:
	msp430-gnatmake watch.adb -o $(OUTPUT_PATH)/watch.elf -D $(OUTPUT_PATH) $(FLAGS)

flash:
	mspdebug rf2500 "prog $(OUTPUT_PATH)/watch.elf"

clean:
	rm -f *.o
	rm -f *.ali
	rm -f $(OUTPUT_PATH)/*.o
	rm -f $(OUTPUT_PATH)/*.ali
	rm b~watch.adb
	rm b~watch.ads

all: watch.elf
