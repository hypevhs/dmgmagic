ASM=main.asm
OBJ="$(ASM).o"
ROM="$(ASM).gb"
SYM="$(ASM).sym"
TILEGENPY=./tilegen.py
#WAVEGENPY=./wavegen.py
TILES=$(wildcard *.png)

$(ROM):
	python $(TILEGENPY) $(TILES)
	#/usr/bin/python $(WAVEGENPY) 255 38 wave.bin
	rgbasm -o $(OBJ) $(ASM)
	rgblink -o $(ROM) -n $(SYM) $(OBJ)
	rgbfix -vp 0xFF $(ROM)

deploy: $(ROM)
	cp $(ROM) $(SYM) /mnt/shared
