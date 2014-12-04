ASM=hello-sprite.asm
TILEGENPY=./tilegen.py
#WAVEGENPY=./wavegen.py

rom:
	/usr/bin/find -type f -name "*png" -exec /usr/bin/python ${TILEGENPY} "{}" \;
	#/usr/bin/python ${WAVEGENPY} 255 38 wave.bin
	rgbasm -o ${ASM}.o ${ASM}
	rgblink -o ${ASM}.gb ${ASM}.o
	rgbfix -vp 0xFF ${ASM}.gb
