PATH=../rgbds
TILEGENPY=../tilegen/tg.py
ASM=hello-sprite.asm

rom:
	/usr/bin/find -type f -name "*png" -exec /usr/bin/python ${TILEGENPY} "{}" \;
	${PATH}/rgbasm -o ${ASM}.o ${ASM}
	${PATH}/rgblink -o ${ASM}.gb ${ASM}.o
	${PATH}/rgbfix -vp 0xFF ${ASM}.gb