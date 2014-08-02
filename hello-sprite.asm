; Hello Sprite
; originl version February 17, 2007
; John Harrison
; An extension of Hello World, based mostly from GALP

;* 2008-May-01 --- V1.0a
;*                 replaced reference of hello-sprite.inc with sprite.inc

INCLUDE "gbhw.inc" ; standard hardware definitions from devrs.com
INCLUDE "ibmpc1.inc" ; ASCII character set from devrs.com
INCLUDE "sprite.inc" ; specific defs

LoNVar:	MACRO
\1		EQU	LoRamBase
LoRamBase	SET	LoRamBase+(\2)
		ENDM

PLXStart EQU $0a * 8
PLXEnd EQU $0f * 8
PLXOffset EQU 75
ScreenHeight EQU $12 * 8

; create variables. make sure to use tab (why??)
	SpriteAttr Sprite0
	LoByteVar VBLANKED
	LoWordVar scrollX
	LoNVar asdf, 10

; IRQs
SECTION "Vblank", HOME[$0040]
	jp DMACODELOC
SECTION "LCDC", HOME[$0048]
	jp LCDC_STAT
SECTION "Timer_Overflow", HOME[$0050]
	reti
SECTION "Serial", HOME[$0058]
	reti
SECTION "p1thru4", HOME[$0060]
	reti

; boot loader jumps to here.
SECTION "start", HOME[$0100]
nop
jp begin

; *****************************************************************************
; header and and hardcoded data
; *****************************************************************************
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE
INCLUDE "memory.asm"
TileData:
	chr_IBMPC1 1, 4 ; some character set
TileDataEnd:
Title:
	;  [                    ] 20tiles
	DB "                                "
	DB "                                "
	DB "                                "
	DB "                                "
	DB "                                "
	DB "                                "
	
	DB "                                "
	DB $85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88,$85,$86,$87,$88
	DB $89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c,$89,$8a,$8b,$8c
	DB $8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90,$8d,$8e,$8f,$90
	
	DB $82,$81,$82,$80,$80,$81,$82,$81,$80,$82,$80,$81,$80,$80,$80,$82,$82,$80,$81,$82,$82,$80,$82,$82,$82,$82,$81,$80,$82,$80,$80,$81
	DB $81,$80,$81,$80,$82,$82,$82,$81,$80,$82,$81,$82,$80,$81,$80,$81,$81,$81,$80,$81,$81,$80,$81,$80,$82,$82,$82,$81,$82,$82,$82,$82
	DB $82,$80,$82,$81,$82,$82,$80,$81,$82,$81,$80,$81,$81,$80,$80,$81,$81,$82,$80,$82,$80,$82,$80,$81,$82,$80,$80,$80,$80,$80,$81,$80
	DB $81,$82,$82,$81,$82,$81,$82,$80,$80,$82,$82,$82,$80,$82,$80,$82,$81,$81,$82,$81,$80,$80,$81,$80,$80,$80,$80,$80,$80,$81,$80,$80
	DB $80,$80,$80,$82,$81,$80,$82,$81,$81,$81,$80,$80,$81,$81,$80,$82,$82,$82,$80,$80,$80,$81,$82,$82,$82,$80,$81,$81,$80,$80,$82,$81
	
	DB $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
	DB $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
	
	DB $81,$82,$80,$82,$82,$81,$82,$81,$82,$81,$82,$80,$81,$82,$80,$81,$80,$80,$82,$80,$80,$82,$80,$80,$81,$81,$81,$81,$80,$81,$80,$82
TitleEnd:
GameTile:
GameTile_Dirt0:		incbin "tile_dirt0.png.2bp"
GameTile_Dirt1:		incbin "tile_dirt1.png.2bp"
GameTile_Dirt2:		incbin "tile_dirt2.png.2bp"
GameTile_TrackHi:	incbin "tile_trackhi.png.2bp"
GameTile_TrackLow:	incbin "tile_tracklow.png.2bp"
GameTileMnt:
GameTile_Mnt00:		incbin "tile_mnt00.png.2bp"
GameTile_Mnt10:		incbin "tile_mnt10.png.2bp"
GameTile_Mnt20:		incbin "tile_mnt20.png.2bp"
GameTile_Mnt30:		incbin "tile_mnt30.png.2bp"
GameTile_Mnt01:		incbin "tile_mnt01.png.2bp"
GameTile_Mnt11:		incbin "tile_mnt11.png.2bp"
GameTile_Mnt21:		incbin "tile_mnt21.png.2bp"
GameTile_Mnt31:		incbin "tile_mnt31.png.2bp"
GameTile_Mnt02:		incbin "tile_mnt02.png.2bp"
GameTile_Mnt12:		incbin "tile_mnt12.png.2bp"
GameTile_Mnt22:		incbin "tile_mnt22.png.2bp"
GameTile_Mnt32:		incbin "tile_mnt32.png.2bp"
GameTileMntEnd:
GameTileEnd:

; *****************************************************************************
; Initialization
; *****************************************************************************
begin:
	nop
	di
	ld sp, $ffff			; set the stack pointer to highest mem location + 1

; NEXT FOUR LINES FOR SETTING UP SPRITES *hs*
	call initdma			; move routine to HRAM
	ld a, IEF_LCDC | IEF_VBLANK
	ld [rIE], a				; ENABLE VBLANK INTERRUPT (and lcdc)
	ei						; LET THE INTS FLY

init:
	ld a, %11100100		; Window palette colors, from darkest to lightest
	ld [rBGP], a		; set background and window pallette
	ldh [rOBP0], a		; set sprite pallette 0
	ldh [rOBP1], a		; 1 (choose palette 0 or 1 when describing the sprite)
	
	ld a, 0				; SET SCREEN TO TO UPPER RIGHT HAND CORNER
	ld [rSCX], a
	ld [rSCY], a
	
	call StopLCD		; YOU CAN NOT LOAD $8000 WITH LCD ON
	ld hl, TileData
	ld de, _VRAM		; $8000
	ld bc, TileDataEnd - TileData
	call mem_CopyMono	; load tile data
	
	ld hl, GameTile
	ld de, _VRAM + $800
	ld bc, GameTileEnd - GameTile
	call mem_CopyVRAM
	
	ld a, 0
	ld hl, OAMDATALOC
	ld bc, OAMDATALENGTH
	call mem_Set		; *hs* erase sprite table

	ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON
	ld [rLCDC], a		; LCD back on

	ld a, 32			; ascii for space
	ld hl, _SCRN0
	ld bc, SCRN_VX_B * SCRN_VY_B
	call mem_SetVRAM
; *****************************************************************************
; Main code
; *****************************************************************************
; general init
	ld a, $0
	ld [Sprite0YAddr], a
	ld [Sprite0XAddr], a
	ld [Sprite0TileNum], a
	ld [Sprite0Flags], a
	ld [scrollX], a

; write those tiles from ROM!
	ld hl,Title
	ld de, _SCRN0+(SCRN_VY_B*0)
	ld bc, TitleEnd-Title
	call mem_CopyVRAM
	
;um hi, make hblank trigger lcdc interrupt
	ld	a, STATF_MODE00
	ld	[rSTAT], a

; you want sound? too bad. here crash.
	ld a, $00 ;$80
	ld [rNR52], a ; turn OFF sound system
	
	ld a, $ff
	ld [rNR50], a ; turn on both speakers
	
	ld a, $ff
	ld [rNR51], a ; direct all channels to all speakers
	
; sound ch1
	ld a, %00000000
	ld [rNR10], a ; no sweep
	
	ld a, %01111111 ; DDLLLLLL - Duty (00:12.5% 01:25% 10:50% 11:75%), length
	ld [rNR11], a ; set duty and length 
	
	ld a, %00111000 ; VVVVDSSS - initial value, 0=dec 1=inc, num of env sweep
	ld [rNR12], a ; envelope
	
	ld a, %01111111
	ld [rNR13], a ; lo frequency
	
	ld a, %10000110 ; IC...FFF - Initial, counter, hi frequency
	ld [rNR14], a ; pull the trigger

; sprite metadata
	PutSpriteYAddr Sprite0, 0	; set Sprite0 location to 0,0
	PutSpriteXAddr Sprite0, 0
	ld a, 1						; happy face :-)
	ld [Sprite0TileNum], a		; tile address
	ld a, %00000000				; gbhw.inc 33-42
	ld [Sprite0Flags], a

MainLoop:
	halt
	nop					; always put NOP after HALT
	
	ld a, [VBLANKED]
	or a				; V-Blank interrupt ?
	jr z, MainLoop		; No, some other interrupt
	xor a
	ld [VBLANKED], a	; clear flag
	
	; 16-bit scroller variable increment 
	ld a, [scrollX]
	ld c, a
	ld a, [scrollX+1]
	ld b, a
	inc bc
	ld a, b
	ld [scrollX+1], a
	ld a, c
	ld [scrollX], a
	
	srl b
	rr c
	srl b
	rr c
	ld a, c
	ld [rSCX], a		; mountains move at 1/4 pixels per second
	
	call	GetKeys
	
	push	af
	and	PADF_RIGHT
	call	nz,right
	pop	af
	
	push	af
	and	PADF_LEFT
	call	nz,left
	pop	af
	
	push	af
	and	PADF_UP
	call	nz,up
	pop	af
	
	push	af
	and	PADF_DOWN
	call	nz,down
	pop	af
	
	push	af
	and	PADF_START
	call	nz,Yflip
	pop	af
	
	jr	MainLoop

right:
	GetSpriteXAddr Sprite0
	cp SCRN_X - 8	; already on RHS of screen?
	ret z
	inc a
	PutSpriteXAddr Sprite0, a
	ret
left:
	GetSpriteXAddr Sprite0
	cp 0			; already on LHS of screen?
	ret z
	dec a
	PutSpriteXAddr Sprite0, a
	ret
up:
	GetSpriteYAddr Sprite0
	cp 0			; already at top of screen?
	ret z
	dec a
	PutSpriteYAddr Sprite0, a
	ret
down:
	GetSpriteYAddr Sprite0
	cp SCRN_Y - 8	; already at bottom of screen?
	ret z
	inc a
	PutSpriteYAddr Sprite0, a
	ret
Yflip:
	ld a, [Sprite0Flags]
	xor OAMF_YFLIP	; toggle flip of sprite vertically
	ld [Sprite0Flags], a
	ret

LCDC_STAT:
	push af
	push hl
	push de
	push bc
	
	ld a, [rLY]			; read scanline
	ld h, PLXStart-1	; -1 because I'm using way too many clock cycles
	cp h
	jp C, endLCDC		; if scanLine < LayerGrassStart, go to the end
	
	ld h, PLXEnd
	cp h
	jp NC, endLCDC		; if scanline >= LayerTrackStart, go to the end
	
	ld hl, 0			; clear hl dunno...
	
	sub PLXOffset
	ld h, a				; h = scanline - 75
	
	ld a, [scrollX]
	ld e, a
	ld a, [scrollX+1]
	ld d, a				; de = scrollX 16bit
	
	ld a, h				; a = (scanline - 75)
	sra a				; a = (scanline - 75) >> 1
	ld c, a				; h = ^^
	ld b, 0
	call Mul16			; hl = (scrollX * (scanline - 75) >> 1)
	
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l				; hl = (scrollX * (scanline - 75) >> 1) >> 3
	
	ld a, l				; a = low order bits
	
	ld [rSCX], a		; then set it as scroll var
	
endLCDC:
	pop bc
	pop de
	pop hl
	pop af
	reti

INCLUDE "algorithms.asm"
