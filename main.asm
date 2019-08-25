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

PLXStart EQU $0a * 8 ; 80
PLXEnd EQU $0f * 8 ; 120
PLXLength EQU PLXEnd - PLXStart ; 40
PLXOffset EQU 75
ScreenHeight EQU $12 * 8
songTimerSpeed EQU 6 ; this is just what famitracker uses

; create variables. make sure to use tab (why??)
	SpriteAttr Sprite0
	LoNVar plxTable, PLXLength ; c0a0 @40
	LoByteVar VBLANKED ; c0c8 @1
	LoWordVar scrollX ; c0c9 @2
	LoWordVar songPtr ; c0cb @2
	LoByteVar songTimer ; c0cd @1

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
GameTileMnt:		incbin "tile_mnt1.png.2bp"
GameTileMntEnd:
GameTileEnd:
SoundNotesCh1:
REPT 100
	DB %10110010, %00000110			; g3  
	DB %00000000, %00000000			; ---
	DB %10110010, %00000110			; g3
	DB %00000000, %00000000			; ---
	DB %11100111, %00000110			; a#3
	DB %00000000, %00000000			; ---
	DB %00100001, %00000111			; d4 
	DB %00000000, %00000000			; ---
	DB %00111001, %00000111			; e4 
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00100001, %00000111			; d4 
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %10001001, %00000110			; f3
	DB %00000000, %00000000			; ---
	DB %11010110, %00000110			; a3
	DB %00000000, %00000000			; ---
	DB %00100001, %00000111			; d4  
	DB %00000000, %00000000			; ---
	DB %00111001, %00000111			; e4  
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00100001, %00000111			; d4
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
	DB %00000000, %00000000			; ---
ENDR
EndSoundNotesCh1:
; snd frequency = ~(131072 / Hz - 1)
; f3 - 11010000111 - 349.23
; g3 - 11010110000 - 392.00
; a3 - 11011010101 - 440.00
; a#3- 11011100110 - 466.16
; d4 - 11100100000 - 587.33
; e4 - 11100111000 - 659.25

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
	xor a
	ld [Sprite0YAddr], a
	ld [Sprite0XAddr], a
	ld [Sprite0TileNum], a
	ld [Sprite0Flags], a
	ld [scrollX], a
	ld [scrollX+1], a
	
; sound pointer init
	ld hl, SoundNotesCh1
	ld a, h
	ld [songPtr+1], a
	ld a, l
	ld [songPtr], a ; little endian
	
	ld a, songTimerSpeed
	ld [songTimer], a

; write those tiles from ROM!
	ld hl,Title
	ld de, _SCRN0+(SCRN_VY_B*0)
	ld bc, TitleEnd-Title
	call mem_CopyVRAM
	
;um hi, make hblank trigger lcdc interrupt
	ld	a, STATF_MODE00
	ld	[rSTAT], a

; you want sound? too bad. here crash.
	ld a, $80
	ld [rNR52], a ; turn OFF sound system
	ld a, $ff
	ld [rNR50], a ; turn on both speakers
	ld a, $ff
	ld [rNR51], a ; direct all channels to all speakers
	
; sound ch1
	ld a, %00000000
	ld [rNR10], a ; no sweep
	ld a, %01000000 ; DDLLLLLL - Duty (00:12.5% 01:25% 10:50% 11:75%), length
	ld [rNR11], a ; set duty and length 
	ld a, %11110000 ; VVVVDSSS - initial value, 0=dec 1=inc, num of env sweep
	ld [rNR12], a ; envelope
	ld a, %00001001
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
	
	call music
	
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
	
	call PLXTable		; generate parallax table
	
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

music::
	; decrement songTimer
	ld hl, songTimer
	ld a, [hl]
	dec a
	; if songTimer is not zero, we don't need to update sound
	jp nz, .notyet
	; reset the timer for later
	ld [hl], songTimerSpeed
	
	; load the byte from songPtr's deref'd loc...
	ld a, [songPtr+1]
	ld h, a
	ld a, [songPtr]
	ld l, a ; hl contains songPtr word
	
	ld a, [hl]
	ld b, a
	inc hl
	ld a, [hl]
	ld h, b
	ld l, a ; hl contains dereferenced songPtr word. h=lofreq, l=hifreq
	; check if we need to skip it
	
	ld a, l
	and a
	jp nz, .oknote
	ld a, h
	and a
	jp nz, .oknote
	; uh oh this is a silent note, let's bail
	jp .nochange
	
.oknote:
	; set the useless sound registers
	ld a, %00000000 ; no sweep
	ld [rNR10], a
	ld a, %01000000 ; duty 25%, length 0
	ld [rNR11], a
	ld a, %11110000 ; max volume, no envelope
	ld [rNR12], a
	; ...into the low freq
	ld a, h
	ld [rNR13], a
	; load the byte from songPtr+1's loc, set the "trigger" bit...
	ld a, l
	or %10000000
	; ...and into the high freq/trigger register
	ld [rNR14], a
.nochange:
	; increment the songPtr and save it
	ld a, [songPtr+1]
	ld h, a
	ld a, [songPtr]
	ld l, a ; hl contains songPtr word
	inc hl
	inc hl ; 1 tracker note = 2bytes
	ld a, l
	ld [songPtr], a
	ld a, h
	ld [songPtr+1], a
.done:
	ret
.notyet:
	; write the new value of songTimer
	ld [hl], a
	ret

; *****************************************************************************
; PLXTable - compute each scanline's scroll value
; *****************************************************************************
PLXTable::
	xor a
	ld hl, plxTable
.loop:
	push af			; save relative scanline
	add PLXStart	; make it absolute scanline
	push hl
	
	sub PLXOffset
	ld h, a				; h = scanline - 75
	
	ld a, [scrollX]
	ld e, a
	ld a, [scrollX+1]
	ld d, a				; de = scrollX 16bit
	
	ld a, h				; a = (scanline - 75)
	sra a				; a = (scanline - 75) >> 1
	ld c, a				; c = ^^
	ld b, 0
	call Mul16			; hl = (scrollX * (scanline - 75) >> 1)
	
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l				; hl = (scrollX * (scanline - 75) >> 1) >> 3
	
	ld a, l				; a = LSB of that function
	
	pop hl
	ld [hl+], a		; save to ram at index, increment hl
	ld [hl+], a		; each scroll layer is 2px
	ld [hl+], a		; layer is 3px... comment me out if you can spare cycles
	ld [hl+], a		; layer is 4px... comment me out if you can spare cycles
	pop af			; restore relative scanline
	inc a
	inc a
	inc a			; 3px comment me out if you can spare cycles
	inc a			; 4px comment me out if you can spare cycles
	cp a, PLXLength
	jr nz, .loop
	ret

LCDC_STAT:
	push af
	push hl
	
	ld a, [rLY]			; read scanline
	ld h, PLXStart		; would be -1; too many clock cycles. let's not...
	cp h
	jp C, endLCDC		; if scanLine < LayerGrassStart, go to the end
	
	ld h, PLXEnd
	cp h
	jp NC, endLCDC		; if scanline >= LayerTrackStart, go to the end
	
	; read from the array
	sub PLXStart		; a = index
	ld hl, plxTable		; hopefully the LSB of the location of plxTable in mem
	add a, l			; is low enough that this add doesn't overflow
	ld l, a
	ld a, [hl]			; load that byte
	
	ld [rSCX], a		; then set it as scroll var
	
endLCDC:
	pop hl
	pop af
	reti

INCLUDE "algorithms.asm"
