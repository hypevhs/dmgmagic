; Hello Sprite
; originl version February 17, 2007
; John Harrison
; An extension of Hello World, based mostly from GALP

;* 2008-May-01 --- V1.0a
;*                 replaced reference of hello-sprite.inc with sprite.inc

INCLUDE "gbhw.inc" ; standard hardware definitions from devrs.com
INCLUDE "ibmpc1.inc" ; ASCII character set from devrs.com
INCLUDE "sprite.inc" ; specific defs

; constants
PLXStart EQU $0a * 8 ; 80, $50
PLXEnd EQU $0f * 8 ; 120, $78
PLXLength EQU PLXEnd - PLXStart ; 40
PLXOffset EQU 75
ScreenHeight EQU $12 * 8
MusicRAMLength EQU $80

; RAM variables
; music RAM section must not move from this offset
SECTION "Music RAM", WRAM0[$c000]
MusicRAM:
GBCFlag:	DS 1
GBAFlag:	DS 1
SndEnabled:	DS 1
MusicRAMEnd:
			DS MusicRAMLength-(MusicRAMEnd-MusicRAM) ; pad to size

SECTION "Regular RAM", WRAM0
VBLANKED:	DS 1
scrollX:	DS 2

SECTION "Parallax Table", WRAM0, ALIGN[8]
plxTable:	DS PLXLength		; $100-aligned

; WRAMX variables. first $a0 bytes are Fake OAM
SECTION "RAM1", WRAM0[$d000]
	SpriteAttr Sprite0
OAMData:	DS $a0				; shitty macro protection

; RST jump vectors
SECTION "Org $0000", ROM0[$0000]
RST_00:		ld b, b
SECTION "Org $0008", ROM0[$0008]
RST_08:		ld b, b
SECTION "Org $0010", ROM0[$0010]
RST_10:		ld b, b
SECTION "Org $0018", ROM0[$0018]
RST_18:		ld b, b
SECTION "Org $0020", ROM0[$0020]
RST_20:		ld b, b
SECTION "Org $0028", ROM0[$0028]
RST_28:		ld b, b
SECTION "Org $0030", ROM0[$0030]
RST_30:		ld b, b
SECTION "Org $0038", ROM0[$0038]
RST_38:		ld b, b

; Interrupts
SECTION "VBlank", ROM0[$0040]
	; copy OAM, though it's a frame out of date
	; this also sets VBLANKED so that when the DMA is RETI'd
	; the game logic after MainLoop's HALT will execute
	jp DMACODELOC
SECTION "LCDC", ROM0[$0048]
	jp LCDC_STAT
SECTION "Timer", ROM0[$0050]
	reti
SECTION "Serial", ROM0[$0058]
	reti
SECTION "p1thru4", ROM0[$0060]
	reti

; boot loader jumps to here.
SECTION "start", ROM0[$0100]
	nop
	jp begin

; *****************************************************************************
; header and and hardcoded data
; *****************************************************************************
SECTION "header", ROM0[$0104]
	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the GB
	; will not run the program)
	NINTENDO_LOGO

	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	;	 0123456789ABCDE
	db	"AvgDayCowboyV01"

	; $0143 (Game Boy Color compatibility code)
	db	$00		; $00 - DMG
				; $80 - DMG/GBC
				; $C0 - GBC Only cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	db	0

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	db	0

	; $0146 (Game Boy/Super Game Boy indicator)
	db	0

	; $0147 (Cartridge type - all Game Boy Color cartridges are at least $19)
	db	$19	; $19 - MBC5

	; $0148 (ROM size)
	db	$4	; $4 = 512Kb (32 banks)

	; $0149 (RAM size)
	db	0	; $00 - None

	; $014A (Destination code)
	db	1	; $01 - All others
			; $00 - Japan

	; $014B (Licensee code - this _must_ be $33)
	db	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version)
	db	0

	; $014D (Complement check - handled by post-linking tool)
	db	0

	; $014E-$014F (Cartridge checksum - handled by post-linking tool)
	dw	0

SECTION "Music in ROM0", ROM0[$0500]
MusicLoad EQU $0500		; deflemask-generated .GBS have procedures
MusicInit EQU $05ec		; at these locations
MusicPlay EQU $0544		; see also: https://github.com/DevEd2/Deflemask2GB
	; music code
	; darkman.gbs
	;
	; size			$1eaf (7855_10)
	; offset		$0070
	; includedSize	$1e3f
	incbin "darkman.gbs",$70,$1e3f

SECTION "ROM data", ROM0
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

; *****************************************************************************
; Initialization
; *****************************************************************************

SECTION "Main code", ROM0
begin:
	nop
	di
	ld sp, $ffff		; set the stack pointer to highest mem location + 1

	call initdma		; move routine to HRAM

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

; clear out OAM (in RAM)
	ld a, 0
	ld hl, OAMDATALOC
	ld bc, OAMDATALENGTH
	call mem_Set

; DMA OAM
	call DMACODELOC

; clear tiles
	ld a, 32			; ascii for space
	ld hl, _SCRN0
	ld bc, SCRN_VX_B * SCRN_VY_B
	call mem_SetVRAM

; write those tiles from ROM!
	ld hl,Title
	ld de, _SCRN0+(SCRN_VY_B*0)
	ld bc, TitleEnd-Title
	call mem_CopyVRAM

	di			; before we set IE, lots of the above macros/funcs re-enabled
	ld a, [rIF]	; IME with reti or ei.
	res 0, a	; we don't want vblank before we're done with initialization.
	res 1, a	; while we're at it, clear stale LCD interrupt flags,
	ld [rIF], a	; just in case

; enable vblank and LCDC interrupts
	ld a, IEF_LCDC | IEF_VBLANK
	ld [rIE], a

; enable y-coincidence interrupt for LCDC
	ld a, $ff
	ld [rLYC], a		; LY never reaches above 153 ($99) so === disabled
	ld a, STATF_LYC
	ld [rSTAT], a

; start screen again
	ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON
	ld [rLCDC], a

; *****************************************************************************
; Main code
; *****************************************************************************
; zero out WRAM things
	xor a
	ld [VBLANKED], a
	ld [scrollX], a
	ld [scrollX+1], a
	ld hl, plxTable
	ld bc, PLXLength
	call mem_Set

; sprite metadata
	PutSpriteYAddr Sprite0, 0	; necessary because X=Y=$00 is offscreen
	PutSpriteXAddr Sprite0, 0
	ld a, 1						; happy face :-)
	ld [Sprite0TileNum], a

; music RAM and code init
	xor a
	ld hl, MusicRAM
	ld bc, MusicRAMLength
	call mem_Set
	; GBCFlag = 0, GBAFlag = 0
	ld a, 1
	ld [SndEnabled], a
	; the init funcs don't set any registers before using them. undefined
	; behavior? zeroing out seems to work.
	xor a
	ld b,a
	ld c,a
	ld d,a
	ld e,a
	ld h,a
	ld l,a
	call MusicLoad
	call MusicInit

; all done with setup, begin interrupts
; (even though the music funcs already did, most likely)
; also, because I DMA'd earlier, need to reset this flag
; because we're likely halfway down the screen
	xor a
	ld [VBLANKED], a
	ei
; then move on to mainloop

MainLoop:
	halt
	nop					; always put NOP after HALT

	ld a, [VBLANKED]
	; TODO: vblank can execute right here and cause a skipped frame...
	or a				; V-Blank interrupt ?
	jr z, MainLoop		; No, some other interrupt
	xor a
	ld [VBLANKED], a	; clear flag

	call MusicPlay		; every VBlank, jump into the .gbs file to play music

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
	ld a, PLXStart
	ld [rLYC], a		; interrupt when rLY is at the first parallax line

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

; *****************************************************************************
; PLXTable - compute each scanline's scroll value
; *****************************************************************************
PLXTable::
	xor a			; a = first scanline
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
.wait_hblank:			; spinloop until hblank
	ld a, [rSTAT]
	and %00000011		; if LCD mode bits are set (ie not in mode 0)
	jp nz, .wait_hblank

	ld a, [rLY]			; read scanline
	ld h, PLXEnd
	cp h				; if scanline >= LayerTrackStart, go to the end
	jp NC, endLCDC		; rLYC isn't updated, so this ends parallax til vblank

	; todo: any optimizations from $100-alignment?
	sub PLXStart		; a = index
	ld hl, plxTable		; hl = scroll table start
	add a, l			; hl += index
	ld l, a				; (which is low enough that this add doesn't overflow)
	ld a, [hl]			; load from scroll table

	ld [rSCX], a		; scroll the window

	ld a, [rLYC]
	add a, 4
	ld [rLYC], a		; see you again in 4 lines
endLCDC:
	pop hl
	pop af
	reti

INCLUDE "algorithms.asm"
