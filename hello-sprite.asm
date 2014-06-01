; Hello Sprite
; originl version February 17, 2007
; John Harrison
; An extension of Hello World, based mostly from GALP

;* 2008-May-01 --- V1.0a
;*                 replaced reference of hello-sprite.inc with sprite.inc

INCLUDE "gbhw.inc" ; standard hardware definitions from devrs.com
INCLUDE "ibmpc1.inc" ; ASCII character set from devrs.com
INCLUDE "sprite.inc" ; specific defs

Layer1Start EQU $0a * 8
Layer2Start EQU $0c * 8
Layer3Start EQU $0e * 8
Layer4Start EQU $10 * 8
LayerEnd EQU $12 * 8

; create variables. make sure to use tab (why??)
	SpriteAttr Sprite0 ; struct of 4
	LoByteVar VBLANKED

; IRQs
SECTION	"Vblank", HOME[$0040]
	jp	DMACODELOC
SECTION	"LCDC", HOME[$0048]
	jp LCDC_STAT
SECTION	"Timer_Overflow", HOME[$0050]
	reti
SECTION	"Serial", HOME[$0058]
	reti
SECTION	"p1thru4", HOME[$0060]
	reti

; boot loader jumps to here.
SECTION	"start",HOME[$0100]
nop
jp	begin

; ****************************************************************************************
; header and and hardcoded data
; ****************************************************************************************
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE
INCLUDE "memory.asm"
TileData:
	chr_IBMPC1 1, 4 ; some character set
Title:
	DB " ****************** ","            "
	DB "  * this is the  *  ","            "
	DB "   **************   ","            "
	DB "~~~~* parallax *~~~~","            "
	DB "     **********     ","            "
	DB "      * demo *      ","            "
	DB "~~~~~~~~~~~~~~~~~~~~","            "
	DB "  So please,        ","            "
	DB "     take a look    ","            "
	DB "       ",2,"            ","            "
	DB "11111111111111111111","            "
	DB "11111111111111111111","            "
	DB "22222222222222222222","            "
	DB "22222222222222222222","            "
	DB "33333333333333333333","            "
	DB "33333333333333333333","            "
	DB "44444444444444444444","            "
	DB "44444444444444444444","            "
	;  [                    ] 20tiles
TitleEnd:

; ****************************************************************************************
; Initialization
; ****************************************************************************************
begin:
	nop
	di
	ld sp, $ffff			; set the stack pointer to highest mem location + 1

; NEXT FOUR LINES FOR SETTING UP SPRITES *hs*
	call initdma			; move routine to HRAM
	ld a, IEF_LCDC | IEF_VBLANK
	ld [rIE], a				; ENABLE ONLY VBLANK INTERRUPT (lol no, enable lcdc too)
	ei						; LET THE INTS FLY

init:
	ld a, %11100100		; Window palette colors, from darkest to lightest
	ld [rBGP], a		; set background and window pallette
	ldh [rOBP0], a		; set sprite pallette 0 (choose palette 0 or 1 when describing the sprite)
	ldh [rOBP1], a		; set sprite pallette 1
	
	ld a, 0				; SET SCREEN TO TO UPPER RIGHT HAND CORNER
	ld [rSCX], a
	ld [rSCY], a
	
	call StopLCD		; YOU CAN NOT LOAD $8000 WITH LCD ON
	ld hl, TileData
	ld de, _VRAM		; $8000
	ld bc, 8*128		; half of ascii, 8B per char
	call mem_CopyMono	; load tile data

	ld a,0
	ld hl,OAMDATALOC
	ld bc,OAMDATALENGTH
	call mem_Set		; *hs* erase sprite table

	ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON;|LCDCF_BG9800|LCDCF_WIN9C00|LCDCF_WINON ; *hs* see gbspec.txt lines 1525-1565 and gbhw.inc lines 70-86
	; 11110011
	ld [rLCDC], a

	ld a, 32			; ascii for space
	ld hl, _SCRN0
	ld bc, SCRN_VX_B * SCRN_VY_B
	call mem_SetVRAM
	;ld a, 32			; ascii for space
	;ld hl, _SCRN1
	;ld bc, SCRN_VX_B * SCRN_VY_B
	;call mem_SetVRAM
; ****************************************************************************************
; Main code
; ****************************************************************************************
; general init
	ld a, $0
	ld [Sprite0YAddr], a
	ld [Sprite0XAddr], a
	ld [Sprite0TileNum], a
	ld [Sprite0Flags], a
	;ld [scroller], a

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
	
	ld a, %00111000 ; VVVVDSSS - initial value, 0=dec 1=inc, number of env sweep
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
	
	;reset scroller
	ld a, $0
	ld [rSCX], a
	
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
	
	; reset scroller
	;ld a, 40
	;ld [scroller], a
	
	jr	MainLoop

right:
	GetSpriteXAddr Sprite0
	cp SCRN_X-8		; already on RHS of screen?
	ret z
	inc a
	PutSpriteXAddr Sprite0,a
	ret
left:
	GetSpriteXAddr Sprite0
	cp 0			; already on LHS of screen?
	ret z
	dec a
	PutSpriteXAddr Sprite0,a
	ret	
up:
	GetSpriteYAddr Sprite0
	cp 0			; already at top of screen?
	ret z
	dec a
	PutSpriteYAddr Sprite0,a
	ret
down:
	GetSpriteYAddr Sprite0
	cp SCRN_Y-8		; already at bottom of screen?
	ret z
	inc a
	PutSpriteYAddr Sprite0,a
	ret
Yflip:
	ld a,[Sprite0Flags]
	xor OAMF_YFLIP	; toggle flip of sprite vertically
	ld [Sprite0Flags],a
	ret

; *hs* START
initdma:
	ld	de, DMACODELOC
	ld	hl, dmacode
	ld	bc, dmaend-dmacode
	call	mem_CopyVRAM			; copy when VRAM is available
	ret
dmacode:
	push af
	push bc
	push de
	push hl
	
	ld	a, OAMDATALOCBANK		; bank where OAM DATA is stored
	ldh	[rDMA], a			; Start DMA
	ld	a, $28				; 160ns
dma_wait:
	dec	a
	jr	nz, dma_wait
	
	ld a, 1				;yes, mister halt, this is vblank calling.
	ld [VBLANKED], a
	
	pop hl
	pop de
	pop bc
	pop af
	reti
dmaend:
; *hs* END

LCDC_STAT:
	push af
	push bc
	
	;if scanline >= Layer1Start
	
	;set scroll to 
	;ld a, [scroller]
	;dec a
	;ld [scroller], a
	;ld [rSCX], a
	
	pop bc
	pop af
	reti

; GetKeys: adapted from APOCNOW.ASM and gbspec.txt
GetKeys:                 ;gets keypress
	ld 	a,P1F_5			; set bit 5
	ld 	[rP1],a			; select P14 by setting it low. See gbspec.txt lines 1019-1095
	ld 	a,[rP1]
 	ld 	a,[rP1]			; wait a few cycles
	cpl				; complement A. "You are a very very nice Accumulator..."
	and 	$0f			; look at only the first 4 bits
	swap 	a			; move bits 3-0 into 7-4
	ld 	b,a			; and store in b

 	ld	a,P1F_4			; select P15
 	ld 	[rP1],a
	ld	a,[rP1]
	ld	a,[rP1]
	ld	a,[rP1]
	ld	a,[rP1]
	ld	a,[rP1]
	ld	a,[rP1]			; wait for the bouncing to stop
	cpl					; as before, complement...
 	and $0f				; and look only for the last 4 bits
 	or b				; combine with the previous result
 	ret					; do we need to reset joypad? (gbspec line 1082)

; ****************************************************************************************
; StopLCD:
; turn off LCD if it is on
; and wait until the LCD is off
; ****************************************************************************************
StopLCD:
        ld      a,[rLCDC]
        rlca                    ; Put the high bit of LCDC into the Carry flag
        ret     nc              ; Screen is off already. Exit.

; Loop until we are in VBlank

.wait:
        ld      a,[rLY]
        cp      145             ; Is display on scan line 145 yet?
        jr      nz,.wait        ; no, keep waiting

; Turn off the LCD

        ld      a,[rLCDC]
        res     7,a             ; Reset bit 7 of LCDC
        ld      [rLCDC],a

        ret
