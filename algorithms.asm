Mul8b:					; this routine performs the operation HL=H*E
	ld d, 0				; clearing D and L
	ld l, d
	ld b, 8				; we have 8 bits
Mul8bLoop:
	add hl, hl			; advancing a bit
	jp nc, Mul8bSkip	; if zero, we skip the addition (jp is used for speed)
	add hl, de			; adding to the product if necessary
Mul8bSkip:
	dec b
	jp nz, Mul8bLoop
	ret

Mul16:					; This routine performs the operation DEHL=BC*DE
	ld hl, 0
	ld a, 16
Mul16Loop:
	add hl, hl
	rl e
	rl d
	jp nc, NoMul16
	add hl, bc
	jp nc, NoMul16
	inc de				; This instruction (with the jump) is like an "ADC DE,0"
NoMul16:
	dec a
	jp nz, Mul16Loop
	ret

;Div8:				; this routine performs the operation HL=HL/D
;	xor a			; clearing the upper 8 bits of AHL
;	ld b,16			; the length of the dividend (16 bits)
;Div8Loop:
;	add hl,hl		; advancing a bit
;	rla
;	cp d			; checking if the divisor divides the digits chosen (in A)
;	jp c,Div8NextBit; if not, advancing without subtraction
;	sub d			; subtracting the divisor
;	inc l			; and setting the next digit of the quotient
;Div8NextBit:
;	djnz Div8Loop
;	ret

; GetKeys: adapted from APOCNOW.ASM and gbspec.txt
GetKeys:			; gets keypress
	ld a, P1F_5		; set bit 5
	ld [rP1], a		; select P14 by setting it low
	ld a, [rP1]
 	ld a, [rP1]		; wait a few cycles
	cpl				; complement A. "You are a very very nice Accumulator..."
	and $0f			; look at only the first 4 bits
	swap a			; move bits 3-0 into 7-4
	ld b,a			; and store in b

 	ld a, P1F_4		; select P15
 	ld [rP1], a
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]
	ld a, [rP1]		; wait for the bouncing to stop
	cpl				; as before, complement...
	and $0f			; and look only for the last 4 bits
	or b			; combine with the previous result
	ret				; do we need to reset joypad? (gbspec line 1082)

; StopLCD:
; turn off LCD if it is on and wait until the LCD is off
StopLCD:
	ld a,[rLCDC]
	rlca			; Put the high bit of LCDC into the Carry flag
	ret nc			; Screen is off already. Exit.
.wait:				; Loop until we are in VBlank
	ld a, [rLY]
	cp 145			; Is display on scan line 145 yet?
	jr nz, .wait	; no, keep waiting
	ld a, [rLCDC]	; Turn off the LCD
	res 7, a		; Reset bit 7 of LCDC
	ld [rLCDC], a
	ret

; *hs* START
initdma:
	ld de, DMACODELOC
	ld hl, dmacode
	ld bc, dmaend-dmacode
	call mem_CopyVRAM	; copy when VRAM is available
	ret
dmacode:
	push af
	push bc
	push de
	push hl

	ld a, OAMDATALOCBANK	; bank where OAM DATA is stored
	ldh [rDMA], a			; Start DMA
	ld a, $28				; 160ns
dma_wait:
	dec a
	jr nz, dma_wait

	ld a, 1				; yes, mister halt, this is vblank calling.
	ld [VBLANKED], a

	pop hl
	pop de
	pop bc
	pop af
	reti
dmaend:
; *hs* END
