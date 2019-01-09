.segment "HEADER"
	.byte "NES",$1a   ;iNES header
	.byte $02         ;# of PRG-ROM blocks. These are 16kb each. $4000 hex.
	.byte $01         ;# of CHR-ROM blocks. These are 8kb each. $2000 hex.
	.byte $01         ;Vertical mirroring. SRAM disabled. No trainer. Four-screen mirroring disabled. Mapper #0 (NROM)
	.byte $00         ;Rest of NROM bits (all 0)

.segment "VECTORS"
	.word nmi
	.word reset
	.word irq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
.segment "ZEROPAGE"

; we reserve one byte for storing the data that is read from controller
buttons1: .res 1   ; hold state of controller 1
buttons2: .res 1   ; hold state of controller 2

xvelocity: .res 1
yvelocity: .res 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "STARTUP" ; avoids warning
.segment "CODE"
reset:
	sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    inx        ; now X = 0
    stx $2000  ; disable NMI
    stx $2001  ; disable rendering
    stx $4010  ; disable DMC IRQs

    ; Optional (omitted):
    ; Set up mapper and jmp to further init code here.

    ; If the user presses Reset during vblank, the PPU may reset
    ; with the vblank flag still true.  This has about a 1 in 13
    ; chance of happening on NTSC or 2 in 9 on PAL.  Clear the
    ; flag now so the @vblankwait1 loop sees an actual vblank.
    bit $2002

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblankwait1:  
    bit $2002
    bpl @vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    txa
@clrmem:
    sta $000,x
    sta $100,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x  ; Remove this if you're storing reset-persistent data

    ; We skipped $200,x on purpose.  Usually, RAM page 2 is used for the
    ; display list to be copied to OAM.  OAM needs to be initialized to
    ; $EF-$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).

    inx
    bne @clrmem

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
   
@vblankwait2:
    bit $2002
    bpl @vblankwait2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
LoadPalettes:
	LDA $2002             ; read PPU status to reset the high/low latch
	LDA #$3F
	STA $2006             ; write the high byte of $3F00 address
	LDA #$00
	STA $2006             ; write the low byte of $3F00 address
	LDX #$00              ; start out at 0
LoadPalettesLoop:
	LDA palette, x        ; load data from address (palette + the value in x)
						  ; 1st time through loop it will load palette+0
						  ; 2nd time through loop it will load palette+1
						  ; 3rd time through loop it will load palette+2
						  ; etc
	STA $2007             ; write to PPU
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
	BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
						; if compare was equal to 32, keep going down


LoadSprites:
	LDX #$00              ; start at 0
LoadSpritesLoop:
	LDA sprites, x        ; load data from address (sprites +  x)
	STA $0200, x          ; store into RAM address ($0200 + x)
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $20, decimal 32
	BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						; if compare was equal to 32, keep going down
			  
			  

	LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
	STA $2000

	LDA #%10010000   ; enable sprites
	STA $2001

forever:
	jmp forever
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main task - run on every vblank  
nmi:
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer
	jsr readjoy
	jsr processinput
	jsr moveplayer
	
	rti  ; return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
irq:
	rti
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYPAD1 = $4016
JOYPAD2 = $4017
; read the controllers and store the button states in buttonsx vars
readjoy:
    lda #$01
    sta JOYPAD1
    sta buttons2  ; player 2's buttons double as a ring counter
    lsr a         ; now A is 0
    sta JOYPAD1
loop:
    lda JOYPAD1
    and #%00000011  ; ignore bits other than controller
    cmp #$01        ; Set carry if and only if nonzero
    rol buttons1    ; Carry -> bit 0; bit 7 -> Carry
    lda JOYPAD2     ; Repeat
    and #%00000011
    cmp #$01
    rol buttons2    ; Carry -> bit 0; bit 7 -> Carry
    bcc loop
    rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0


processinput:
; read the controller button status and update the player movement velocities

ReadUp:
	lda buttons1
	AND #BUTTON_UP  
	BEQ ReadDown   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)
	LDA #$FF		  
	STA yvelocity
	JMP ReadUpDownDone

ReadDown:
	LDA buttons1      
	AND #BUTTON_DOWN 
	BEQ ReadUpDownReleased   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)

	LDA #$01			  
	STA yvelocity
	JMP ReadUpDownDone

ReadUpDownReleased:
	; neither up or down was pressed, clear the y velocity
    LDA #$00
	STA yvelocity

ReadUpDownDone:        ; done handling up/down input

	
ReadLeft:
	lda buttons1
	AND #BUTTON_LEFT  
	BEQ ReadRight   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)
	LDA #$FF		  
	STA xvelocity
	JMP ReadLeftRightDone

ReadRight:
	LDA buttons1      
	AND #BUTTON_RIGHT
	BEQ ReadLeftRightReleased   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)

	LDA #$01			  
	STA xvelocity
	JMP ReadLeftRightDone

ReadLeftRightReleased:
	; neither up or down was pressed, clear the y velocity
    LDA #$00
	STA xvelocity

ReadLeftRightDone:        ; done handling left/right input

	rts   ; end of processinput
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
moveplayer:	
; read the controller button status and move player sprites accordingly

; for screen boundaries these are based on position of top left sprite in player
; this boundary check doesn't check for overflow so it assumes velocity is kept less than 
;  the boundary size so the player movement can't jump through the boundary to the other side
;  e.g.  for a boundary of 10 from 0 or 256, the max velocity needs to be kept below 10
XPOSMIN = 10
XPOSMAX = 230
YPOSMIN = 10
YPOSMAX = 190

	; check right boundary
	LDA $0203
	CLC
	ADC xvelocity
	CMP #XPOSMAX
	BCS outboundx ; outside boundary
	
	; check left boundary
	CMP #XPOSMIN
	BCS updatex  ; within boundary

outboundx:
	LDA #$00
	STA xvelocity
	
updatex:
; update X position of player sprite
	
	LDA $0203       ; load sprite X position
	CLC             ; make sure the carry flag is clear
	ADC xvelocity       
	STA $0203       ; save sprite X position

	LDA $0207 ; load sprite 2 position 
	CLC ; make sure carry flag is set 
	ADC xvelocity
	STA $0207 ; save sprite 2 position

	LDA $020B ; load sprite 3 position 
	CLC ; make sure carry flag is set 
	ADC xvelocity
	STA $020B ; save sprite 3 position 

	LDA $020F ; load sprite 4 position 
	CLC ; make sure carry flag is set 
	ADC xvelocity 
	STA $020F ; save sprite 4 position 


	; check bottom boundary
	LDA $0200
	CLC
	ADC yvelocity
	CMP #YPOSMAX
	BCS outboundy ; outside boundary
	
	; check top boundary
	CMP #YPOSMIN
	BCS updatey  ; within boundary

outboundy:
	LDA #$00
	STA yvelocity

updatey:
; update Y position of player sprite
	LDA $0200       ; load sprite Y position
	CLC             ; make sure the carry flag is clear
	ADC yvelocity
	STA $0200       ; save sprite Y position

	LDA $0204 ; load sprite 2 position 
	CLC ; make sure carry flag is set 
	ADC yvelocity
	STA $0204 ; save sprite 2 position

	LDA $0208 ; load sprite 3 position 
	CLC ; make sure carry flag is set 
	ADC yvelocity
	STA $0208 ; save sprite 3 position 

	LDA $020C ; load sprite 4 position 
	CLC ; make sure carry flag is set 
	ADC yvelocity
	STA $020C ; save sprite 4 position 
	
	rts  ; end of moveplayer
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constant data		
palette:
  .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  .byte $0F,$1C,$15,$14,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .byte $A0, $05, $00, $10   ;sprite 0
  .byte $A0, $06, $00, $18   ;sprite 1
  .byte $A8, $15, $00, $10   ;sprite 2
  .byte $A8, $16, $00, $18   ;sprite 3

  .byte $50, $00, $00, $80   ;sprite 0
  .byte $50, $01, $00, $88   ;sprite 1
  .byte $58, $10, $00, $80   ;sprite 2
  .byte $58, $11, $00, $88   ;sprite 3

	
.segment "CHARS"
    .incbin "char.chr" ; include chr data file
	
	
