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


.segment "BSS"
; we reserve one byte for storing the data that is read from controller
buttons1: .res 1   ; hold state of controller 1
buttons2: .res 1   ; hold state of controller 2

xvelocity: .res 1
yvelocity: .res 1

playerx: .res 1
playery: .res 1

enemyx: .res 1
enemyy: .res 1

oddframe: .res 1   ; toggles every other frame, used for advancing things such as animations only on every other frame

.segment "OAM"
sprites: .res 256

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


LoadPlayerSprites:
	LDX #$00              ; start at 0
LoadPlayerSpritesLoop:
	LDA playersprites, x        ; load data from address (sprites +  x)
	STA $0200, x          ; store into RAM address ($0200 + x)
	INX                   ; X = X + 1
	CPX #16               ; bytes of playersprite data
	BNE LoadPlayerSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						; reached end of playersprite data, keep going down

LoadMissleSprite:
	LDX #$00              ; start at 0
LoadMissleSpriteLoop:
	LDA misslesprite, x        ; load data from address (sprites +  x)
	STA $0210, x          ; store into RAM address ($0210 + x)
	INX                   ; X = X + 1
	CPX #4              ; bytes of misslesprite data
	BNE LoadMissleSpriteLoop   ; Branch to LoadMissleSpriteLoop if compare was Not Equal to zero
						; reached end of missleprite data, keep going down
MISSLE_X_ADDR = $0213
MISSLE_Y_ADDR = $0210

						
LoadEnemySprites:
	LDX #$00              ; start at 0
LoadEnemySpritesLoop:
	LDA enemysprites, x        ; load data from address (sprites +  x)
	STA $0214, x        ; store into RAM address ($0214 + x)
		
	INX                   
	CPX #160              ; bytes of enemy sprite data
	BNE LoadEnemySpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						; reached end of enemysprite data, keeep going down
				  
			  
; initialize player position
	PLAYER_STARTX = 120
	PLAYER_STARTY = 180
	
	LDA #PLAYER_STARTX
	STA playerx
	LDA #PLAYER_STARTY
	STA playery
	;initialize left most enemy of first row
	LDA #$3E
	STA enemyx
	LDA #$50
	STA enemyy
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
	STA $2000

	LDA #%10010000   ; enable sprites
	STA $2001

	jsr init_apu
	
forever:
	jmp forever
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main task - run on every vblank  
nmi:
	; DMA transfer sprite data from $0200 to the PPU SPR-RAM 
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer (DMA transfer 256 bytes)
	
	jsr readjoy
	jsr processinput
	jsr moveplayer
	jsr UpdatePlayerSprites
	jsr UpdateMissle
	jsr CheckMissleCollision
	jsr MoveEnemySprites
	jsr MoveEnemySpritesLoop
	
	LDA oddframe
	EOR #$01
	STA oddframe
	
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
MIN_X_VELOCITY = $FD
MAX_X_VELOCITY = $03

	
ReadLeft:
	lda buttons1
	AND #BUTTON_LEFT  
	BEQ ReadRight   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)
	LDA xvelocity
	SBC #$01
	CMP #MIN_X_VELOCITY
	BMI ReachMinVelocity
	JMP WriteVelocityLeft
ReachMinVelocity:
	LDA #MIN_X_VELOCITY
	STA xvelocity
	JMP ReadLeftRightDone
	
WriteVelocityLeft:		  
	STA xvelocity
	JMP ReadLeftRightDone

ReadRight:
	LDA buttons1      
	AND #BUTTON_RIGHT
	BEQ ReadLeftRightReleased   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)

	LDA xvelocity
	ADC #$01
	CMP #MAX_X_VELOCITY
	BPL ReachMaxVelocity
	JMP WriteVelocityRight
ReachMaxVelocity:
	LDA #MAX_X_VELOCITY
	STA xvelocity
	JMP ReadLeftRightDone
WriteVelocityRight:		  
	STA xvelocity
	JMP ReadLeftRightDone


ReadLeftRightReleased:
	; neither up or down was pressed, clear the y velocity
    LDA xvelocity
	CMP #$00
	BMI IncreaseVelocity
	BEQ ReadLeftRightDone
	BPL DecreaseVelocity
	JMP ReadLeftRightDone
	
DecreaseVelocity:
	SBC #$01
	JMP WriteVelocityChange
	
IncreaseVelocity:
	ADC #$01
WriteVelocityChange:
	STA xvelocity

ReadLeftRightDone:        ; done handling left/right input

ReadButtonA:
	lda buttons1
	AND #BUTTON_A
	BEQ ReadButtonADone   ; branch if button is NOT pressed (0)
				  ; add instructions here to do something when button IS pressed (1)
	LDA MISSLE_Y_ADDR
	CMP #$FF                  ; check if missle is currently hidden
	BNE ReadButtonADone       ; don't shoot if a missle is currenlty on screen
	
	LDA playery
	SEC
	SBC #$08   ;offset to above player sprite
	STA MISSLE_Y_ADDR
	
	LDA playerx
	CLC
	ADC #$04    ; offset to middle of player sprite
	STA MISSLE_X_ADDR
	
	; fire missle sound
    lda #$9F
    sta $4000
    lda #$13
    sta $4003

	
ReadButtonADone:


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

; check player x boundaries
	; check right boundary
	LDA playerx     
	CLC
	ADC xvelocity
	CMP #XPOSMAX
	BCS outboundx ; outside boundary
	
	; check left boundary
	CMP #XPOSMIN
	BCS updatex  ; within boundary

outboundx:
; clear velocity since we would be outside of the boundary
	LDA #$00    
	STA xvelocity
	
updatex:
; update X position of player sprite
	LDA playerx
	CLC
	ADC xvelocity
	STA playerx

; check player y boundaries
	; check bottom boundary
	LDA playery
	CLC
	ADC yvelocity
	CMP #YPOSMAX
	BCS outboundy ; outside boundary
	
	; check top boundary
	CMP #YPOSMIN
	BCS updatey  ; within boundary

outboundy:
; clear velocity since we would be outside of the boundary
	LDA #$00
	STA yvelocity

updatey:
; update Y position of player sprite
	LDA playery
	CLC
	ADC yvelocity
	STA playery

	rts  ; end of moveplayer
	
UpdateMissle:
	LDA MISSLE_Y_ADDR
	CMP #$FF   ;chekc if missle already hidden
	BNE CheckMissleTop
	RTS   ;missle is hidden, so return
CheckMissleTop:
	CMP #$02     ;check if missle hit top of screen
	BCS MoveMissle   
	LDA #$FF
	STA MISSLE_Y_ADDR ; hide the missle
	RTS
	
MoveMissle:
	SEC
	SBC #$02    ; move up screen
	
	STA MISSLE_Y_ADDR
	RTS
	
	
UpdatePlayerSprites:
	; update player sprite
	LDA playerx
	STA $0203       ; save sprite 1 X position
	STA $020B       ; save sprite 3 X position
	
	CLC
	ADC #$08        ; move to the right most sprite position
	
	STA $0207       ; save sprite 2 X position
	STA $020F       ; save sprite 4 X position
	
	LDA playery
	STA $0200       ; save sprite 1 Y position
	STA $0204       ; save sprite 2 Y position
	
	CLC
	ADC #$08        ; move to the bottom most sprite position
	
	STA $0208       ; save sprite 3 Y position
	STA $020C       ; save sprite 4 Y position
	
	
	rts
	
FIRST_ENEMY_Y_ADDR = $0214
FIRST_ENEMY_X_ADDR = $0217
FIRST_ENEMY_TILE_ADDR = FIRST_ENEMY_Y_ADDR + 1

ENEMY_SPRITE_DATA_LEN = $98

ENEMY_EXPLOSION_START_TILE = $08  ; tile index of first frame of enemy explosion
ENEMY_EXPLOSION_END_TILE = $0A
MoveEnemySprites:
	LDX $00             ; start at 0
	RTS
MoveEnemySpritesLoop:
	LDA FIRST_ENEMY_X_ADDR, x
	CLC
	ADC #$01
	STA FIRST_ENEMY_X_ADDR, x
	
	; check if this enemy is exploding and update animation
	LDA oddframe
	CMP #$01
	BEQ StepToNextEnemy   ; skip this animation step every other frame
	LDA FIRST_ENEMY_TILE_ADDR, x
	CMP #ENEMY_EXPLOSION_START_TILE
	BCC StepToNextEnemy
	CMP #ENEMY_EXPLOSION_END_TILE
	BCS DisableEnemy
	ADC #$01        ; increment the tile index for this frame of animation
	STA FIRST_ENEMY_TILE_ADDR, x  ; store new tile number in this enemy sprite
	JMP StepToNextEnemy

DisableEnemy:
	LDA #$FF
	STA FIRST_ENEMY_Y_ADDR, x   ; hide the enemy sprite
	
StepToNextEnemy:
	INX   
	INX
	INX
	INX
	CPX #ENEMY_SPRITE_DATA_LEN                   ; bytes of enemy sprite data
	BNE MoveEnemySpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						       ; reached end of enemysprite data, keeep going down
	RTS
	
CheckMissleCollision:
	LDA MISSLE_Y_ADDR
	CMP #$FF
	BEQ CheckMissleCollisionEnd
	
	LDX $00             ; start at 0
CheckMissleCollisionLoop:
; missle.x + missle.width < enemy.x ||
; enemy.x + enemy.width < missle.x ||
; missle.y + missle.height < enemy.y||
; enemy.y + enemy.height < missle.y
;  then no collision
	LDA MISSLE_X_ADDR
	ADC #$08
	CMP FIRST_ENEMY_X_ADDR, x
	BCC NoCollision
	LDA FIRST_ENEMY_X_ADDR, x
	ADC #$08
	CMP MISSLE_X_ADDR
	BCC NoCollision
	
	LDA MISSLE_Y_ADDR
	ADC #$08
	CMP FIRST_ENEMY_Y_ADDR, x
	BCC NoCollision
	LDA FIRST_ENEMY_Y_ADDR, x
	ADC #$08
	CMP MISSLE_Y_ADDR
	BCC NoCollision
	
	; found collision
	; TODO - check if this is still a valid enemy sprite and not an explosion

	LDA #$FF
	STA MISSLE_Y_ADDR   ;remove missle sprite
	LDA #ENEMY_EXPLOSION_START_TILE
	STA FIRST_ENEMY_TILE_ADDR, x  ; set enemy sprite to explosion animation
	
	; explosding enemy sound
	lda #$9F
    sta $400C
    lda #$22
    sta $400E
	lda #$80
	sta $400F
	
NoCollision:
	INX   
	INX
	INX
	INX	
	CPX #ENEMY_SPRITE_DATA_LEN                   ; bytes of enemy sprite data
	BNE CheckMissleCollisionLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						       ; reached end of enemysprite data, keeep going down

CheckMissleCollisionEnd:
    RTS

init_apu:
        ; Init $4000-4013
        ldy #$13
init_apu_loop:  lda init_apu_regs,y
        sta $4000,y
        dey
        bpl init_apu_loop
 
        ; We have to skip over $4014 (OAMDMA)
        lda #$0f
        sta $4015
        lda #$40
        sta $4017
   
        rts
init_apu_regs:
        .byte $30,$08,$00,$00
        .byte $30,$08,$00,$00
        .byte $80,$00,$00,$00
        .byte $30,$00,$00,$00
        .byte $00,$00,$00,$00
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constant data		
palette:
  .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  .byte $0F,$1C,$15,$14,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

playersprites:
  ;vert tile attr horiz
  ;player object
  .byte $A0, $00, $00, $20   ;sprite 0
  .byte $A0, $00, $00, $28   ;sprite 1
  .byte $A8, $00, $00, $20   ;sprite 2
  .byte $A8, $00, $00, $28   ;sprite 3

misslesprite:
  .byte $FF, $07, $00, $00  ;missle  (Y = FF so hidden at start)
  
enemysprites:
  .byte $50, $01, $00, $3E   ;enemy type 1
  .byte $50, $01, $00, $4E   ;enemy type 1
  .byte $50, $01, $00, $5E   ;enemy type 1
  .byte $50, $01, $00, $6E   ;enemy type 1
  .byte $50, $01, $00, $7E   ;enemy type 1
  .byte $50, $01, $00, $8E   ;enemy type 1
  .byte $50, $01, $00, $9E   ;enemy type 1
  .byte $50, $01, $00, $AE   ;enemy type 1

  .byte $40, $03, $00, $46   ;enemy type 2
  .byte $40, $03, $00, $56   ;enemy type 2
  .byte $40, $03, $00, $66   ;enemy type 2
  .byte $40, $03, $00, $76   ;enemy type 2
  .byte $40, $03, $00, $86   ;enemy type 2
  .byte $40, $03, $00, $96   ;enemy type 2
  .byte $40, $03, $00, $A6   ;enemy type 2

  .byte $30, $02, $00, $3E   ;enemy type 3
  .byte $30, $02, $00, $4E   ;enemy type 3
  .byte $30, $02, $00, $5E   ;enemy type 3
  .byte $30, $02, $00, $6E   ;enemy type 3
  .byte $30, $02, $00, $7E   ;enemy type 3
  .byte $30, $02, $00, $8E   ;enemy type 3
  .byte $30, $02, $00, $9E   ;enemy type 3
  .byte $30, $02, $00, $AE   ;enemy type 3

  .byte $20, $04, $00, $46   ;enemy type 4
  .byte $20, $04, $00, $56   ;enemy type 4
  .byte $20, $04, $00, $66   ;enemy type 4
  .byte $20, $04, $00, $76   ;enemy type 4
  .byte $20, $04, $00, $86   ;enemy type 4
  .byte $20, $04, $00, $96   ;enemy type 4
  .byte $20, $04, $00, $A6   ;enemy type 4

  .byte $10, $05, $00, $3E   ;enemy type 5
  .byte $10, $05, $00, $4E   ;enemy type 5
  .byte $10, $05, $00, $5E   ;enemy type 5
  .byte $10, $05, $00, $6E   ;enemy type 5
  .byte $10, $05, $00, $7E   ;enemy type 5
  .byte $10, $05, $00, $8E   ;enemy type 5
  .byte $10, $05, $00, $9E   ;enemy type 5
  .byte $10, $05, $00, $AE   ;enemy type 5


.segment "CHARS"
    .incbin "office_space_invaders.chr" ; include chr data file
	
	
