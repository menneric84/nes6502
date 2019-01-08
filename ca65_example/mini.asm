.segment "CHARS"
    ;.incbin "chr.bin" ; if you have one
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
.segment "STARTUP" ; avoids warning
.segment "CODE"

nmi:
irq:
reset:
    lda #$01 ; play short tone
    sta $4015
    lda #$9F
    sta $4000
    lda #$22
    sta $4003
forever:
    jmp forever