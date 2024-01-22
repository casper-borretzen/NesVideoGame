.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 2
; Your goal here is to just store some values into zero-page memory positions.
.segment "CODE"
.org $8000
Reset: ; TODO:
    CLD
    LDA #$A         ; Load the A register with the hexadecimal value $A
    LDX #%11111111  ; Load the X register with the binary value %11111111
    STA $80        ; Store the value in the A register into memory address $80
    STX $81        ; Store the value in the X register into memory address $81
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything



.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
