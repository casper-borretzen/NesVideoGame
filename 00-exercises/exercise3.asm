.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 3
; This exercise is about transferring values from registers to other registers.
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDA #15        ; Load the A register with the literal decimal value 15
    TAX            ; Transfer the value from A to X
    TAY            ; Transfer the value from A to Y
    TXA            ; Transfer the value from X to A
    TYA            ; Transfer the value from Y to A
    LDX #6         ; Load X with the decimal value 6    
    ; Transfer the value from X to Y
    ; X->A A->Y
    TXA
    TAY
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
