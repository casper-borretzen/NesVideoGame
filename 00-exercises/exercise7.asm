.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 7
; This exercise covers the increment and decrement using zero-page addressing mode.
; The zero-page addressing mode helps us directly increment and decrement values
; inside memory positions. The “zero page” in the 6502 are addresses between 0 and 255.
; These addresses are special for the 6502 processor because we can store them using
; only 1 byte (8 bits), which also means they can be performed relatively fast by the CPU.
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDA #10        ; Load the A register with the decimal value 10
    STA $80        ; Store the value from A into memory position $80
    INC $80        ; Increment the value inside a (zero page) memory position $80
    DEC $80        ; Decrement the value inside a (zero page) memory position $80
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
