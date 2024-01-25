.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 4
; This exercise is about adding and subtracting values. Adding and subtracting are math
; operations that are done by the processor ALU (arithmetic-logic-unit). Since the ALU
; can only manipulate values from the (A)ccumulator, all these additions and subtractions
; must be performed with the values in the A register.
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDA #100       ; Load the A register with the literal decimal value 100
    CLC
    ADC #5         ; Add the decimal value 5 to the accumulator
    SEC
    SBC #10        ; Subtract the decimal value 10 from the accumulator
    ; Register A should now contain the decimal 95 (or $5F in hexadecimal)i

NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
