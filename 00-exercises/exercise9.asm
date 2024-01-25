.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 9
; Your goal in this exercise is to create a simple loop that goes from 1 to 10. If possible, try
; using the CMP instruction. This instruction that can be used to compare the value of
; the accumulator with a certain literal number. Once the comparison is done, the
; processor flags will be set (zero if the compared values are equal, non-zero if different).
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDA #1         ; Initialize the A register with 1
Loop:
    CLC
    ADC #1         ; Increment A
    CMP #10        ; Compare the value in A with the decimal value 10
    BNE Loop       ; Branch back to "Loop" if the comparison was not equals (to zero)
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
