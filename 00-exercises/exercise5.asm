.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 5
; The ADC and SBC instructions can also be used with different addressing modes. The
; above exercise used ADC with immediate mode (adding a literal value directly into the
; accumulator), but we can also ask ADC to add a value from a (zero page) memory
; position into the accumulator.
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDA #$A        ; Load the A register with the hexadecimal value $A
    LDX #%1010  ; Load the X register with the binary value %1010
    STA $80        ; Store the value in the A register into (zero page) memory address $80
    STX $81        ; Store the value in the X register into (zero page) memory address $81
    LDA #10        ; Load A with the decimal value 10
    CLC
    ADC $80        ; Add to A the value inside RAM address $80
    ADC $81        ; Add to A the value inside RAM address $81
    ; A should contain (#10 + $A + %1010) = #30 (or $1E in hexadecimal)
    STA $82        ; Store the value of A into RAM position $82
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
