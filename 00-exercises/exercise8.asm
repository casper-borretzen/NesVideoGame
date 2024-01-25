.segment "HEADER"  ; Don’t forget to always add the iNES header to your ROM files
.org $7FF0
.byte $4E,$45,$53,$1A,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Exercise 8
; Your goal here is to create a loop that counts down from 10 to 0. You should also fill the
; memory addresses from $80 to $8A with values from 0 to A
; 
; Memory address:  $80  $81  $82  $83  $84  $85  $86  $87  $88  $89  $8A
;          Value:  0    1    2    3    4    5    6    7    8    9    A
.segment "CODE"    ; Define a segment called "CODE" for the PRG-ROM at $8000
.org $8000
Reset:
    CLD
    LDY #10        ; Initialize the Y register with the decimal value 10
Loop:
    TYA            ; Transfer Y to A
    STA $80,Y      ; Store the value in A inside memory position $80+Y
    DEY            ; Decrement Y
    BPL Loop       ; Branch back to "Loop" until we are done
NMI:               ; NMI handler
    rti            ; doesn't do anything
IRQ:               ; IRQ handler
    rti            ; doesn't do anything

.segment "VECTORS" ; Add addresses with vectors at $FFFA
.org $FFFA
.word NMI          ; Put 2 bytes with the NMI address at memory position $FFFA
.word Reset        ; Put 2 bytes with the break address at memory position $FFFC
.word IRQ          ; Put 2 bytes with the IRQ address at memory position $FFFE
