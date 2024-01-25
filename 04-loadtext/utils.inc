.macro PPU_SETADDR addr
    bit PPU_STATUS            ; Read from PPU_STATUS to reset PPU_ADDR latch
    lda #>addr                ; Fetch the hi-byte from the 16-bit address
    sta PPU_ADDR              ; Set hi-byte of PPU_ADDR $2006
    lda #<addr                ; Fetch the lo-byte from the 16-bit address
    sta PPU_ADDR              ; Set lo-byte of PPU_ADDR $2006
.endmacro

.macro PPU_SETDATA val
    lda val
    sta PPU_DATA              ; Send value to PPU register $2007
.endmacro
