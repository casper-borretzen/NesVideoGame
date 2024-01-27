;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to set a 16-bit address to the PPU_ADDR register (at $2006)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETADDR addr
    bit PPU_STATUS           ; Read from PPU_STATUS to reset the address latch
    lda #>addr               ; Fetch the hi-byte from 16-bit address
    sta PPU_ADDR             ; Store the hi-byte into PPU_ADDR $2006
    lda #<addr               ; Fetch the lo-byte from 16-bit address
    sta PPU_ADDR             ; Store the lo-byte into PPU_ADDR $2006
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to send a value to the PPU_DATA (at $2007)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETDATA val
    lda val
    sta PPU_DATA             ; Send value to PPU register at $2007
.endmacro