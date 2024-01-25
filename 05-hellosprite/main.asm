.include "consts.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables in Zero Page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"
Frame:   .res 1               ; Reserve 1 byte to store the number of frames
Clock60: .res 1               ; Reserve 1 byte to store a counter that increments every second (60 frames)
BgPtr:   .res 2               ; Reserve 2 bytes (16 bits) to store a pointer to the background address
                              ; (we store first the lo-byte, and immediately after, the hi-byte) - little endian
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPalette            
    PPU_SETADDR $3F00
    ldy #0                    ; Y = 0
:   lda PaletteData,y         ; Load A with PaletteData color byte
    sta PPU_DATA              ; Set value to send to PPU_DATA (that auto increments)
    iny                       ; Y++
    cpy #32                   ; Is Y equal to 32?
    bne :-                    ; Keep looping if not equal
    rts                       ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 16 bytes into OAM-RAM starting at $0200
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadSprites         
    ldx #0                    ; Y = 0
:   lda SpriteData,x          ; Load A with PaletteData color byte
    sta $0200,x               ; Set value to send to PPU_DATA (that auto increments)
    inx                       ; Y++
    cpx #32                   ; Is Y equal to 32?
    bne :-                    ; Keep looping if not equal
    rts                       ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load tiles and attributes into the first nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadBackground        
    lda #<BackgroundData      ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData      ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1
    PPU_SETADDR $2000         ; Set PPU address to the start of first nametable at $2000
    ldx #$00
    ldy #$00
OuterLoop:
InnerLoop:
    lda (BgPtr),y             ; Fetch the value *pointed* by (BgPtr) + Y offset
    sta PPU_DATA              ; Store value in PPU_DATA
    iny                       ; Y++
    cpy #0                    ; If Y == 0 (roll-off after 256 loops through all lo-bytes)
    beq IncreaseHiByte        ;    Then: continue to next hi-byte
    jmp InnerLoop             ;    Else: continue with the inner loop, looping through lo-bytes
IncreaseHiByte:
    inc BgPtr+1               ; Increment the hi-byte pointer to point to the next background section
    inx                       ; X++
    cpx #4                    ; Compare X with #4
    bne OuterLoop             ; If X is still not 4, then we keep looping back to the outer loop
    rts                       ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESET:
    INIT_NES
    lda #0
    sta Frame                 ; Initialize the Frame variable
    sta Clock60               ; Initialize the Clock60 variable

Main:
    jsr LoadPalette           ; Jump to subroutine
    jsr LoadBackground        ; Jump to subroutine
    jsr LoadSprites           ; Jump to subroutine

EnablePPURendering:
    lda #%10010000            ; Enable NMI and set background to use the 2nd pattern table
    sta PPU_CTRL
    lda #0
    sta PPU_SCROLL            ; Disable scroll in X
    sta PPU_SCROLL            ; Disable scroll in Y
    lda #%00011110
    sta PPU_MASK              ; Set PPU_MASK bits to show background and sprites

LoopForever:
    jmp LoopForever           ; Force infinite loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler (VBLANK)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    inc Frame                 ; Frame++
    
    ; Update PPU OAM:
    ;-------------------------
    lda #$02                  ; Copy sprite data starting at $02**
    sta $4014                 ; The OAM DMA copy starts when we write to $4014
    ;-------------------------
    
    lda Frame
    cmp #60                   ; Compare frame with #60
    bne Skip                    ; If not 60 then bypass
    inc Clock60               ; Else, increment Clock60 and zero the Frame counter
    lda #0
    sta Frame
Skip:
    rti                       ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                       ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hardcoded list of color values in ROM to be loaded by the PPU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaletteData:
.byte $22,$29,$1A,$0F, $22,$36,$17,$0F, $22,$30,$21,$0F, $22,$27,$17,$0F ; Background palette
.byte $22,$16,$27,$18, $22,$1A,$30,$27, $22,$16,$30,$27, $22,$0F,$36,$17 ; Sprite palette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background data to be copied to the nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BackgroundData:
.incbin "background.nam"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OAM sprite attribute data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SpriteData:

; Metasprite #1
;      Y   tile#   attribs    X
.byte $6F,  $3A,  %00000000, $70 ; OAM sprite 1
.byte $6F,  $37,  %00000000, $78 ; OAM sprite 2
.byte $77,  $4F,  %00000000, $70 ; OAM sprite 3
.byte $77,  $4F,  %01000000, $78 ; OAM sprite 4

; Metasprite #2
;      Y   tile#   attribs    X
.byte $6F,  $70,  %00000011, $A0 ; OAM sprite 5
.byte $6F,  $71,  %00000011, $A8 ; OAM sprite 6
.byte $77,  $72,  %00000011, $A0 ; OAM sprite 7
.byte $77,  $73,  %00000011, $A8 ; OAM sprite 8

; Sprite Attribute Byte:
;-----------------------
;  76543210
;  |||   ||
;  |||   ++- Color palette of sprite.
;  |||
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add CHR-ROM data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CHARS"
.incbin "tiles.chr"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                     ; Address (2-bytes) of the NMI handler
.word RESET                   ; Address (2-bytes) of the Reset handler
.word IRQ                     ; Address (2-bytes) of the IRQ handler
