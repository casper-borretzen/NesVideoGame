.include "consts.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables in Zero Page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"
Buttons: .res 1               ; Reserve 1 byte to store button state (A|B|SELECT|START|UP|DOWN|LEFT|RIGHT)
XPos:    .res 2               ; Player X position (8.8 fixed-point math) - Xhi + Xlo/256
YPos:    .res 2               ; Player Y position (8.8 fixed-point math) - Yhi + Ylo/256
XVel:    .res 1               ; Player X speed in pixels per 256 frames
YVel:    .res 1               ; Player Y speed in pixels per 256 frames
Frame:   .res 1               ; Reserve 1 byte to store the number of frames
Clock60: .res 1               ; Reserve 1 byte to store a counter that increments every second (60 frames)
BgPtr:   .res 2               ; Reserve 2 bytes (16 bits) to store a pointer to the background address (we store first the lo-byte, and immediately after, the hi-byte) - little endian

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for player movement
;; PS: PAL frames runs ~20% slower than NTSC frames. Adjust accordingly!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAXSPEED = 120                ; Max speed limit in 1/256 px/frame
ACCEL    = 2                  ; Movement acceleration in 1/256 px/frame^2
BRAKE    = 2                  ; Stopping acceleration in 1/256 px/frame^2

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
;; Routine to read controller state and store bits inside "Buttons"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ReadControllers
    lda #1                    ; A = 1
    sta Buttons               ; Buttons = 1
    sta JOYPAD1               ; Set controller latch = 1 to begin input mode
    lsr                       ; A = 0
    sta JOYPAD1               ; Set controller latch = 0 to begin output mode
LoopButtons:
    lda JOYPAD1               ; 1. Reads a bit from the controller data line and inverts its value
                              ; 2. Sends a signal to the Clock line to shift the bits inside the controller
    lsr                       ; We shift-right to place that 1-bit we just read into the Carry flag
    rol Buttons               ; Roll bits left, placing the Carry value into the 1st bit of Buttons
    bcc LoopButtons           ; Loop until Carry is set (from that initial 1 we had inside Buttons)
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESET:
    INIT_NES

InitVariables:
    lda #0
    sta Frame                 ; Initialize the Frame variable
    sta Clock60               ; Initialize the Clock60 variable
    
    lda #20
    sta XVel

    ldx #0
    lda SpriteData,x
    sta YPos+1                ; Initialize the Y position hi-byte
    inx
    inx
    inx
    lda SpriteData,x
    sta XPos                  ; Initialize the X position hi-byte

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

OAMStartDMACopy:
    lda #$02                  ; Copy sprite data starting at $02**
    sta PPU_OAM_DMA           ; The OAM DMA copy starts when we write to $4014

ControllerInput:
    jsr ReadControllers       ; Jump to subroutine

CheckRightButton:
    lda Buttons
    and #BUTTON_RIGHT
    beq NotRight
      lda XVel
      clc
      adc #ACCEL              ; Add the acceleration to the velocity
      cmp #MAXSPEED           ; Check if we've reached MAXSPEED
      bcc :+
        lda #MAXSPEED         ; Clamp the maximum speed
      :
      sta XVel                ; Save the new updated velocity
      jmp CheckLeftButton
    NotRight:
      lda XVel                
      cmp #BRAKE              ; Check if we can subtract from the velocity
      bcs :+
        lda #BRAKE+1          ; Force it to be the BRAKE (+1 to compensate for the carry)
      :
      sbc #BRAKE              ; Subtracting the brake from the velocity
      sta XVel                ; Save the new updated velocity
CheckLeftButton:
    lda Buttons
    and #BUTTON_LEFT
    beq CheckDownButton
      dec XPos
CheckDownButton:
    lda Buttons
    and #BUTTON_DOWN
    beq CheckUpButton
      inc YPos
CheckUpButton:
    lda Buttons
    and #BUTTON_UP
    beq :+
      dec YPos
;CheckSelectButton:
;    lda Buttons
;    and #%00010000
;    beq CheckStartButton
;CheckStartButton:
;    lda Buttons
;    and #%00100000
;    beq CheckBButton
;CheckBButton:
;    lda Buttons
;    and #%01000000
;    beq CheckAButton
;CheckAButton:
;    lda Buttons
;    and #%10000000
:
UpdateSpritePosition:
    lda XVel
    clc
    adc XPos                  ; Add the velocity to the X position lo-byte
    sta XPos
    lda #0
    adc XPos+1                ; Add the hi-byte (using the carry of the previous add)
    sta XPos+1

DrawSpriteTile:
    ldy #0
    lda XPos+1
    sta $0203
    sta $020B
    clc
    adc #8
    sta $0207
    sta $020F
    lda YPos+1
    sta $0200
    sta $0204
    clc
    adc #8
    sta $0208
    sta $020C

    lda Frame
    cmp #60                   ; Compare frame with #60
    bne Skip                  ; If not 60 then bypass
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
.byte $1D,$10,$20,$2D, $1D,$1D,$2D,$10, $1D,$0C,$19,$1D, $1D,$06,$17,$07 ; Background palette
.byte $0F,$1D,$19,$29, $0F,$08,$18,$38, $0F,$0C,$1C,$3C, $0F,$2D,$10,$30 ; Sprite palette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background data to be copied to the nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BackgroundData:
.incbin "background2.nam"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OAM sprite attribute data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SpriteData:

; Metasprite #1
;      Y   tile#   attribs    X
.byte $80,  $18,  %00000000, $10 ; OAM sprite 1
.byte $80,  $1A,  %00000000, $18 ; OAM sprite 2
.byte $88,  $19,  %00000000, $10 ; OAM sprite 3
.byte $88,  $1B,  %00000000, $18 ; OAM sprite 4

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
.incbin "tiles2.chr"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                     ; Address (2-bytes) of the NMI handler
.word RESET                   ; Address (2-bytes) of the Reset handler
.word IRQ                     ; Address (2-bytes) of the IRQ handler
