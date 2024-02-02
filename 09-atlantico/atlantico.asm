.include "consts.inc"
.include "header.inc"
.include "actor.inc"
.include "reset.inc"
.include "utils.inc"
.include "state.inc"

.segment "ZEROPAGE"

ActorsArray:    .res MAX_ACTORS * .sizeof(Actor)

MenuItem:       .res 1       ; Keep track of the menu item that is selected

GameState:      .res 1       ; Keep track of game state

Buttons:        .res 1       ; Pressed buttons (A|B|Select|Start|Up|Dwn|Lft|Rgt)
PrevButtons:    .res 1       ; Previously pressed buttons

XPos:           .res 1       ; Player X position
YPos:           .res 1       ; Player Y position

PrevSubmarine:  .res 1       ; Time in seconds since last time a submarine was spawned
PrevAirplane:   .res 1       ; Time in seconds since last time an airplane was spawned

Frame:          .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete: .res 1       ; Flag to indicate when VBlank is done drawing
Clock60:        .res 1       ; Counter that increments per second (60 frames)

BgPtr:          .res 2       ; Pointer to background address - 16bits (lo,hi)
SprPtr:         .res 2       ; Pointer to the sprite address - 16bits (lo,hi)
BufPtr:         .res 2       ; Pointer to the buffer address - 16bits (lo,hi)
PalPtr:         .res 2       ; Pointer to the palette address

XScroll:        .res 1       ; Store the horizontal scroll position
CurrNametable:  .res 1       ; Store the current starting nametable (0 or 1)
Column:         .res 1       ; Stores the column (of tiles) we are in the level
NewColAddr:     .res 2       ; The destination address of the new column in PPU
SourceAddr:     .res 2       ; The source address in ROM of the new column tiles

ParamType:      .res 1
ParamXPos:      .res 1
ParamYPos:      .res 1
ParamTileNum:   .res 1
ParamNumTiles:  .res 1
ParamAttribs:   .res 1
ParamRectX1:    .res 1
ParamRectX2:    .res 1
ParamRectY1:    .res 1
ParamRectY2:    .res 1
ParamScreen:    .res 1

PrevOAMCount:   .res 1       ; Store the previous number of bytes that were sent to the OAM

Seed:           .res 2       ; Initialize 16-bit seed to any value except 0

Collision:      .res 1       ; Flag to indicate if collision happened or not

Score:          .res 4       ; Score (1s, 10s, 100, and 1000s digits in decimal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FamiStudio audio engine configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.define FAMISTUDIO_CA65_ZP_SEGMENT   ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT  RAM
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE

FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_USE_RELEASE_NOTES  = 1
FAMISTUDIO_DPCM_OFF           = $E000

.include "audioengine.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to increment the scole value simulating BCD mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IncrementScore
    Increment1sDigit:
      lda Score+0            ; Load the lowest digit of the number
      clc
      adc #1                 ; Add 1
      sta Score+0
      cmp #$A                ; Check for overflow
      bne DoneIncrementing   ; If no overflow happened, we're done
    Increment10sDigit:
      lda #0
      sta Score+0            ; Reset one's digit from 9 to 0
      lda Score+1            ; Load second digit
      clc
      adc #1                 ; Add 1 (the carry from the previous digit)
      sta Score+1            
      cmp #$A                ; Check for overflow
      bne DoneIncrementing   ; If no overflow happened, we're done
    Increment100sDigit: 
      lda #0
      sta Score+1            ; Reset ten's digit from 9 to 0
      lda Score+2            ; Load the third digit
      clc
      adc #1                 ; Add 1 (the carry from the previous digit)
      sta Score+2
      cmp #$A                ; Check for overflow
      bne DoneIncrementing   ; If no overflow happened, we're done
    Increment1000sDigit: 
      lda #0
      sta Score+2            ; Reset ten's digit from 9 to 0
      lda Score+3            ; Load the third digit
      clc
      adc #1                 ; Add 1 (the carry from the previous digit)
      sta Score+3
    DoneIncrementing:
      rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the score in the nametable/background using buffering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer format starting at memory address $0700:
;;
;; 03 20 52 00 00 02 01 20  78 00 00
;;  | \___/ \______/  | \___/   |  |
;;  |   |      |      |   |     |  |
;;  |   |      |      |   |     |  Length=0 (end of buffering)
;;  |   |      |      |   |     byte to copy
;;  |   |      |      |   PPU Address $2078
;;  |   |      |      Length=1
;;  |   |       bytes to copy
;;  |   PPU Address $2052
;;  Length=3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawScore
    lda #$07
    sta BufPtr+1
    lda #$00
    sta BufPtr+0

    ldy #0
    
    lda #3                   ; Length = 3 (how many bytes we will send)
    sta (BufPtr),y
    iny

    lda #$20
    sta (BufPtr),y           ; Hi-Byte of the PPU address to be updated
    iny
    lda #$52
    sta (BufPtr),y           ; Lo-Byte of the PPU address to be updated
    iny

    ;; Send the 3 digits of the score (from MSB to LSB) 100s, 10s, 1s

    lda Score+2              ; 100s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda Score+1              ; 10s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda Score+0              ; 1s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda #0
    sta (BufPtr),y           ; Length=0 to signal the end of the buffer
    iny

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to read controller state and store it inside "Buttons" in RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ReadControllers
    lda #1                   ; A = 1
    sta Buttons              ; Buttons = 1
    sta JOYPAD1              ; Set Latch=1 to begin 'Input'/collection mode
    lsr                      ; A = 0
    sta JOYPAD1              ; Set Latch=0 to begin 'Output' mode
LoopButtons:
    lda JOYPAD1              ; This reads a bit from the controller data line and inverts its value,
                             ; And also sends a signal to the Clock line to shift the bits
    lsr                      ; We shift-right to place that 1-bit we just read into the Carry flag
    rol Buttons              ; Rotate bits left, placing the Carry value into the 1st bit of 'Buttons' in RAM
    bcc LoopButtons          ; Loop until Carry is set (from that initial 1 we loaded inside Buttons)
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns a random 8.bit number inside A (0-255), clobbers Y (0).
;; Requires a 2-byte value on the zero-page called "Seed".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an 16-bit Galois linear feedback shift register with polynomial $0039.
;; The sequence of numbers it generates will repeat after 65535 calls.
;; Execution time is an average of 125 cycles (excluding jsr and rts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetRandomNumber
    ldy #8                   ; Loop counter (generate 8 bits)
    lda Seed+0
:   asl                      ; Shift the register
    rol Seed+1
    bcc :+
      eor #$39               ; Apply XOR feedback when a 1 bit is shifted out
    :
    dey
    bne :--
    sta Seed+0               ; Saves the value in A into the Seed
    cmp #0                   ; Set flags
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPalette
    PPU_SETADDR $3F00
    ldy #0                   ; Y = 0
:   lda (PalPtr),y           ; Lookup byte in ROM
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #32                  ; Is Y equal to 32?
    bne :-                   ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine set the color palette to Cloudy mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetPaletteCloudy
    lda #>PaletteDataCloudy
    sta PalPtr+1
    lda #<PaletteDataCloudy
    sta PalPtr+0
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine set the color palette to Clear mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetPaletteClear
    lda #>PaletteDataClear
    sta PalPtr+1
    lda #<PaletteDataClear
    sta PalPtr+0
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine set the color palette to Night mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetPaletteNight
    lda #>PaletteDataNight
    sta PalPtr+1
    lda #<PaletteDataNight
    sta PalPtr+0
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to draw a new column of tiles off-screen as we scroll horizontally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawNewColumn
    lda XScroll              ; We'll set the NewColAddr lo-byte and hi-byte
    lsr
    lsr
    lsr                      ; Shift left 3 times to divide XScroll by 8
    sta NewColAddr           ; Set the lo-byte of the column address

    lda CurrNametable        ; The hi-byte comes from the nametable
    eor #1                   ; Invert the low bit (0 or 1)
    asl
    asl                      ; Multiply by 4 (A is $00 or $04)
    clc
    adc #$20                 ; Add $20 (A is $20 or $24) for nametable 0 or 1
    sta NewColAddr+1         ; Set the hi-byte of the column address  ($20xx or $24xx)

    lda Column               ; Multiply (col * 32) to compute the data offset
    asl
    asl
    asl
    asl
    asl
    sta SourceAddr           ; Store lo-byte (--XX) of column source address

    lda Column
    lsr
    lsr
    lsr                      ; Divide current Column by 8 (using 3 shift rights)
    sta SourceAddr+1         ; Store hi-byte (XX--) of column source address

                             ; Here we'll add the offset the column source address with the address of where the BackgroundData
    lda SourceAddr           ; Lo-byte of the column data start + offset = address to load column data from
    clc
    adc #<BackgroundData     ; Add the lo-byte
    sta SourceAddr           ; Save the result of the offset back to the source address lo-byte

    lda SourceAddr+1         ; Hi-byte of the column source address
    adc #>BackgroundData     ; Add the hi-byte
    sta SourceAddr+1         ; Add the result of the offset back to the source address hi-byte

    DrawColumn:
        lda #%00000100
        sta PPU_CTRL         ; Tell the PPU that the increments will be +32 mode

        lda PPU_STATUS       ; Hit PPU_STATUS to reset hi/lo address latch
        lda NewColAddr+1
        sta PPU_ADDR         ; Set the hi-byte of the new column start address
        lda NewColAddr
        sta PPU_ADDR         ; Set the lo-byte of the new column start address

        ldx #30              ; We'll loop 30 times (=30 rows)
        ldy #0
    DrawColumnLoop:
        lda (SourceAddr),y   ; Copy from the address of the column source + y offset
        sta PPU_DATA
        iny                  ; Y++
        dex                  ; X--
        bne DrawColumnLoop
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to draw a new attributes off-screen every 32 pixels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawNewAttribs
    lda CurrNametable
    eor #1
    asl
    asl
    clc
    adc #$23
    sta NewColAddr+1

    lda XScroll
    lsr
    lsr
    lsr
    lsr
    lsr
    clc
    adc #$C0
    sta NewColAddr

    lda Column
    and #%11111100
    asl
    sta SourceAddr

    lda Column
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    sta SourceAddr+1

    lda SourceAddr
    clc
    adc #<AttributeData
    sta SourceAddr

    lda SourceAddr+1
    adc #>AttributeData
    sta SourceAddr+1

    DrawAttribute:
      bit PPU_STATUS
      ldy #0
      DrawAttribLoop:
        lda NewColAddr+1
        sta PPU_ADDR
        lda NewColAddr
        sta PPU_ADDR
        lda (SourceAddr),y
        sta PPU_DATA
        iny
        cpy #8
        beq :+
          lda NewColAddr
          clc
          adc #8
          sta NewColAddr
          jmp DrawAttribLoop
        :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add new actor to the array in the first empty slot found
;; Params = ParamType, ParamXPos, ParamYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewActor
    ldx #0                             ; X = 0
  ArrayLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)   ; Reached maximum number of actors allowed in the
    beq EndRoutine                     ; Then we skip and don't add a new actor
    lda ActorsArray+Actor::Type,x
    cmp #ActorType::NULL               ; If the actor type of this array position is NULL
    beq AddNewActorToArray             ; Then we found an empty slot, proceed to add actor to position [x]
  NextActor:
    txa
    clc
    adc #.sizeof(Actor)                ; Otherwise, we offset to check the next actor in the array
    tax                                ; X += sizeof(Actor)
    jmp ArrayLoop

  AddNewActorToArray:                  ; Here we add a new actor at index [x] of the array
    lda ParamType                      ; Fetch parameter "actor type" from RAM
    sta ActorsArray+Actor::Type,x
    lda ParamXPos                      ; Fetch parameter "actor position X" from RAM
    sta ActorsArray+Actor::XPos,x
    lda ParamYPos                      ; Fetch parameter "actor position Y" from RAM
    sta ActorsArray+Actor::YPos,x
    lda #0
    sta ActorsArray+Actor::Screen,x
EndRoutine:
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to spawn actors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SpawnActors
  SpawnSubmarine:
    lda Clock60                        ; Submarine are added in intervals of 3 seconds
    sec
    sbc PrevSubmarine
    cmp #3                             ; Only add a new submarine if the difference in time from the previous
    bne :+
      lda #ActorType::SUBMARINE
      sta ParamType                    ; Load parameter for the actor type
      lda #255
      sta ParamXPos                    ; Load the parameter for actor position X
      jsr GetRandomNumber
      lsr
      lsr
      lsr
      clc
      adc #180
      sta ParamYPos                    ; Load the parameter for actor position Y

      jsr AddNewActor                  ; Call the subroutine to add new actor

      lda Clock60
      sta PrevSubmarine                ; Save the current Clock60 as the submarine last spawn time
    :

  SpawnAirplane:
    lda Clock60                        ; Submarine are added in intervals of 3 seconds
    sec
    sbc PrevAirplane
    cmp #2                             ; Only add a new submarine if the difference in time from the previous
    bne :+
      lda #ActorType::AIRPLANE
      sta ParamType                    ; Load parameter for the actor type
      lda #255
      sta ParamXPos                    ; Load the parameter for actor position X
      jsr GetRandomNumber
      lsr
      lsr
      clc
      adc #35
      sta ParamYPos                    ; Load the parameter for actor position Y

      jsr AddNewActor                  ; Call the subroutine to add new actor

      lda Clock60
      sta PrevAirplane                ; Save the current Clock60 as the submarine last spawn time
    :

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all the enemy actors checking for a collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckEnemyCollision
    txa
    pha
    
    ldx #0
    stx Collision

  EnemiesCollisionLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)
    beq FinishCollisionCheck
      lda ActorsArray+Actor::Type,x
      cmp #ActorType::AIRPLANE
      bne NextEnemy

      ;; LOAD BOUNDING BOX X1, Y1, X2 and Y2
      lda ActorsArray+Actor::XPos,x
      sta ParamRectX1
      lda ActorsArray+Actor::YPos,x
      sta ParamRectY1

      lda ActorsArray+Actor::XPos,x
      clc
      adc #22
      sta ParamRectX2

      lda ActorsArray+Actor::YPos,x
      clc
      adc #8
      sta ParamRectY2

      jsr IsPointInsideBoundingBox

      lda Collision
      beq NextEnemy
        lda #ActorType::NULL
        sta ActorsArray+Actor::Type,x
        jmp FinishCollisionCheck
  
  NextEnemy:
    txa
    clc
    adc #.sizeof(Actor)
    tax
    jmp EnemiesCollisionLoop

FinishCollisionCheck:
    pla
    tax
    rts

.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point (ParamX, ParamY) is inside a bounding box ParamRectX1/ParamRectY1, ParamRextX2/ParamRectY2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsPointInsideBoundingBox
    lda ParamXPos
    cmp ParamRectX1
    bcc :+
      cmp ParamRectX2
      bcs :+
        lda ParamYPos
        cmp ParamRectY1
        bcc :+
          cmp ParamRectY2
          bcs :+
            lda #1
            sta Collision
:   rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to update all the actors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateActors
    ldx #0
    ActorsLoop:
      lda ActorsArray+Actor::Type,x

      cmp #ActorType::MISSILE
      bne :+
        lda ActorsArray+Actor::YPos,x
        sec
        sbc #1                         ; Decrement Y position of missiles of 1
        sta ActorsArray+Actor::YPos,x
        bcs SkipMissile
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,x
        SkipMissile:
      CheckCollision:
        lda ActorsArray+Actor::XPos,x
        clc
        adc #3
        sta ParamXPos

        lda ActorsArray+Actor::YPos,x
        clc
        adc #1
        sta ParamYPos

        jsr CheckEnemyCollision
        lda Collision
        beq NoCollisionFound
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,x
          
          jsr IncrementScore
          jsr DrawScore
            
          PUSH_REGS
          lda #1
          ldx #FAMISTUDIO_SFX_CH1
          jsr famistudio_sfx_play ; Play missile launch sound effect
          PULL_REGS


        NoCollisionFound:

        jmp NextActor
      :
      
      cmp #ActorType::SUBMARINE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec
        sbc #1                          ; Decrement X position of submarine of 1
        sta ActorsArray+Actor::XPos,x

        lda ActorsArray+Actor::Screen,x
        sbc #0
        sta ActorsArray+Actor::Screen,x ; Subtracting from the hi-byte of the XPos (which is our Screen)

        cmp #$FF
        bne SkipSubmarine               ; Only remove actor if it's in Screen -1 (left screen)
          lda ActorsArray+Actor::XPos,x
          cmp #$E0
          bne SkipSubmarine
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x
        SkipSubmarine:
        jmp NextActor
      :
      
      cmp #ActorType::AIRPLANE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec
        sbc #1                         ; Decrement X position of submarine of 1
        sta ActorsArray+Actor::XPos,x

        lda ActorsArray+Actor::Screen,x
        sbc #0
        sta ActorsArray+Actor::Screen,x ; Subtracting from the hi-byte of the XPos (which is our Screen)

        cmp #$FF
        bne SkipAirplane               ; Only remove actor if it's in Screen -1 (left screen)
          lda ActorsArray+Actor::XPos,x
          cmp #$E0
          bne SkipAirplane
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x
        SkipAirplane:
        jmp NextActor
      :

      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        beq :+ 
          jmp ActorsLoop
        :
    
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to render all the actors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderActors
    ;; Load SprPtr to point to $0200
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ldy #0                             ; Count how many tiles we are sending
    ldx #0                             ; Counts how many actors we are looping
    ActorsLoop:
      lda ActorsArray+Actor::Screen,x
      sta ParamScreen

      lda ActorsArray+Actor::Type,x
      
      cmp #ActorType::SPRITE0
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$70
        sta ParamTileNum
        lda #1
        sta ParamNumTiles
        lda #%00100000
        sta ParamAttribs
        jsr DrawSprite                 ; Draw sprite0
        jmp NextActor
      :

      cmp #ActorType::PLAYER
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$60
        sta ParamTileNum
        lda #4
        sta ParamNumTiles
        lda #%00000000
        sta ParamAttribs
        jsr DrawSprite                 ; Draw player sprite
        jmp NextActor
      :

      cmp #ActorType::MISSILE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$50
        sta ParamTileNum
        lda #1
        sta ParamNumTiles
        lda #%00000001
        sta ParamAttribs
        jsr DrawSprite                 ; Draw missile sprite
        jmp NextActor
      :

      cmp #ActorType::SUBMARINE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$04
        sta ParamTileNum
        lda #4
        sta ParamNumTiles
        lda #%00100000
        sta ParamAttribs
        jsr DrawSprite                 ; Draw submarine sprite
        jmp NextActor
      :

      cmp #ActorType::AIRPLANE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$10
        sta ParamTileNum
        lda #3
        sta ParamNumTiles
        lda #%00000011
        sta ParamAttribs
        jsr DrawSprite                 ; Draw airplane sprite
        jmp NextActor
      :

      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        beq :+
          jmp ActorsLoop
        :

    tya
    pha
    
    LoopTrailingTiles:
      cpy PrevOAMCount
      bcs:+
        lda #$FF
        sta (SprPtr),y
        iny
        sta (SprPtr),y
        iny
        sta (SprPtr),y
        iny
        sta (SprPtr),y
        iny
        jmp LoopTrailingTiles
      :

    pla                                ; Save the previous value of Y into PrevOAMCount
    sta PrevOAMCount                   ; This is the total number of bytes that we just sent to the OAM

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to loop "NumTiles" times, sending bytes to the OAM-RAM
;; Params = ParamXPos, ParamYPos, ParamTileNum, ParamAttribs, ParamNumTiles, ParamScreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawSprite
    txa
    pha                      ; Save the value of the X register

    ldx #0
    TileLoop:

      lda ParamScreen
      bne SkipTile

        lda ParamYPos          ; Send Y position to the OAM 
        sta (SprPtr),y
        iny

        lda ParamTileNum       ; Send the Tile # to the OAM
        sta (SprPtr),y
        inc ParamTileNum       ; ParamTileNum++
        iny

        lda ParamAttribs       ; Send the attributes to the OAM
        sta (SprPtr),y
        iny

        lda ParamXPos          ; Send X position to the OAM 
        sta (SprPtr),y
        clc
        adc #8
        sta ParamXPos          ; ParamXPos += 8
        
        lda ParamScreen
        adc #0
        sta ParamScreen        ; Incrementing hi-byte of the XPos (using the carry of the previous addition)
        iny

        jmp NextTile
      
      SkipTile:
        lda ParamXPos
        clc 
        adc #8
        sta ParamXPos          ; ParamXPos += 8
        lda ParamScreen
        adc #0
        sta ParamScreen        ; Increment hi-byte of the XPos (using the carry of the previous addition)
        inc ParamTileNum       ; ParamTileNum++

      NextTile:
        inx                    ; X++
        cpx ParamNumTiles      ; Loop until X == NumTiles
        bne TileLoop
    
    pla
    tax                      ; Restore the value of the X register

    rts 
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load background data from the TitleScreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadTitleScreenNAM
    lda #<TitleScreenData    ; Lo-byte of memory address of (BgPtr)+0
    sta BgPtr
    lda #>TitleScreenData    ; Hi-byte of memory address of (BgPtr)+1
    sta BgPtr+1

    PPU_SETADDR $2000        ; Point PPU address to first nametable at $2000

    ldx #$00                 ; X = 0 --> x is the outer loop index (hi-byte) from $0 to $4
    ldy #$00                 ; Y = 0 --> y is the inner loop index (lo-byte) from $0 to $FF
  OuterLoop:
  InnerLoop:
    lda (BgPtr),y            ; Fetch background data from ROM
    sta PPU_DATA
    iny                      ; Y++
    cpy #0
    beq IncreaseHiByte       ; If Y == 0 (wrapped around 256?), Then: we need to increase the hi-byte
    jmp InnerLoop            ; Else: Continue with the inner loop
  IncreaseHiByte:
    inc BgPtr+1              ; We increment the hi-byte pointer to point to the next background data region
    inx                      ; X++
    cpx #4
    bne OuterLoop            ; If X is not 4, loop back to the outer loop
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load background data from RLE compressed TitleScreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadTitleScreenRLE
    lda #<TitleScreenData    ; Lo-byte of memory address of (BgPtr)+0
    sta BgPtr
    lda #>TitleScreenData    ; Hi-byte of memory address of (BgPtr)+1
    sta BgPtr+1

    PPU_SETADDR $2000        ; Point PPU address to first nametable at $2000
    
    ldy #0                   ; Y = 0 (Y counts how many bytes we are reading)

  LengthLoop:
    lda (BgPtr),y            ; Fetch new length
    beq EndRoutine           ; Stop looping when we find Length = 0
    iny
    
    bne :+
      inc BgPtr+1            ; Increment hi-byte if Y rolls off back to 0
    :

    tax                      ; Transfer length to X (tile counter)
    lda (BgPtr),y            ; Fetch new tile number
    iny                      ; Y++
    
    bne :+
      inc BgPtr+1            ; Increment hi-byte if Y rolls off back to 0
    :

    TileLoop:
      sta PPU_DATA           ; Send tile value to PPU
      dex                    ; X--
      bne TileLoop
    jmp LengthLoop           ; Loop back to read the next length

  EndRoutine:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to switch CHR banks
;; Params = A has the bank number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SwitchCHRBank
    sta $8000                ; $8000 is the bank switch register of mapper 3
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES                 ; Macro to initialize the NES to a known state
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   T I T L E S C R E E N   ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc TitleScreen
    lda #1
    jsr SwitchCHRBank        ; Switch the CHR tiles to use the titlescreen one
    lda #State::TITLESCREEN
    sta GameState            ; GameState = TITLESCREEN
    jsr SetPaletteCloudy     ; Set palette to cloudy
    jsr LoadPalette          ; Load the selected palette
    jsr LoadTitleScreenRLE   ; Load the titlescreen nametable
    lda #0
    sta MenuItem
    
AudioEngineInit:
    ldx #<music_data_titan    ; load lo-byte of music
    ldy #>music_data_titan    ; load hi-byte of music
    lda #1 ; NTSC
    jsr famistudio_init       ; initialize famistudio
    lda #0
    jsr famistudio_music_play ; start playing music

DrawMenuArrow:
    lda #92                  
    sta $0200                ; Sprite Y position at $0200
    lda #$23                 
    sta $0201                ; Sprite Tile # at $0201
    lda #%00000001           
    sta $0202                ; Sprite attributes at $0202
    lda #90                  
    sta $0203                ; Sprite X position at $0203

EnableNMI:
    lda #%10010000
    sta PPU_CTRL
    lda #%00011110
    sta PPU_MASK

CheckMenuItem:
    lda MenuItem             ; Check the current menu selection
    bne :+
      jsr SetPaletteClear    ; If 0 set palette to clear
    : cmp #1
    bne :+
      jsr SetPaletteCloudy   ; If 1 set palette to cloudy
    : cmp #2
    bne :+
      jsr SetPaletteNight    ; If 2 set palette to night
    :

TitleScreenLoop:
    jsr famistudio_update    ; progress audio frame per frame

    lda Buttons
    sta PrevButtons
    jsr ReadControllers
    
  CheckStartButton:
    lda Buttons
    and #BUTTON_START
    beq :+
      jmp GamePlay           ; Start the game (and change to the PLAYING GameState)
    :

  CheckUpButton:
    lda Buttons              ; Load buttons byte
    and #BUTTON_UP           ; Mask UP button bit
    beq :+                   ; If the UP button is pressed the UP bit should be 1
      lda PrevButtons
      cmp Buttons
      beq :+                 ; Only perform button action once per press/release
        lda MenuItem
        beq :+               ; Only decrement MenuItem if over 0
          dec MenuItem       ; MenuItem--
          lda $0200          ; Load selection arrow Y position
          sec
          sbc #17            ; Subtract 17 pixels as we go up in the menu items
          sta $0200          ; Store selection arrow Y position
          jmp CheckMenuItem  ; Do things based on currently selected menu entry
    :

  CheckDownButton:
    lda Buttons
    and #BUTTON_DOWN
    beq :+
      lda PrevButtons
      cmp Buttons
      beq :+
        lda MenuItem
        cmp #2
        beq:+
          inc MenuItem
          lda $0200
          clc
          adc #17            ; Add 17 pixels as we go down in the menu items
          sta $0200
          jmp CheckMenuItem
    :

    WaitForVBlank:
      lda IsDrawComplete
      beq WaitForVBlank
    lda #0
    sta IsDrawComplete

    jmp TitleScreenLoop
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   G A M E   P L A Y   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GamePlay
    lda #0
    jsr SwitchCHRBank
    lda #State::PLAYING
    sta GameState

    PPU_DISABLE_NMI

InitVariables:
    lda #0
    sta Frame                ; Frame = 0
    sta Clock60              ; Clock60 = 0
    sta XScroll              ; XScroll = 0
    sta Column               ; Column = 0
    sta CurrNametable        ; CurrNametable = 0
    lda #113
    sta XPos
    lda #165
    sta YPos

    lda #$10
    sta Seed+1
    sta Seed+0               ; Initialize the Seed with any value different than 0

AudioEngineInit:
    jsr famistudio_music_stop
    ldx #<music_data_maritime ; load lo-byte of music
    ldy #>music_data_maritime ; load hi-byte of music
    lda #1 ; NTSC
    jsr famistudio_init       ; initialize famistudio
    lda #0
    jsr famistudio_music_play ; start playing music

    ldx #<sounds
    ldy #>sounds
    jsr famistudio_sfx_init       ; initialize famistudio sfx

Main:
    jsr LoadPalette          ; Call LoadPalette subroutine to load 32 colors into our palette

 AddSprite0:
     lda #ActorType::SPRITE0
     sta ParamType
     lda #0
     sta ParamXPos
     lda #27
     sta ParamYPos
     lda #0
     jsr AddNewActor
 
 AddPlayer:
     lda #ActorType::PLAYER
     sta ParamType
     lda XPos
     sta ParamXPos
     lda YPos
     sta ParamYPos
     lda #0
     jsr AddNewActor

InitBackgroundTiles:
    lda #1
    sta CurrNametable
:   jsr DrawNewColumn
    inc Column

    lda #8
    clc
    adc XScroll
    sta XScroll               

    lda Column
    cmp #32
    bne :-                   

    lda #0
    sta CurrNametable
    sta XScroll

    jsr DrawNewColumn
    inc Column

    lda #%00000000
    sta PPU_CTRL

InitAttributes:
    lda #1
    sta CurrNametable
    lda #0
    sta XScroll
    sta Column
InitAttribsLoop:
    jsr DrawNewAttribs
    lda XScroll
    clc
    adc #32
    sta XScroll

    lda Column
    clc
    adc #4
    sta Column
    cmp #32
    bne InitAttribsLoop

    lda #0
    sta CurrNametable
    lda #1
    sta XScroll
    jsr DrawNewAttribs

    inc Column

EnableRendering:
    lda #%10010000           ; Enable NMI and set background to use the 2nd pattern table (at $1000)
    sta PPU_CTRL
    lda #0
    sta PPU_SCROLL           ; Disable scroll in X
    sta PPU_SCROLL           ; Disable scroll in Y
    lda #%00011110
    sta PPU_MASK             ; Set PPU_MASK bits to render the background

GameLoop:   
    jsr famistudio_update
    lda Buttons
    sta PrevButtons          ; Stores the previously pressed buttons

    jsr ReadControllers

CheckAButton:
    lda Buttons
    and #BUTTON_A
    beq :+
      lda PrevButtons
      cmp Buttons
      beq:+
        lda #ActorType::MISSILE
        sta ParamType
        lda XPos
        sta ParamXPos
        lda YPos
        sta ParamYPos
        jsr AddNewActor

        lda #0
        ldx #FAMISTUDIO_SFX_CH0
        jsr famistudio_sfx_play ; Play missile launch sound effect
    :

    jsr SpawnActors
    jsr UpdateActors
    jsr RenderActors

    WaitForVBlank:
      lda IsDrawComplete
      beq WaitForVBlank
    lda #0
    sta IsDrawComplete

    jmp GameLoop
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    PUSH_REGS                ; Macro to save register values by pushing them to the stack

    inc Frame                ; Frame++

OAMStartDMACopy:             ; DMA copy of OAM data from RAM to PPU
    lda #$02                 ; Every frame, we copy spite data starting at $02**
    sta PPU_OAM_DMA          ; The OAM-DMA copy starts when we write to $4014

BackgroundCopy:              ; Here is where we copy/draw the background buffer from $0700 to the PPU
    lda #$07
    sta BufPtr+1             
    lda #$00
    sta BufPtr+0             ; Set BufPtr pointer to start at address $0700

    ldy #$00
  BufferLoop:
    lda (BufPtr),y           ; Fetch the Length
    beq EndBackgroundCopy    ; If Length is 0, stop reading from background buffer

    tax                      ; X = Length

    iny
    lda (BufPtr),y           ; Fetch hi-byte of PPU address to be updated
    sta PPU_ADDR
    iny
    lda (BufPtr),y           ; Fetch lo-byte of PPU address to be updated
    sta PPU_ADDR
    iny
  DataLoop:
    lda (BufPtr),y
    sta PPU_DATA
    iny
    dex                      ; X--
    bne DataLoop

    jmp BufferLoop           ; Loop back until we finish the buffer (find an entry with Length=0)
EndBackgroundCopy:

    lda GameState
    cmp #State::PLAYING
    bne EndScrolling

NewColumnCheck:
    lda XScroll
    and #%00000111           ; Check if the scroll a multiple of 8
    bne :+                   ; If it isn't, we still don't need to draw a new column
      jsr DrawNewColumn      ; If it is a multiple of 8, we proceed to draw a new column of tiles!
    Clamp128Cols:
      lda Column
      clc
      adc #1                 ; Column++
      and #%01111111         ; Drop the left-most bit to wrap around 128
      sta Column             ; Clamping the value to never go beyond 128
    :

NewAttribsCheck:
    lda XScroll
    and #%00011111
    bne :+
      jsr DrawNewAttribs
    :

SetPPUNoScroll:
    lda #0
    sta PPU_SCROLL
    sta PPU_SCROLL

EnablePPUSprite0:
    lda #%10010000
    sta PPU_CTRL
    lda #%00011110
    sta PPU_MASK

WaitForNoSprite0:
    lda PPU_STATUS
    and #%01000000
    bne WaitForNoSprite0

WaitForSprite0:
    lda PPU_STATUS
    and #%01000000
    beq WaitForSprite0

ScrollBackground:
    inc XScroll              ; XScroll++
    lda XScroll
    bne :+                   ; Check if XScroll rolled back to 0, then we swap nametables!
      lda CurrNametable
      eor #1                 ; An XOR with %00000001 will flip the right-most bit.
      sta CurrNametable      ; If it was 0, it becomes 1. If it was 1, it becomes 0.
    :
    lda XScroll
    sta PPU_SCROLL           ; Set the horizontal X scroll first
    lda #0
    sta PPU_SCROLL           ; No vertical scrolling

EndScrolling:

RefreshRendering:
    lda #%10010000           ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
    ora CurrNametable        ; OR with CurrNametable (0 or 1) to set PPU_CTRL bit-0 (starting nametable)
    sta PPU_CTRL
    lda #%00011110           ; Enable sprites, enable background, no clipping on left side
    sta PPU_MASK

SetGameClock:
    lda Frame                ; Increment Clock60 every time we reach 60 frames (NTSC = 60Hz)
    cmp #60                  ; Is Frame equal to #60?
    bne :+                   ; If not, bypass Clock60 increment
    inc Clock60              ; But if it is 60, then increment Clock60 and zero Frame counter
    lda #0
    sta Frame
:

SetDrawComplete:
    lda #1
    sta IsDrawComplete

    PULL_REGS

    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hardcoded list of color values in ROM to be loaded by the PPU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaletteDataCloudy:
.byte $1C,$0F,$22,$1C, $1C,$37,$3D,$0F, $1C,$37,$3D,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette
PaletteDataClear:
.byte $1C,$0F,$22,$1C, $1C,$36,$21,$0B, $1C,$36,$21,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette
PaletteDataNight:
.byte $0C,$0F,$1C,$0C, $0C,$26,$0C,$0F, $0C,$26,$0C,$2D, $0C,$36,$07,$2D ; Background palette
.byte $0C,$0F,$1D,$2D, $0C,$0F,$20,$27, $0C,$2D,$38,$18, $0C,$0F,$1A,$21 ; Sprite palette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background data (contains 4 screens that should scroll horizontally)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BackgroundData:
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$23,$33,$15,$21,$12,$00,$31,$31,$31,$55,$56,$00,$00 ; ---> screen column 1 (from top to bottom)
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$24,$34,$15,$15,$12,$00,$31,$31,$53,$56,$56,$00,$00 ; ---> screen column 2 (from top to bottom)
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$14,$11,$3e,$15,$12,$00,$00,$00,$31,$52,$56,$00,$00 ; ---> screen column 3 (from top to bottom)
.byte $13,$13,$7f,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$44,$21,$21,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$31,$5a,$56,$00,$00 ; ...
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$45,$21,$21,$21,$22,$32,$15,$15,$12,$00,$00,$00,$31,$58,$56,$00,$00 ; ...
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$46,$21,$21,$21,$26,$36,$15,$15,$12,$00,$00,$00,$51,$5c,$56,$00,$00 ; ...
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$27,$37,$15,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$61,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$28,$38,$15,$15,$12,$00,$00,$00,$00,$5c,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$57,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$47,$21,$21,$21,$48,$21,$21,$22,$32,$3e,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$4a,$21,$21,$23,$33,$4e,$15,$12,$00,$00,$00,$00,$59,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$24,$34,$3f,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$57,$56,$00,$00
.byte $13,$13,$6c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$59,$56,$00,$00
.byte $13,$13,$78,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$7b,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$15,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$53,$56,$00,$00
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$15,$21,$21,$25,$35,$15,$15,$12,$00,$60,$00,$00,$54,$56,$00,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$26,$36,$15,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$27,$37,$15,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$48,$21,$21,$15,$27,$37,$15,$15,$12,$00,$00,$00,$00,$5d,$56,$00,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$28,$38,$3e,$21,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$22,$35,$3f,$21,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$26,$36,$3f,$21,$12,$00,$00,$00,$00,$57,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$4a,$21,$21,$21,$27,$37,$21,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$76,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$21,$21,$21,$28,$38,$15,$15,$12,$00,$00,$00,$00,$58,$56,$00,$00
.byte $13,$13,$72,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$14,$11,$3e,$21,$12,$00,$00,$00,$00,$59,$56,$00,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$14,$11,$4e,$21,$12,$00,$00,$00,$51,$59,$56,$00,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$00,$5c,$56,$00,$00
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$29,$39,$21,$21,$12,$00,$00,$00,$00,$55,$56,$00,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$48,$21,$2c,$2a,$3a,$3c,$21,$12,$00,$00,$00,$54,$56,$56,$00,$00
.byte $13,$13,$65,$13,$20,$21,$21,$21,$21,$21,$21,$21,$46,$21,$21,$21,$4a,$21,$2d,$2a,$3a,$3d,$15,$12,$00,$00,$00,$00,$52,$56,$00,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$2b,$3b,$15,$15,$12,$00,$00,$00,$00,$57,$56,$00,$00

.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$21,$12,$00,$31,$31,$31,$55,$56,$ff,$9a
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$15,$21,$14,$11,$15,$15,$12,$00,$31,$31,$53,$56,$56,$ff,$5a
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$15,$21,$14,$11,$3e,$15,$12,$00,$00,$00,$31,$52,$56,$ff,$5a
.byte $13,$13,$7f,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$15,$15,$15,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$31,$5a,$56,$ff,$56
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$15,$15,$15,$21,$14,$11,$15,$15,$12,$00,$00,$00,$31,$58,$56,$ff,$59
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$15,$15,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$51,$5c,$56,$ff,$5a
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$ff,$5a
.byte $13,$13,$61,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$5c,$56,$ff,$5a
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$57,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$47,$21,$21,$21,$48,$21,$21,$14,$11,$3e,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$4a,$21,$21,$14,$11,$4e,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$25,$35,$3f,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$26,$36,$15,$15,$12,$00,$00,$00,$00,$57,$56,$aa,$00
.byte $13,$13,$6c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$21,$21,$21,$27,$37,$15,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$78,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$21,$21,$21,$28,$38,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7b,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$15,$15,$21,$21,$29,$39,$15,$15,$12,$00,$00,$00,$00,$53,$56,$aa,$00
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$15,$21,$1f,$2a,$3a,$3c,$15,$12,$00,$61,$00,$00,$54,$56,$aa,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$28,$3b,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$48,$21,$21,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$5d,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$14,$11,$3e,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$14,$11,$3f,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$14,$11,$3f,$21,$12,$00,$00,$00,$00,$57,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$4a,$21,$21,$21,$14,$11,$21,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$76,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$5a,$00
.byte $13,$13,$72,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$14,$11,$3e,$21,$12,$00,$00,$00,$00,$59,$56,$9a,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$22,$32,$4e,$21,$12,$00,$00,$00,$51,$59,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$23,$33,$3f,$15,$12,$00,$00,$00,$00,$5c,$56,$6a,$00
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$24,$34,$21,$21,$12,$00,$00,$00,$00,$55,$56,$9a,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$48,$15,$15,$14,$11,$15,$21,$12,$00,$00,$00,$54,$56,$56,$aa,$00
.byte $13,$13,$65,$13,$20,$21,$21,$21,$21,$21,$21,$21,$46,$21,$21,$21,$4a,$21,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$52,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$57,$56,$aa,$00

.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$21,$12,$00,$31,$31,$31,$58,$56,$ff,$9a
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$14,$11,$15,$15,$12,$00,$31,$31,$00,$5d,$56,$ff,$5a
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$14,$11,$3e,$15,$12,$00,$00,$00,$31,$58,$56,$ff,$5a
.byte $13,$13,$7f,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$44,$21,$21,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$31,$58,$56,$ff,$aa
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$45,$21,$21,$21,$22,$32,$15,$15,$12,$00,$00,$00,$31,$58,$56,$ff,$56
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$46,$21,$21,$21,$26,$36,$15,$15,$12,$00,$00,$00,$51,$58,$56,$ff,$9a
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$27,$37,$15,$15,$12,$00,$00,$00,$00,$58,$56,$ff,$59
.byte $13,$13,$61,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$28,$38,$15,$15,$12,$00,$00,$00,$00,$55,$56,$ff,$5a
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$57,$56,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$47,$21,$21,$21,$48,$21,$21,$22,$32,$3e,$15,$12,$00,$00,$00,$00,$52,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$4a,$21,$21,$23,$33,$4e,$15,$12,$00,$00,$00,$00,$53,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$24,$34,$3f,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$6c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$78,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7b,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$62,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$29,$39,$15,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$1f,$2a,$3a,$3d,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$48,$21,$15,$2d,$2a,$3a,$3c,$15,$12,$00,$00,$00,$00,$5b,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$2f,$2a,$3a,$3d,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$28,$3b,$3e,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$49,$21,$21,$21,$14,$11,$4e,$21,$12,$00,$00,$00,$51,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$4a,$21,$21,$21,$14,$11,$21,$15,$12,$00,$00,$00,$51,$58,$56,$aa,$00
.byte $13,$13,$76,$13,$20,$21,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$21,$21,$15,$29,$39,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$72,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$15,$2c,$2a,$3a,$3e,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$2e,$2a,$3a,$4e,$21,$12,$00,$00,$00,$51,$58,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$1f,$2a,$3a,$3f,$15,$12,$00,$00,$00,$00,$5d,$56,$aa,$00
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$15,$28,$3b,$3f,$21,$12,$00,$00,$00,$00,$57,$56,$aa,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$48,$21,$15,$14,$11,$15,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$65,$13,$20,$21,$21,$21,$21,$21,$21,$21,$46,$21,$21,$21,$4a,$21,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00

.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$21,$12,$00,$31,$31,$31,$58,$56,$ff,$9a
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$14,$11,$15,$15,$12,$00,$31,$31,$00,$58,$56,$ff,$5a
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$31,$58,$56,$ff,$5a
.byte $13,$13,$7f,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$15,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$31,$54,$56,$ff,$59
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$15,$21,$21,$21,$14,$11,$3e,$15,$12,$00,$00,$00,$31,$54,$56,$ff,$56
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$42,$15,$21,$21,$15,$21,$21,$21,$14,$11,$4e,$15,$12,$00,$00,$00,$51,$58,$56,$ff,$5a
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$21,$21,$21,$21,$14,$11,$4e,$15,$12,$00,$00,$00,$00,$58,$56,$ff,$59
.byte $13,$13,$61,$13,$20,$21,$21,$21,$21,$21,$21,$44,$21,$21,$21,$21,$21,$21,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$00,$58,$56,$ff,$5a
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$45,$21,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$47,$15,$21,$21,$21,$15,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$53,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$15,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$21,$21,$21,$14,$11,$15,$15,$12,$00,$00,$00,$00,$57,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$21,$21,$21,$29,$39,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$6c,$13,$20,$21,$21,$21,$21,$21,$48,$21,$15,$21,$21,$21,$21,$1d,$1e,$2a,$3a,$3c,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$78,$13,$20,$21,$21,$21,$21,$21,$49,$21,$21,$21,$21,$21,$21,$21,$21,$2b,$3b,$3e,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$7b,$13,$20,$21,$21,$21,$21,$21,$4a,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$4e,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$6e,$13,$20,$21,$21,$21,$21,$15,$48,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$4e,$15,$12,$00,$63,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$49,$21,$21,$21,$21,$21,$21,$21,$21,$14,$11,$3f,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$4a,$21,$21,$21,$21,$21,$21,$15,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$15,$21,$15,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$60,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$15,$21,$21,$15,$14,$11,$15,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$42,$21,$21,$21,$15,$21,$21,$21,$29,$39,$15,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$43,$21,$21,$21,$15,$21,$21,$2c,$2a,$3a,$3c,$21,$12,$00,$00,$00,$50,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$44,$15,$21,$21,$15,$21,$21,$2d,$2a,$3a,$3e,$15,$12,$00,$00,$00,$50,$58,$56,$aa,$00
.byte $13,$13,$76,$13,$20,$21,$21,$21,$21,$21,$21,$45,$15,$21,$21,$21,$21,$21,$15,$2b,$3b,$3f,$15,$12,$00,$00,$00,$00,$54,$56,$aa,$00
.byte $13,$13,$72,$13,$20,$21,$21,$21,$21,$21,$21,$46,$15,$21,$21,$21,$21,$15,$15,$14,$11,$3f,$21,$12,$00,$00,$00,$00,$59,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$21,$21,$15,$14,$11,$15,$21,$12,$00,$00,$00,$51,$58,$56,$aa,$00
.byte $13,$13,$7c,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$21,$21,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$75,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$21,$21,$15,$14,$11,$15,$21,$12,$00,$00,$00,$00,$5d,$56,$aa,$00
.byte $13,$13,$84,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$15,$21,$15,$14,$11,$15,$21,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$65,$13,$20,$21,$21,$21,$21,$21,$21,$21,$15,$21,$21,$21,$15,$15,$15,$14,$11,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00
.byte $13,$13,$13,$13,$20,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$22,$32,$15,$15,$12,$00,$00,$00,$00,$58,$56,$aa,$00

AttributeData:
.byte $ff,$aa,$aa,$aa,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$6a,$a6,$00,$00,$00
.byte $ff,$aa,$aa,$9a,$59,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00

.byte $ff,$aa,$aa,$5a,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$9a,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$6a,$56,$00,$00,$00
.byte $ff,$aa,$aa,$9a,$59,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00

.byte $ff,$aa,$aa,$aa,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$aa,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$56,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$59,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00

.byte $ff,$aa,$aa,$aa,$9a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$59,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$56,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$59,$00,$00,$00
.byte $ff,$aa,$aa,$aa,$5a,$00,$00,$00

TitleScreenData:
;.incbin "titlescreen.nam"
.incbin "titlescreen.rle"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here goes the encoded music/sound data that was exported by FamiStudio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MusicData:
.include "music/maritime.asm"
.include "music/titan.asm"

SoundFXData:
.include "sfx/sounds.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here we add the CHR-ROM data, included from an external .CHR file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CHARS1"
.incbin "atlantico.chr"

.segment "CHARS2"
.incbin "titlescreen.chr"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                    ; Address (2 bytes) of the NMI handler
.word Reset                  ; Address (2 bytes) of the Reset handler
.word IRQ                    ; Address (2 bytes) of the IRQ handler
