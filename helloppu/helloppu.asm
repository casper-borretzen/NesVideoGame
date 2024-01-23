;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for PPU registers mapped from addresses $2000 to $2007
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL   = $2000
PPU_MASK   = $2001
PPU_STATUS = $2002
OAM_ADDR   = $2003
OAM_DATA   = $2004
PPU_SCROLL = $2005
PPU_ADDR   = $2006
PPU_DATA   = $2007

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the flags at $7FF0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.byte $4E,$45,$53,$1A         ; 4 bytes with the characters 'N','E','S','\n'
;       \N  \E  \S  \NEW LINE
.byte $02                     ; How many 16KB of PRG-ROM we'll use (=32KB)
.byte $01                     ; How many 8KB of CHR-ROM we'll use (=8KB)
.byte %00000000               ; Flags 6
;      ||||||| \ Mirroring (0: horizontal; 1: vertical)
;      |||||| \ Cartridge contains battery-backed PRG RAM
;      ||||| \ 512-byte trainer
;      |||| \ Provide four-scren VRAM
;       \\\\ Lower nybble of mapper number
.byte %00000000               ; Flags 7
;      ||||||| \ VS Unisystem
;      |||||| \ PlayChoice-10
;      |||| \\ If equal to 2, flags 8-15 are in NES 2.0 format
;       \\\\ Upper nybble of mapper number
.byte %00000000               ; Flags 8
;       \\\\\\\\ PRG-RAM size
.byte %00000000               ; Flags 9
;      ||||||| \ TV system (0: NTSC; 1: PAL)
;       \\\\\\\ Reserved, set to zero
.byte %00000000               ; Flags 10
;      ||||   \\ TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
;      || \\ PRG-RAM
;       \\ 0: Board has no bus conflicts; 1: Board has bus conflicts 
.byte $00,$00,$00,$00,$00


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESET:
    sei                       ; Disable all IRQ interrupts
    cld                       ; Clear decimal mode (unsupported by the NES)
    ldx #$FF
    txs                       ; Initialize the stack pointer at address $FF
    
    inx                       ; Increment X, causing a roll-off from $FF to $0
    stx PPU_CTRL              ; Disable NMI
    stx PPU_MASK              ; Disable rendering
    stx $4010                 ; Disable DMC IRQs
    
    lda #$40
    sta $4017                 ; Disable APU frame IRQ

Wait1stVBlank:                ; Wait for the first VBlank from the PPU
    bit PPU_STATUS            ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait1stVBlank         ; Loop until bit-7 (sign bit) is 1 (inside VBlank)

    txa                       ; A = 0
ClearRAM:
    sta $0000,x               ; Zero Ram addresses from $0000 to $00FF
    sta $0100,x               ; Zero Ram addresses from $0100 to $01FF
    sta $0200,x               ; Zero Ram addresses from $0200 to $02FF
    sta $0300,x               ; Zero Ram addresses from $0300 to $03FF
    sta $0400,x               ; Zero Ram addresses from $0400 to $04FF
    sta $0500,x               ; Zero Ram addresses from $0500 to $05FF
    sta $0600,x               ; Zero Ram addresses from $0600 to $06FF
    sta $0700,x               ; Zero Ram addresses from $0700 to $07FF
    inx                       ; X++
    bne ClearRAM              ; Loop until X reaches zero (after roll-off)

Wait2ndVBlank:                ; Wait for the first VBlank from the PPU
    bit PPU_STATUS            ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait2ndVBlank         ; Loop until bit-7 (sign bit) is 1 (inside VBlank)

Main:
    ldx #$3F
    stx PPU_ADDR             ; Set hi-byte of PPU_ADDR to $3F
    ldx #$00
    stx PPU_ADDR             ; Set lo-byte of PPU_ADDR to $00
    lda #$2A
    sta PPU_DATA             ; Send $2A (lime-green color code) to PPU_DATA
    lda #%00011110
    sta PPU_MASK             ; Set PPU_MASK bits to show background and sprites

LoopForever:
    jmp LoopForever          ; Force infinite loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    rti                       ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                       ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                     ; Address (2-bytes) of the NMI handler
.word RESET                   ; Address (2-bytes) of the Reset handler
.word IRQ                     ; Address (2-bytes) of the IRQ handler
