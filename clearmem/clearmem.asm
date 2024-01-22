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

RESET:
    sei                       ; Disable all IRQ interrupts
    cld                       ; Clear the decimal mode flag (unsupported by the NES)
    ldx #$FF
    txs                       ; Initialize the stack pointer at $01FF
    
    inx                       ; Increment X, causing a roll-off from $FF to $0
    txa                       ; A = 0

;; Clears entire system RAM region between $0000-$07FF
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
    bne ClearRAM              ; Loops until X reaches zero again (after roll-off)

LoopForever:
    jmp LoopForever           ; Force infinite loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                     ; Address (2-bytes) of the NMI handler
.word RESET                   ; Address (2-bytes) of the Reset handler
.word IRQ                     ; Address (2-bytes) of the IRQ handler
