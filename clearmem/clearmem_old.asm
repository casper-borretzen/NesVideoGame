;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the flags at $7FF0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.org $7FF0
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
.org $8000
; TODO: Add code of PRG-ROM

RESET:
    sei                       ; Disable all IRQ interrupts
    cld                       ; Clear the decimal mode flag (unsupported by the NES)
    ldx #$FF
    txs                       ; Initialize the stack pointer at $01FF
    
    lda #$0                   ; A = 0
    inx                       ; Increment X from $FF to $0
MemLoop:
    sta $0,x                  ; Store the value of A (zero) into $0+X
    dex                       ; X--
    bne MemLoop               ; If X is not zero, loop back to the MemLoop label

NMI:
    rti

IRQ:
    rti

.segment "VECTORS"
.org $FFFA
.word NMI
.word RESET
.word IRQ
