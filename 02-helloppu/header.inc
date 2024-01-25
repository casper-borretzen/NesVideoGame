;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with flags at address $7FF0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.byte $4E,$45,$53,$1A         ; 4 bytes that spell out NES
;       \N  \E  \S  \NEW LINE
.byte $02                     ; Number of 16KB PRG-ROM to be used (2=32KB)
.byte $01                     ; Number of 8KB CHR-ROM to be used
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
.byte $00,$00,$00,$00,$00     ; Unused padding (should be filled with zero)
