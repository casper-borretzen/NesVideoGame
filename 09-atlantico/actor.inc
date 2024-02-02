;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumeration with the different types of actor of our game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum ActorType
    NULL = 0
    PLAYER = 1
    SUBMARINE = 2
    AIRPLANE = 3
    MISSILE = 4
    BOMB = 5
    SPRITE0 = 6
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct to hold the data for one actor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.struct Actor
    Type   .byte
    XPos   .byte
    YPos   .byte
    Screen .byte
.endstruct
