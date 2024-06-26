    PROCESSOR 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXpos         BYTE                ; player0 x-position
JetYpos         BYTE                ; player0 y-position
BomberXpos      BYTE                ; player1 x-position
BomberYpos      BYTE                ; player1 y-position
Score           BYTE                ; 2-digit score stored as BCD
Timer           BYTE                ; same as score, timer is right score in memory (this will help with programing)
Temp            BYTE                ; aux var to store temp values
OnesDigitOffset WORD                ; lookup table offset for the scores 1's digit
TensDigitOffset WORD                ; lookup table offset fot the scores 10's digit
JetSprtPtr      WORD                ; pointer to player0 sprite lookup table
JetClrPtr       WORD                ; pointer to player0 color lookup table
BmbrSprtPtr     WORD                ; pointer to player1 sprite lookup table
BmbrClrPtr      WORD                ; pointer to player1 color lookup table
JetAnim         BYTE                ; player0 sprite frame offset of animation
Random          BYTE                ; random number generated to set positions
ScoreSprite     BYTE                ; store the sprite bit pattern for the score
TimerSprite     BYTE                ; store the sprite bit pattern for the timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9                      ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9                   ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5                   ; scoreboard digit height (# rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memomry address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG Code
    ORG $F000

Reset:
    CLEAN_START                     ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #10
    STA JetYpos                     ; JetYPos = 10
    LDA #80
    STA JetXpos                     ; JetXPos = 60
    LDA #83
    STA BomberYpos                  ; Bomber Y posisition = 83
    LDA #62
    STA BomberXpos                  ; Bomber X posistion = 62
    LDA #%11010100
    STA Random                      ; random = $D4
    LDA #9
    STA Score
    LDA #5                       ;
    STA Timer                       ; Score and Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #<Jet
    STA JetSprtPtr                  ; lo-byte pointer for jet sprite lookup table
    LDA #>Jet
    STA JetSprtPtr+1                ; hi-byte pointer for jet sprite lookup table

    LDA #<ColorJet
    STA JetClrPtr                   ; lo-byte pointer for jet color lookup table
    LDA #>ColorJet
    STA JetClrPtr+1                 ; hi-byte pointer for jet color lookup table

    LDA #<Bomber
    STA BmbrSprtPtr                 ; lo-byte pointer for bomber sprite lookup table
    LDA #>Bomber
    STA BmbrSprtPtr+1               ; hi-byte pointer for bomber sprite lookup table

    LDA #<ColorBomber
    STA BmbrClrPtr                  ; lo-byte pointer for bomber color lookup table
    LDA #>ColorBomber
    STA BmbrClrPtr+1                ; hi-byte pointer for bomber color lookup table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK                      ; turn on VBLANK
    STA VSYNC                       ; turn on VSYNC
    REPEAT 3
        STA WSYNC                   ; display 3 recommended lines of VSYNC
    REPEND
    LDA #0
    STA VSYNC                       ; turn off VSYNC
    REPEAT 37
        STA WSYNC                   ; display 37 recommended lines of VBLANK (some clock cycles will reduce this number)
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations tasks performed in the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA JetXpos
    LDY #0
    JSR SetObjectXPos               ; set player0 horizontal position

    LDA BomberXpos
    LDY #1
    JSR SetObjectXPos               ; set player1 horizontal position

    JSR CalculateDigitOffset        ; calculate the scoreboard digit lookup table offset

    STA WSYNC
    STA HMOVE                       ; apply the horizontal offsets set by the subroutine


    LDA #0
    STA VBLANK                      ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #0                          
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1                        ; clear TIA registers before each new frame
    LDA #$0F                        ; set scoreboard color to white
    STA COLUPF
    LDA #$00
    STA COLUBK                      ; set the scoreboard background color to black
    LDA #%00000000
    STA CTRLPF                      ; disable playfield reflection

    LDX #DIGITS_HEIGHT              ; start x counter with 5 (height of the digits)

.ScoreDigitLoop:
    LDY TensDigitOffset             ; get the tens digit offset for the score
    LDA Digits,Y                    ; load the bit pattern from lookup table
    AND #$F0                        ; mask/remove the graphics for the ones digit
    STA ScoreSprite                 ; save the score tens digit pattern in a variable
    
    LDY OnesDigitOffset             ; get the one digit offset for the score
    LDA Digits,Y                    ; load the bit pattern from lookup table
    AND #$0F                        ; mask/remove the graphics for the tens digit
    ORA ScoreSprite                 ; merge it with the saved tens digit sprite
    STA ScoreSprite                 ; and save it
    STA WSYNC                       ; wait for the end of scanline
    sta PF1                         ; update the playfield to display the score sprite

    LDY TensDigitOffset+1           ; get the tens digit offest for the Timer
    LDA Digits,Y                    ; load the digit pattern from lookup table
    and #$F0                        ; mask/remove the graphics for the ones digit
    STA TimerSprite                 ; save the timer tens digit pattern in a variable

    LDY OnesDigitOffset+1           ; get the ones digit offset for the Timer
    LDA Digits,Y                    ; load digit pattern from lookup table
    AND #$0F                        ; mask/remove the graphics for the tens digit
    ORA TimerSprite                 ; merge with the saved tens digit graphics
    STA TimerSprite                 ; save it

    jsr Sleep12Cycles               ; subroutine to waste some cycles

    STA PF1                         ; update playfield for Timer display

    LDY ScoreSprite                 ; preload for the next scanline
    STA WSYNC                       ; wait for next scanline

    STY PF1                         ; update playfield for the score display
    INC TensDigitOffset
    INC TensDigitOffset+1
    INC OnesDigitOffset
    INC OnesDigitOffset+1           ; crement all digits for the next line of data

    JSR Sleep12Cycles               ; waste some cycles

    DEX                             ; X--
    STA PF1                         ; update the playfield for the Timer Display
    BNE .ScoreDigitLoop             ; if dex != 0, then branch to ScoreDigitLoop

    STA WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the (96) 192 visible scanlines of our main game (because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    LDA #$84                
    STA COLUBK                      ; set color background to blue
    LDA #$C2                
    STA COLUPF                      ; set the playfield/grass color to green

    LDA #%00000001
    STA CTRLPF                      ; enable playfield reflection
    
    LDA #$F0
    STA PF0                         ; setting PF0 bit pattern
    
    LDA #$FC
    STA PF1                         ; setting PF1 bit pattern

    LDA #0
    STA PF2                         ; setting PF2 bit pattern

    LDX #84                         ; X counts the number of remaining scanlines (84 because of the 20 scanlines for scoreboard)
                                    ;(96 for 2-line kernel, produces the streched sprites)
.GameLineLoop:
.AreWeInsideJetSprite:
    TXA                             ; transfer X to A
    SEC                             ; set the carry flag before subtraction
    SBC JetYpos                     ; subtract sprite Y-coords
    CMP JET_HEIGHT                  ; sprite height within bounds?
    BCC .DrawSpriteP0               ; if result < SpriteHeight, call the draw routine
    LDA #0                          ; else, set lookup index to zero
.DrawSpriteP0:
    CLC                             ; clear carry flag before addition
    ADC JetAnim                     ; set our sprite offset
    TAY                             ; transfer A to Y, Y is the only one that works with pointers (something),Y
    LDA (JetSprtPtr),Y              ; load player0 bitmap data from lookup table
    STA WSYNC                       ; wait for scanline
    STA GRP0                        ; set graphics for player0
    LDA (JetClrPtr),Y               ; load player0 color from lookup table
    STA COLUP0                      ; set color of player0

.AreWeInsideBomberSprite:
    TXA                             ; transfer X to A
    SEC                             ; set the carry flag before subtraction
    SBC BomberYpos                  ; subtract sprite Y-coords
    CMP BOMBER_HEIGHT               ; sprite height within bounds?
    BCC .DrawSpriteP1               ; if result < SpriteHeight, call the draw routine
    LDA #0                          ; else, set lookup index to zero
.DrawSpriteP1:
    TAY                             ; transfer A to Y, Y is the only one that works with pointers (something),Y

    LDA #%00000101
    STA NUSIZ1                      ; stretch player1 sprite

    LDA (BmbrSprtPtr),Y             ; load player0 bitmap data from lookup table
    STA WSYNC                       ; wait for scanline
    STA GRP1                        ; set graphics for player0
    LDA (BmbrClrPtr),Y              ; load player0 color from lookup table
    STA COLUP1                      ; set color of player0

    DEX                             ; X--
    BNE .GameLineLoop               ; repeat next main game scanline until finished

    LDA #0
    STA JetAnim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK                      ; turn on VBLANK
    REPEAT 30
        STA WSYNC                   ; displayer 30 recommended lines of VBLANK overscan
    REPEND
    LDA #0
    STA VBLANK                      ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    LDA #%00010000                  ; player0 joystick up
    BIT SWCHA                       
    BNE CheckP0Down                 ; if not up check down
    INC JetYpos                     ; if up Jet Y Pos + 1
    LDA JET_HEIGHT*3                ; load jet sprite height * 3
    STA JetAnim                     ; store that to the Jet Animation Pointer

CheckP0Down:
    LDA #%00100000                  ; player0 joystick down
    BIT SWCHA
    BNE CheckP0Left                 ; if not down check left
    DEC JetYpos                     ; if down Jet Y Pos - 1
    LDA JET_HEIGHT
    ASL
    ASL                             ; load jet sprite height * 4 (by shifting left twice we times by 4)
    STA JetAnim                     ; store that to the Jet Animation Pointer

CheckP0Left:
    LDA #%01000000                  ; player joystick left
    BIT SWCHA
    BNE CheckP0Right                ; if not left check right
    DEC JetXpos                     ; if left Jet X Pos - 1
    ;LDA #18                        ; JET_HEIGHT*2, and JET_HEIGHT+JET_HEIGHT caused a weird error (JET_HEIGHT = 9)
                                    ; never good coding pratice to have a set varible like this. If I have to change
                                    ; the sprite height I have to come back and remember this one
    LDA JET_HEIGHT
    ASL                             ; Loading the JET_HEIGHT and shifting left same as A * 2
    STA JetAnim                     ; store that to the Jet Animation Pointer

CheckP0Right:
    LDA #%10000000                  ; player joystick right
    BIT SWCHA
    BNE EndInputCheck               ; if not right end input check
    INC JetXpos                     ; if right Jet X Pos + 1
    LDA JET_HEIGHT                  ; A = 9
    STA JetAnim                     ; store that to the Jet Animation Pointer

EndInputCheck:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    LDA BomberYpos
    CLC
    CMP #0                          ; compare bomber y-position with 0
    BMI .ResetBomberPosition        ; if it is < 0 then reset y-position back to the top
    DEC BomberYpos                  ; else decrement player1 y-position for next frame
    JMP EndPositionUpdate

.ResetBomberPosition
    JSR GetRandomBomberPos          ; call subroutine for a random x-position

EndPositionUpdate:                  ; if y-position < 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    LDA #%10000000                  ; CXPPMM bit 7 detects P0 and P1 collision
    BIT CXPPMM                      ; check CXPPMM bit 7 with the above pattern
    BNE .CollisionP0P1              ; collision between P0 and P1 happened
    JMP CheckCollisionP0PF          ; no collisions with P0 and P1
.CollisionP0P1:
    JSR GameOver                    ; call GameOver subroutine
CheckCollisionP0PF:
    LDA #%10000000                  ; CXP0FB bit 7 detects P0 and PF collision
    BIT CXP0FB                      ; check CXP0FB with the above pattern
    BNE .CollisionP0PF              ; collision between P0 and PF happened
    JMP EndCollisionCheck           ; no collisions
.CollisionP0PF:
    JSR GameOver
EndCollisionCheck:                  ; fallback when no collision happans
    STA CXCLR                       ; clear all collisions flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame                  ; continue to display next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coord pos in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missle0, 3:missle1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos SUBROUTINE
    STA WSYNC                       ; start a fresh new scanline
    SEC                             ; set carry flag before subtraction
DivideLoop:
    SBC #15                         ; Subtract 15 from A
    BCS DivideLoop                  ; loop while carry flag is still set
    EOR #7                          ; adjust the remainder in A between -8 and 7
    ASL                             ; shift left by 4, as HMP0 uses only 4 bits
    ASL
    ASL
    ASL
    STA HMP0,Y                      ; set fine position value
    STA RESP0,Y                     ; reset rough position
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver SUBROUTINE
    LDA #$30
    STA COLUBK
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feadback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFST random number
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos SUBROUTINE
    LDA Random
    ASL
    EOR Random
    ASL
    EOR Random
    ASL
    ASL
    EOR Random
    ASL
    ROL Random                      ; performs a series of shifts and bit operations   

    LSR
    LSR                             ; divide the valve by 4 with 2 right shifts
    STA BomberXpos                  ; save it to the variable
    lda #30
    ADC BomberXpos                  ; adds 30 to compensate for left PF
    STA BomberXpos                  ; sets the new value to the player1 x-pos

    LDA #96
    STA BomberYpos                  ; set the y-pos to the top of the screen (96 because of 2-line kernel)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the balues can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5
;;  - we can use left shits to perform multiplications by 2
;;  - for any number N, the value of N=5 = (N=2=2)+N
;; For the upper nibble, since its already times 16. we need to devide it
;; and then multiply by 5
;;  - we can use right shifts to perform division by 2
;;  - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


CalculateDigitOffset SUBROUTINE
    LDX #1                          ; X register is the loop counter
.PrepareScoreLoop                   ; this will loop twice, first X = 1, next X = 0

    LDA Score,X                     ; load A with Timer (X=1) or Score (X=0)
    AND #$0F                        ; remove the tens digit by masking 4 bits %00001111
    STA Temp                        ; save the value of A into the Temp var
    ASL                             ; shift left A*2
    ASL                             ; shift left A*2
    ADC Temp                        ; add temp value
    STA OnesDigitOffset,X           ; save A in OnesDigitOffset + 1 or OnesDigitOffset

    LDA Score,X                     ; load A with Timer (X=1) or Score (X=0)
    AND #$F0                        ; remove the ones digit by masking 4 bits %11110000
    LSR                             ; shift right A/2
    LSR                             ; shift right A/2
    STA Temp                        ; save the value of A into the Temp var
    LSR                             ; Divide by 2
    LSR                             ; Divide by 2
    ADC Temp                        ; add the temp value (A/16)+(A/4)
    STA TensDigitOffset,X           ; store A in TensDigitOffset + 1 or TensDigitOffset

    DEX                             ; X--
    BPL .PrepareScoreLoop           ; while X >= 0, loop to pass a second time

    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSR takes 6 cycles
;; RTS takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles SUBROUTINE
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00110011                 ;  ##  ##
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %00100010                 ;  #   #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01100110                 ; ##  ##
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
Jet
    .byte #%00000000
    .byte #%00000000
    .byte #%00001000
    .byte #%00011100
    .byte #%01111111
    .byte #%00111110
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
JetR
    .byte #%00000000
    .byte #%00000000
    .byte #%00001000
    .byte #%00011100
    .byte #%01111110
    .byte #%00111100
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
JetL
    .byte #%00000000
    .byte #%00000000
    .byte #%00001000
    .byte #%00011100
    .byte #%00111111
    .byte #%00011110
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
JetU
    .byte #%00000000
    .byte #%00001000
    .byte #%00001000
    .byte #%00011100
    .byte #%01111111
    .byte #%00111110
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000
JetD
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00011100
    .byte #%01111111
    .byte #%00111110
    .byte #%00011100
    .byte #%00001000
    .byte #%00001000

Bomber
    .byte #%00000000
    .byte #%00000000
    .byte #%00001000
    .byte #%00001000
    .byte #%01111111
    .byte #%00001000
    .byte #%00011100
    .byte #%00111110
    .byte #%00010100
ColorJet
    .byte #$00
    .byte #$36
    .byte #$42
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$06
    .byte #$9A
    .byte #$04
ColorJetR
    .byte #$00
    .byte #$36
    .byte #$42
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$06
    .byte #$9A
    .byte #$04
ColorJetL
    .byte #$00
    .byte #$36
    .byte #$42
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$06
    .byte #$9A
    .byte #$04
ColorJetU
    .byte #$00
    .byte #$36
    .byte #$42
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$06
    .byte #$9A
    .byte #$04
ColorJetD
    .byte #$00
    .byte #$36
    .byte #$42
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$06
    .byte #$9A
    .byte #$04
ColorBomber
    .byte #$00
    .byte #$02
    .byte #$02
    .byte #$9A
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$42

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC                       ; move to position $FFFC
    WORD Reset                      ; write 2 bytes with the program reset address
    WORD Reset                      ; write 2 bytes with the interruption vector