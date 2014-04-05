;
; 2048 2600
; =========
;
; A port of the 2048 game to the Atari 2600
;
; © 2014 Carlos Duarte do Nascimento (chesterbr)
; <cd@pobox.com | @chesterbr | http://chester.me>
;
; Latest version, contributors and general info:
;   http://github.com/chesterbr/2048-2060
;

; Building
; ---------
;
; You can build with DASM (http://dasm-dillon.sourceforge.net/), e.g.:
;
;   dasm 2048.asm -o2048.bin -f3
;
; However, you'll likely want to run graphics_gen.rb before. I've included
; my build script (build.sh), under the express condition that you don't
; mock my lame bash abilities :-P

; Cell Tables
; -----------
;
; The game store each player's tiles in a "cell table", which can contain one
; of these values:
;
;   0         = empty cell
;   1         = "2" tile
;   2         = "4" tile
;   3         = "8" tile
;   4         = "16" tile
;   ...
;   n         = "2ⁿ" tile (or, if you prefer: log₂k = "k" tile)
;   ...
;   11        = "2048" tile
;   12        = "4096" tile
;   13        = "8192" tile
;               (could go on, but try drawing a5-digit 8x10 tiles :-P )
;   128 ($FF) = sentinel tile (see below)
;
; In theory, we'd use 16 positions in memory for a 4x4 grid. Navigating
; left/right on the grid would mean subtracting/adding one position, and
; moving up/down would be done by asubtracting 4 positions (that
; is, "cell table y offset" would be 4)
;
; However, we'd need to do complicated boundaries checking, so instead I
; surround the grid with "sentinel" or "wall" tiles. That would theoretically
; need 20 extra cells (bytes) to store the grid:
;
;   first cell -> SSSSSS       S = sentinel, . = data (a tile or empty cell)
;     7th cell -> S....S
;    13rd cell -> S....S
;    19th cell -> S....S
;    25th cell -> S....S
;                 SSSSSS <- last (36th) cell
;
; But we can save some space by removing th left-side sentinels, since the
; memory position before those will be a sentinel anyway (the previous line's
; right-side sentinel).
;
; We can also cut the first and last sentinel (no movement can reach those),
; ending with with this layout in memory (notice how you still hit a
; sentinel if you try to leave the board in any direction):
;
;   first cell -> SSSSS        S = sentinel, . = data (a tile or empty cell)
;     6th cell -> ....S
;    11th cell -> ....S
;    16th cell -> ....S
;    21st cell -> ....S
;                 SSSS <- last (29th) cell
;
; Only change from usual 4x4 is the cell table vertical y offset is now 5 (we
; add/subtract 5 to go down/up). The first data cell is still the first cell
; plus vertical y offset
;
;
; Grid Drawing
; ------------
;
; The grid itself will be drawn using the TIA playfield, and the tiles
; with player graphics. The Atari only allows two of those graphics per
; scanline (although they can be repeated up to 3 times by the hardware),
; and we have four tiles per row, meaning we have to trick it[2] by:
;
;    - Load the graphic for tiles A and B:          "A"     and "B"
;    - Ask TIA to repeat each player graphic:       "A   A" and "B   B"
;    - Overlap their horizontal positions:          "A B A B"
;    - Load grpahic for tiles C and D when the      "A B C D"
;      TV beam is right halfway, that is, here: --------^
;
; First three staeps can be done just once when we start drawing the grid
; (TIA remembers the position), but the fourth must be repeated for every
; scanline. Timing is crucial on third and fourth steps, but that's Atari
; programming for you!
;
; To translate the cell table into a visual grid, we have to calculate, for
; each data cell, where the bitmap for its value (tile or empty space) is
; stored. We use the scanlines between each row of cells to do this calculation,
; meaning we need 8 RAM positions (4 cells per row x 2 bytes per address).
;
; We use the full address instead of a memory page offset to take advantage
; of the "indirect indexed" 6502 addressing mode [1], but we load the
; graphics table at a "page aligned" location (i.e., a "$xx00" address),
; so we only need to update the least significant byte on the positions above.
;
;
; Shifting
; --------
;
; The original 2048 sets a "vector" strucutre that points two variables with
; the direction in which the tiles will shift (e.g., vector.x = -1, vector.y
; = 0 means we'll move left). It also processes them from the opposite side
; (e.g., start from leftmost if it's a right shift), making the first one
; stop before the edge, the second stop before the first, etc.
;
; It also marks each merged tile as so (removing the marks between shifts),
; so it can block multiple merges (i.e., the row " 4 4 8 16" does not go
; straight to "32" with a left, but first becomes "8 8 16", then "16 16", then
; "32". Similarly, "2 2 2 2" would first become "4 4", then "8". Finally,
; it stores the previous position for each tile, and lets the awesomeness
; of CSS move them all at once with ease.
;
; We'll translate this idea by having a single-byte "vector" which can be
; -1/+1 for left/right, and -5/+5 for up/down (each row is 4 bytes plus a
; sentinel tile, see above). Each tile will be pushed (by adding the vector
; value) until the next cell is non-empty and does not match its value.
;
; The vector signal also tells us where to start to ensure they all get to
; the end: negative (left/up) start processing from the first cell and
; positive (right/down) start from the last.
;
; Merged tiles are marked by setting bit 7 on their values, which will be
; easy to check in upcoming pushed blocks without needing extra memory.
; We'll only miss the animations, but we can't have everything.
;
;
; Timings
; -------
;
; Since the shift routine can have unpredictable timing (and I wanted some
; freedom to move routines between overscan and vertical blank), I decided
; to use RIOT timers instead of the traditional scanline count. It is not
; the usual route for games (as they tend to squeeze every scanline of
; processing), but for this project it worked fine.
;
; [1] http://skilldrick.github.io/easy6502/
; [2] http://www.slideshare.net/chesterbr/atari-2600programming

    PROCESSOR 6502
    INCLUDE "vcs.h"
    ORG $F800                ; 2K cart

;;;;;;;;;;;;;;;;;
;; DATA TABLES ;;
;;;;;;;;;;;;;;;;;

; Tile and digit graphics go in the beginning of the cart to keep page-aligned
; (that is, the address' MSB never changes and we only calculate the LSB)

    INCLUDE "graphics.asm"                                                            ;

; Score values of each tile, in BCD
TileValuesBCD:
    .byte $00,$04
    .byte $00,$08
    .byte $00,$16
    .byte $00,$32
    .byte $00,$64
    .byte $01,$28
    .byte $02,$56
    .byte $05,$12
    .byte $10,$24
    .byte $20,$48
    .byte $40,$96
    .byte $81,$92

; Values that change if we are on PAL mode (TV TYPE switch "B•W" position)
; Order: NTSC, PAL. (thanks @SvOlli)
VBlankTime64T:
    .byte 44,74
OverscanTime64T:
    .byte 35,65

;;;;;;;;;;;;;;;
;; CONSTANTS ;;
;;;;;;;;;;;;;;;

; Special cell values (see header)
CellEmpty    = 0
Cell2        = 1
Cell4        = 2
Cell2048     = 11
CellSentinel = 127          ; Can't use bit 7 (will be wiped)

MergedMask      = %10000000 ; Bit 7 is set to flag tiles as merged
ClearMergedMask = %01111111 ; We need to reset to get to original values

; The original 2048 puts 2s with 90% probability and 4s with 10%. Our random
; number range between 0-255, so we'll put a 4 if it is above 256 * 0.9 ≅ 230
ThresholdForTile4 = 230

; Values of GameState (it's a state machine!)
TitleScreen       = 0  ; => AddingRandomTitle
AddingRandomTile  = 1  ; => WaitingJoyRelease
WaitingJoyRelease = 2  ; => WaitingJoyPress
WaitingJoyPress   = 3  ; => Shifting
Shifting          = 4  ; => ShowingMerged OR WaitingJoyRelease
ShowingMerged     = 5  ; => AddingRandomTile OR GameOverFX
GameOverFX        = 6  ; => GameOver
GameOver          = 7  ; => TitleScreen

; Values of GameMode
OnePlayerGame = 0
TwoPlayerGame = 1

; Some relative positions on the cell table
; Notice how we go to last data cell: Top-Left + 3 rows down + 3 columns right
; (FYI: add another row down and you'd have the last sentinel)
FirstDataCellOffset = 5
LastDataCellOffset  = FirstDataCellOffset + (CellTableYOffset * 3) + 3
LastCellOffset      = LastDataCellOffset + CellTableYOffset
CellTableYOffset    = 5  ; How much we +/- to move up/down a line on the table

; Position of sentinels that might be picked as random tiles (for being the
; ones on the right "wall", see drawing on header), relative to the
; first data cell offset.
; We'll replace them with the last three, allowing our random nibble to
; effectively map a random data cell
Wall1 = 4
Wall2 = 9
Wall3 = 14
Wall1Repl = 16
Wall2Repl = 17
Wall3Repl = 18

ScoreColor         = $28 ; Colors were chosen to get equal or equally nice
InactiveScoreColor = $04 ; on both PAL and NTSC, avoiding adjust branches
GridColor          = $04
BackgroundColor    = $00


TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)
GridBottomHeight = 5      ; Doesn't count the ones we use calculating P1's score

GridPF0 = $00            ; Grid sides are always clear, minus last bit
GridPF1 = $01
GridPF2Tile  = %10011001 ; Grid has "holes" for numbers
GridPF2Space = %11111111 ; but is solid between the tiles

PlayerTwoCopiesWide = $02 ; P0 and P1 drawing tiles: 0 1 0 1
PlayerThreeCopies   = $03 ; P0 and P1 drawing score: 010101
VerticalDelay       = $01 ; Delays writing of GRP0/GRP1 for 6-digit score

AnimationFrames = 11  ; Time limit for merged tile / new tile animations

JoyUp    = %11100000      ; Masks to test SWCHA for joystick movement
JoyDown  = %11010000      ; (we'll shift P1's bits into P0s on his turn, so
JoyLeft  = %10110000      ;  it's ok to use P0 values)
JoyRight = %01110000
JoyMask  = %11110000

ColSwitchMask   = %00001000  ; Mask to test SWCHB for TV TYPE switch
SelectResetMask = %00000011  ; Mask to test SWCHB for GAME SELECT/RESET switches
GameSelect      = %00000001  ; Value for GAME SELECT pressed (after mask)
GameReset       = %00000010  ; Value for GAME RESET  pressed (after mask)

; Amount to add to move to a direciton in the cell grid, in two's complement
RightShiftVector = $01     ;  1
LeftShiftVector  = $FF     ; -1
DownShiftVector  = $05     ;  5
UpShiftVector    = $FB     ; -5

TurnIndicatorFrames = 60   ; Special display for this # of frames if turn switches

MergeNoise = 4             ; Value of TurnNoise (AUDC0) if a merge happened
ShiftNoise = 1             ; Value of TurnNoise (AUDC0) if no merge happened

;;;;;;;;;
;; RAM ;;
;;;;;;;;;

; Some positions are shared between different coroutines
; (think of them as local variables)

CellTable = $80              ; 29 ($1D) bytes (16 cells + 13 sentinels)

CellCursor = $9D             ; Table offset of the "current" cell on grid setup

; Frame count based RNG, used to add tiles and title screen rainbow
RandomNumber       = $9E

; Counter to display "animated" tiles (merged / new)
AnimationCounter   = $9F

; Added to the tile address to produce the merge animation
AnimationDelta     = $A0

; 6-digit score is stored in BCD (each nibble = 1 digit => 3 bytes)
ScoreBCD           = $A1

GameMode = $A4               ; One or Two players

DidMerge = $A5               ; Nonzero if a merge happened on the last move

; $A6-$A7 have different uses in various kernel routines:

TempVar1 = $A6               ; General use variable
TempVar2 = $A7               ; General use variable

LineCounter  = $A6           ; Counts lines while drawing the score
TempDigitBmp = $A7           ; Stores intermediate part of 6-digit score

DidShift = $A6               ; True if a shift happened

GameState = $A8;

; Tile shift routine variables
ShiftVector        = $A9     ; What to add to get to "next" tile in current direction
TilesLoopDirection = $AA     ; +1 for left/up, -1 for right/down
OffsetBeingPushed  = $AB     ; Position in cell table of the tile being pushed
ShiftEndOffset     = $AC     ; Position in which we'll stop processing
CurrentValue       = $AD     ; Value of that tile

; Address of the graphic for for each digit (6x2 bytes)
; or tile (4x2 bytes) currently being drawn

DigitBmpPtr = $B0            ; 6 bytes
TileBmpPtr  = $B0            ; 4 bytes (2 wasted)

; Colors of each tile on the current row (4 bytes)
RowTileColor = $BC

; Store each player score separatedly and copy
; from/to ScoreBCD as needed to display, add, etc.
; Note: P1 score will store (and show) the high-score in single-player games
P0ScoreBCD = $C0             ; 3 bytes
P1ScoreBCD = $C3             ; 3 bytes

ScoreBeingDrawn = $C6        ; 0 for P0 or 1 for P1
CurrentPlayer = $C7          ; 0 for P0 or 1 for P1

LastSWCHB = $C8              ; Avoid multiple detection of console switches

TurnIndicatorCounter = $C9   ; Controls the time spent changing player turn

GameOverEffectCounter = $CA  ; Controls the time spent on game over effect

CurrentBGColor = $CB         ; Ensures invisible score keeps invisible during
                             ; game over "explosion"

DidMerge2048 = $CC           ; 0 if no 2048 was reached; 11 (Cell2048) if we did
Party2048Counter = $CD       ; 2048 effects counter (and ensures they play only once)

;;;;;;;;;;;;;;;
;; BOOTSTRAP ;;
;;;;;;;;;;;;;;;

Initialize:             ; Cleanup routine from macro.h (by Andrew Davie/DASM)
    sei
    cld
    ldx #0
    txa
    tay
CleanStack:
    dex
    txs
    pha
    bne CleanStack

;;;;;;;;;;;;;;;
;; TIA SETUP ;;
;;;;;;;;;;;;;;;

    lda #%00000001      ; Playfield (grid) in mirror (symmetrical) mode
    sta CTRLPF

;;;;;;;;;;;;;;;;;;;
;; PROGRAM SETUP ;;
;;;;;;;;;;;;;;;;;;;

; Pre-fill the graphic pointers' MSBs, so we only have to
; figure out the LSBs for each tile or digit
    lda #>Tiles        ; MSB of tiles/digits page
    ldx #11            ; 12-byte table (6 digits), zero-based
FillMsbLoop:
    sta TileBmpPtr,x
    dex                ; Skip to the next MSB
    dex
    bpl FillMsbLoop

; Initialize the cell table with sentinels, then fill
; the interior with empty cells
    ldx #LastCellOffset
    lda #CellSentinel
InitCellTableLoop1:
    sta CellTable,x
    dex
    bpl InitCellTableLoop1

;;;;;;;;;;;;;;;;;;
;; TITLE SCREEN ;;
;;;;;;;;;;;;;;;;;;

ShowTitleScreen:
    lda #TitleScreen
    sta GameState

    lda #BackgroundColor                     ; Hide the grid separator
    sta COLUPF

    ldx #LastDataCellOffset                  ; Print using tiles
TitleScreenLoop:
    lda TitleTiles-FirstDataCellOffset,x
    sta CellTable,x
    dex
    cpx #FirstDataCellOffset-1
    bne TitleScreenLoop
    jmp StartFrame

TitleTiles:
    .byte  1, 14,  2,  3, CellSentinel
    .byte  1, 15, 14, 14, CellSentinel
    .byte  0,  0,  0,  0, CellSentinel
    .byte  0,  0, 16, 17, CellSentinel

;;;;;;;;;;;;;;
;; NEW GAME ;;
;;;;;;;;;;;;;;

StartNewGame:
    lda GridColor
    sta COLUPF                    ; Show the grid separator

    ldx #LastDataCellOffset       ; Last non-sentinel cell offset
    lda #CellEmpty
InitCellTableLoop2Outer:
    ldy #4                        ; We'll clean 4 cells at a time
InitCellTableLoop2Inner:
    sta CellTable,x
    dex
    dey
    bne InitCellTableLoop2Inner
    dex                           ; skip 1 cell (side sentinel)
    cpx #FirstDataCellOffset
    bcs InitCellTableLoop2Outer   ; and continue until we pass the top-left cell

; Reset score
    lda #0
    ldx #5                        ; Reset both scores if on two-player game
    ldy GameMode
    cpy #TwoPlayerGame
    beq LoopResetScore
    ldx #2                        ; Only reset P0 if single-player (P1=record)
LoopResetScore:
    sta P0ScoreBCD,x
    dex
    bpl LoopResetScore

; Reset other variables
    sta CurrentPlayer
    sta CurrentBGColor
    sta Party2048Counter

; Start the game with a random tile
    lda #AddingRandomTile
    sta GameState

;;;;;;;;;;;;;;;;;
;; FRAME START ;;
;;;;;;;;;;;;;;;;;

StartFrame:
    lda #%00000010         ; VSYNC
    sta VSYNC
    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC
    sta WSYNC

    ldx #$00
    lda #ColSwitchMask     ; VBLANK start
    bit SWCHB
    bne NoVBlankPALAdjust  ; "Color" => NTSC; "B•W" = PAL
    inx                    ; (this ajust will appear a few times in the code)
NoVBlankPALAdjust:
    lda VBlankTime64T,x
    sta TIM64T             ; Use a RIOT timer (with the proper value) instead
    lda #0                 ; of counting scanlines (since we only care about
    sta VBLANK             ; the overall time)

;;;;;;;;;;;;;;;;;;;;;
;; NEW RANDOM TILE ;;
;;;;;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #AddingRandomTile
    bne EndRandomTile        ; No need for a random tile now

; Pick a random cell from a number from 0..15, mapping those that would
; hit sentinels on the right side ("walls") to the ones not covered by the range

    lda RandomNumber
    and #$0F
    cmp #Wall1
    bne NoWall1
    lda #Wall1Repl
NoWall1:
    cmp #Wall2
    bne NoWall2
    lda #Wall2Repl
NoWall2:
    cmp #Wall3
    bne CheckIfCellIsEmpty
    lda #Wall3Repl

CheckIfCellIsEmpty:
    tax                       ; Cell offset now in X
    lda CellTable+FirstDataCellOffset,x
    cmp #CellEmpty
    bne EndRandomTile         ; Tile not empty, let's try again next frame

PickTileType:
    ldy RandomNumber
    cpy #ThresholdForTile4
    bcc PickTile2
    lda #Cell4                ; >= threshold, A will be a "4" tile
    jmp AddTileToCell
PickTile2:
    lda #Cell2                ; < threshold, A will be a "2" tile

AddTileToCell:
    sta CellTable+FirstDataCellOffset,x
    lda #WaitingJoyRelease    ; Tile added, player can play as soon as they
    sta GameState             ; release the joystick

EndRandomTile:
    inc RandomNumber          ; Feed the random number generator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT, RESET AND P0 FIRE BUTTON ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx GameMode              ; Remember if we were on one or two player mode
    lda SWCHB                 ; We only want the switch presses once
    and #SelectResetMask      ; (in particular GAME SELECT)
    cmp LastSWCHB
    beq NoSwitchChange
    sta LastSWCHB             ; Store so we know if it's a repeat next time

    cmp #GameSelect           ; GAME SELECT flips single/multiplayer...
    bne NoSelect
    lda GameMode
    eor #1
    sta GameMode
    jmp StartNewGame          ; ...and restarts with no further game mode change
NoSelect:
    cmp #GameReset            ; GAME RESET restarts the game at any time
    beq Restart
NoSwitchChange:
    lda INPT4
    bpl ButtonPressed         ; P0 Fire button pressed?
    ldx #1                    ; P1 fire button always starts two-player game
    lda INPT5                 ; P1 fire button pressed?
    bmi NoRestart
ButtonPressed:
    lda GameState
    cmp #TitleScreen
    beq Restart               ; Start game if title screen
    cmp #GameOver             ; or game over
    bne NoRestart
Restart:
    stx GameMode
    jmp StartNewGame
NoRestart:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POST-SHIFT ACTION (MERGE ANIMATION, SCORE UPDATE, GAME OVER) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #ShowingMerged
    bne ResetAnimationCounter     ; No animation: just keep the counter ready
    lda AnimationCounter          ; Steadly decrease the shift tone,
    lsr                           ; giving it a "wall hit" feeling
    lsr
    sta AUDV0
    dec AnimationCounter          ; Decrease counter until animation ends
    bne DoneCounterManagement
; Finished animation: update score and add new tile
    lda #AddingRandomTile         ; Animation done, let's add a tile...
    sta GameState
    ldx FirstDataCellOffset       ; and count points for merged cells...
    lda #0
    sta DidMerge                  ; and set this flag if there were any
CountScoreLoop:
    lda CellTable,x
    cmp #MergedMask
    bmi ClearMergedBit            ; Not merged, just clear the bit
    sta DidMerge                  ; Flag that we had (at least one) merge
    and #ClearMergedMask          ; and get the value without the merge bit
    cmp #Cell2048
    bne CountScore
    sta DidMerge2048              ; If we merged a 2048, flag it
CountScore:
    asl
    tay                           ; Now Y = offset of tile values table
    sed                           ; We'll work in BCD
    lda CurrentPlayer
    bne AddScoreToP1
AddScoreToP0:
    clc
    lda TileValuesBCD-3,y
    adc P0ScoreBCD+2
    sta P0ScoreBCD+2                ; score "low byte" += table LSB

    lda TileValuesBCD-4,y
    adc P0ScoreBCD+1
    sta P0ScoreBCD+1                ; score "middle byte" += table MSB + carry

    lda #0
    adc P0ScoreBCD
    sta P0ScoreBCD                  ; score "high byte" += carry
    jmp DoneAddingScore
AddScoreToP1:
    clc
    lda TileValuesBCD-3,y
    adc P1ScoreBCD+2
    sta P1ScoreBCD+2                ; score "low byte" += table LSB

    lda TileValuesBCD-4,y
    adc P1ScoreBCD+1
    sta P1ScoreBCD+1                ; score "middle byte" += table MSB + carry

    lda #0
    adc P1ScoreBCD
    sta P1ScoreBCD                  ; score "high byte" += carry
DoneAddingScore:
    cld
    lda CellTable,x               ; Restore original value
ClearMergedBit:
    and #ClearMergedMask          ; Clear bit and store
    sta CellTable,x
    inx
    cpx #LastDataCellOffset+1
    bne CountScoreLoop            ; Go to the next, up to the last tile

; Change the turn to the other player, if appropriate
    lda GameMode
    cmp #TwoPlayerGame
    bne DoneCounterManagement     ; Single player always keeps their turn
    lda DidMerge
    bne DoneCounterManagement     ; Did merge, player keeps his turn
    lda CurrentPlayer
    eor #1                        ; Flip player (0<->1)
    sta CurrentPlayer
    lda #TurnIndicatorFrames      ; Show it's his turn
    sta TurnIndicatorCounter
ResetAnimationCounter:
    lda #AnimationFrames          ; Keep this counter initialized
    sta AnimationCounter          ; for the next animation
DoneCounterManagement:
; Reached first 2048? Let's party!
    lda DidMerge2048
    beq NoParty                   ; No 2048
    lda Party2048Counter
    bne NoParty                   ; Already had a party
    inc Party2048Counter          ; Let's party!
NoParty:


;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME OVER DETECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #WaitingJoyRelease
    bne EndGameOverDetection

    ldx #FirstDataCellOffset
FindAMoveLoop:
    lda CellTable,x               ; A = current cell value
    cmp #CellEmpty
    beq FoundAMove                ; Empty cell => can move
    inx
    cmp #CellSentinel
    beq FindAMoveLoopEnd          ; Sentinel, check next cell
    cmp CellTable,x               ; X = offset of cell to the right
    beq FoundAMove                ; Cell matches right neighbour => can merge
    inx
    inx
    inx
    inx                           ; X = offset of cell to the bottom (1+4=5)
    cmp CellTable,x
    beq FoundAMove                ; Cell matches bottom neighbour => can merge
    dex
    dex
    dex
    dex                           ; X = offset of next cell (1+4-4=1)
FindAMoveLoopEnd:
    cpx #LastDataCellOffset+1
    bne FindAMoveLoop             ; Iterate all tiles
; If we get here, no move was found => game over
    lda #GameOverFX               ; Start the game over effects...
    sta GameState
    lda #127                      ; with a centered...
    sta AUDF0
    lda #8                        ; explosion-ish...
    sta AUDC0
    lda #15                       ; loud sound
    sta AUDV0
    ldx #0
    stx GameOverEffectCounter
; For two-player mode, find the winner and make it current (=highlighted)
; For one-player mode, just update the high score (P1 Score)
    ldy GameMode
FindHigherScoreLoop:              ; Iterate over score byte until we can
    lda P0ScoreBCD,x              ; define whether P0 or P1 is higher
    cmp P1ScoreBCD,x
    beq ContinueFindHigherScore   ; Can't tell from this byte, keep looping
    bcc P1Higher
P0Higher:
    lda #0
    cpy #TwoPlayerGame
    beq SetTurnToWinner           ; Two-player game: P0 wins, make it current.
    ldx P0ScoreBCD                ; One-player game: we have a new record,
    stx P1ScoreBCD                ; copy it to the P1 (high score)
    ldx P0ScoreBCD+1
    stx P1ScoreBCD+1
    ldx P0ScoreBCD+2
    stx P1ScoreBCD+2
    jmp EndGameOverDetection
P1Higher:
    cpy #OnePlayerGame
    beq EndGameOverEffects        ; One-player game: no new record, we're done
    lda #1                        ; Two-Player game: P1 wins, make it current.
    jmp SetTurnToWinner
ContinueFindHigherScore:
    inx
    cpx #3
    bne FindHigherScoreLoop
BothAreLosers:
    lda #99                       ; In a tie, no one will be bright
SetTurnToWinner:
    sta CurrentPlayer
FoundAMove:
EndGameOverDetection:

;;;;;;;;;;;;;;;;;;;;;;;
;; GAME OVER EFFECTS ;;
;;;;;;;;;;;;;;;;;;;;;;;
    lda GameState
    cmp #GameOverFX
    bne EndGameOverEffects
    lda RandomNumber              ; Flash the background
    sta COLUBK
    sta CurrentBGColor
    inc GameOverEffectCounter     ; Keep on for ~2.3s (+/-0.2 for PAL/NTSC diff)
    bpl EndGameOverEffects
    lda #BackgroundColor          ; Now game is *really* over, cut the effects
    sta COLUBK
    sta CurrentBGColor
    lda #0
    sta AUDV0
    lda #GameOver
    sta GameState
EndGameOverEffects:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2048 "PARTY" EFFECTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda Party2048Counter
    beq DonePartyCheck           ; No party yet
    bmi DonePartyCheck           ; Party is over
    lda #04                      ; Party!
    sta AUDC1
    lda Party2048Counter
    sta AUDF1
    sta COLUPF
    lda #10
    sta AUDV1
    inc Party2048Counter
    bpl DonePartyCheck
    lda #0                       ; End of party
    sta AUDV1
    lda GridColor
    sta COLUPF
DonePartyCheck:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER FRAME CONFIGURATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #0                       ; First score to show is P0's
    sta ScoreBeingDrawn          ; (P1 will come after the grid)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMAINDER OF VBLANK ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

WaitForVBlankEndLoop:
    lda INTIM                ; Wait until the timer signals the actual end
    bne WaitForVBlankEndLoop ; of the VBLANK period

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP SPACE ABOVE SCORE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #36
SpaceAboveLoop:
    sta WSYNC
    dex
    bne SpaceAboveLoop
    sta WSYNC

;;;;;;;;;;;;;;;;;
;; SCORE SETUP ;;
;;;;;;;;;;;;;;;;;

ScoreSetup:
; Score setup scanline 1:
; general configuration
    lda GameState
    cmp #TitleScreen
    bne YesScore             ; No score on title screen

NoScore:
    ldx #13
ScoreSpaceLoop:
    sta WSYNC
    dex
    bne ScoreSpaceLoop
    jmp ScoreCleanup

YesScore:
    lda #0                   ; No players until we start
    sta GRP0
    sta GRP1
    lda ScoreBeingDrawn      ; Copy the proper score to display
    bne ReadScoreP1
ReadScoreP0:
    lda P0ScoreBCD
    ldx P0ScoreBCD+1
    ldy P0ScoreBCD+2
    jmp WriteScore
ReadScoreP1:
    lda P1ScoreBCD
    ldx P1ScoreBCD+1
    ldy P1ScoreBCD+2
WriteScore:
    sta ScoreBCD
    stx ScoreBCD+1
    sty ScoreBCD+2
    sta WSYNC

; Score setup scanlines 2-3:
; player graphics triplicated and positioned like this: P0 P1 P0 P1 P0 P1
; also, set their colors

    lda #PlayerThreeCopies   ; (2)
    sta NUSIZ0               ; (3)
    sta NUSIZ1               ; (3)

    lda #VerticalDelay       ; (2) ; Needed for precise timing of GRP0/GRP1
    sta VDELP0               ; (3)
    sta VDELP1               ; (3)

    REPEAT 10    ; (20=10x2) ; Delay to position right
        nop
    REPEND
    sta RESP0   ; (3)        ; Position P0
    sta RESP1   ; (3)        ; Position P1
    sta WSYNC

    lda #$E0                 ; Fine-tune player positions to center on screen
    sta HMP0
    lda #$F0
    sta HMP1
    sta WSYNC
    sta HMOVE   ; (3)

    ldx #ScoreColor          ; Animate score for a few seconds when the
    lda TurnIndicatorCounter ; turn changes
    beq NoTurnAnimation
    adc #ScoreColor
    tax
    dec TurnIndicatorCounter
NoTurnAnimation:
    lda ScoreBeingDrawn      ; If score drawn belongs to the current player,
    cmp CurrentPlayer        ; it is always shown as active
    beq SetScoreColor

    lda GameState            ; If game is over, always show both scores
    cmp #GameOver            ; (because P1 is the high score)
    beq ShowAsInactive

    ldx CurrentBGColor       ; Get rid of score if not current and on single
    lda GameMode             ; player game (in which P0 is always current),
    cmp #OnePlayerGame       ; otherwise show as inactive
    beq SetScoreColor

ShowAsInactive:
    ldx #InactiveScoreColor
SetScoreColor:
    stx COLUP0
    stx COLUP1


; Score setup scanlines 4-5
; set the graphic pointers for each score digit

    ldy #2            ; (2)  ; Score byte counter (source)
    ldx #10           ; (2)  ; Graphic pointer counter (target)
    clc               ; (2)

ScorePtrLoop:
    lda ScoreBCD,y    ; (4)
    and #$0F          ; (2)  ; Lower nibble
    sta TempVar1      ; (3)
    asl               ; (2)  ; A = digit x 2
    asl               ; (2)  ; A = digit x 4
    adc TempVar1      ; (3)  ; 4.digit + digit = 5.digit
    adc #<Digits      ; (2)  ; take from the first digit
    sta DigitBmpPtr,x ; (4)  ; Store lower nibble graphic
    dex               ; (2)
    dex               ; (2)

    lda ScoreBCD,y    ; (4)
    and #$F0          ; (2)
    lsr               ; (2)
    lsr               ; (2)
    lsr               ; (2)
    lsr               ; (2)
    sta TempVar1      ; (3)  ; Higher nibble
    asl               ; (2)  ; A = digit x 2
    asl               ; (2)  ; A = digit x 4
    adc TempVar1      ; (3)  ; 4.digit + digit = 5.digit
    adc #<Digits      ; (2)  ; take from the first digit
    sta DigitBmpPtr,x ; (4)  ; store higher nibble graphic
    dex               ; (2)
    dex               ; (2)
    dey               ; (2)
    bpl ScorePtrLoop  ; (2*)
    sta WSYNC         ;      ; We take less than 2 scanlines, round up

; We may have been drawing the end of the grid (if it's P1 score)
    lda #0
    sta PF0
    sta PF1
    sta PF2

;;;;;;;;;;;
;; SCORE ;;
;;;;;;;;;;;

    ldy #4                   ; 5 scanlines
    sty LineCounter
DrawScoreLoop:
    ldy LineCounter          ; 6-digit loop is heavily inspired on Berzerk's
    lda (DigitBmpPtr),y
    sta GRP0
    sta WSYNC
    lda (DigitBmpPtr+2),y
    sta GRP1
    lda (DigitBmpPtr+4),y
    sta GRP0
    lda (DigitBmpPtr+6),y
    sta TempDigitBmp
    lda (DigitBmpPtr+8),y
    tax
    lda (DigitBmpPtr+10),y
    tay
    lda TempDigitBmp
    sta GRP1
    stx GRP0
    sty GRP1
    sta GRP0
    dec LineCounter
    bpl DrawScoreLoop

ScoreCleanup:                ; 1 scanline
    lda #0
    sta VDELP0
    sta VDELP1
    sta GRP0
    sta GRP1
    sta WSYNC

    lda ScoreBeingDrawn
    beq GridSetup           ; If showing P0 score, the grid follows,
    jmp FrameBottomSpace    ; otherwise, we're done with the frame

;;;;;;;;;;;;;;;;
;; GRID SETUP ;;
;;;;;;;;;;;;;;;;

GridSetup:
; Separator scanline 1:
; configure grid playfield
    lda #GridPF0
    sta PF0
    lda #GridPF1
    sta PF1
    lda #GridPF2Space        ; Space between rows
    sta PF2

; point cell cursor to the first data cell
    lda #FirstDataCellOffset
    sta CellCursor

    sta WSYNC

; Separator scanlines 2-4:
; player graphics duplicated and positioned like this: P0 P1 P0 P1

    lda #PlayerTwoCopiesWide ; (2)
    sta NUSIZ0               ; (3)
    sta NUSIZ1               ; (3)

    REPEAT 9    ; (27 = 9x3) ; Position P0 close to the beginning of 1st tile
        bit $80
    REPEND
    sta RESP0   ; (3)

    bit $80     ; (3)        ; and P1 close to the beginning of the second
    sta RESP1   ; (3)
    sta WSYNC

    lda #$F0                 ; Fine-tune player positions to fill the grid
    sta HMP0
    lda #$10
    sta HMP1
    sta WSYNC
    sta HMOVE
    sta WSYNC

; Separator scanlines 5-8:
; point graphic pointers' LSBs to the next 4 tiles

GridRowPreparation:
    lda #
    ldy #0             ; (2)   ; Y = column (*2) counter

UpdateTileBitmapAddressLoop:
    ldx CellCursor       ; (3)
    lda CellTable,x      ; (4) ; A = current grid cell value.
    ldx #0
    cmp #MergedMask
    bcc MultiplyBy11
    and #ClearMergedMask ; (2) ; Clear the merged bit
    ldx AnimationCounter

MultiplyBy11:
    stx AnimationDelta
    ; We need to multiply the value ("n") by 11 (TileHeight).
    sta TempVar1       ; (3)   ; TempVar1 = value

    asl                ; (2)
    asl                ; (2)
    asl                ; (2)
    sta TempVar2       ; (3)   ; TempVar2 = 8*value

    clc                ; (2)
    lda TempVar1       ; (3)
    adc TempVar1       ; (2)
    adc TempVar1       ; (2)   ; A = 3*value
    adc TempVar2       ; (2)   ; A = 3*value + 8*value = 11*value
    sec
    sbc AnimationDelta ; (2)   ; If animating, scroll to the new value

MultiplicationDone:
    sta TileBmpPtr,y   ; (5)   ; Store LSB (MSB is fixed)

    iny                ; (2)
    iny                ; (2)
    inc CellCursor     ; (5)
    sta WSYNC
    cpy #8             ; (2)
    bne UpdateTileBitmapAddressLoop ; (2 in branch fail)

; Separator scanlines 9-10
; Set tile colors according to their values
; (or, in title screen, according to the position)

    ldx CellCursor
    lda GameState
    cmp #TitleScreen
    bne ColorsFromValues          ; Not in title screen => tile values
    cpx #FirstDataCellOffset+19
    bpl FixedColors               ; Title 4th     row => fixed values
    cpx #FirstDataCellOffset+12
    bpl ColorsFromValues          ; Title 3rd     row => tile values (=0)
ColorsFromRainbow:                ; Title 1st/2nd row => rainbow
    lda RandomNumber
    sta RowTileColor
    adc #10
    adc CellCursor
    sta RowTileColor+1
    adc #10
    adc CellCursor
    sta RowTileColor+2
    adc #10
    adc CellCursor
    sta RowTileColor+3
    sta WSYNC
    jmp DoneWithColors
FixedColors:
    sta WSYNC
    lda #BackgroundColor
    sta RowTileColor
    sta RowTileColor+1
    lda #ScoreColor
    sta RowTileColor+2
    sta RowTileColor+3
    ldx #3
PositionChesterTilesLoop:
    nop
    dex
    bne PositionChesterTilesLoop
    sta RESP0
    jmp DoneWithColors
ColorsFromValues:
    ldy #3
SetColorLoop:
    dex
    lda CellTable,x      ; A = current cell value.
    asl
    asl
    asl
    asl
    beq StoreColor
    adc #8               ; If not empty, add some luminance
StoreColor:
    sta RowTileColor,y   ; Y = current color table offset
    dey
    bpl SetColorLoop
DoneWithColors:
    sta WSYNC

; Last separator scanline:
; change playfield (after the beam draws the last separator one)
; and initialize counter

    ldx RowTileColor+3        ; (3)  ; Will be used on grid row
    txs                       ; (2)  ; to save a cycle

    ldy #9                    ; (2)  ; Wait till beam is past the board
LastSeparatorLineLoop:
    dey                       ; (2)
    bne LastSeparatorLineLoop ; (2*) (3 except last)

    ldy #TileHeight-1  ; (2)   ; Initialize tile scanline counter
                               ; (goes downwards and is zero-based)

    lda #GridPF2Tile   ; (2)   ; Change to the "tile" playfield
    sta PF2            ; (3)

    ; no STA wsync (will do it in the grid row loop)

;;;;;;;;;;;;;;
;; GRID ROW ;;
;;;;;;;;;;;;;;

RowScanline:
    ldx RowTileColor+2    ; (3)
    lda RowTileColor      ; (3)
    sta COLUP0            ; (3)
    sta WSYNC             ; (3)
    lda RowTileColor+1    ; (3)
    sta COLUP1            ; (3)
    lda (TileBmpPtr+6),y  ; (5)
    sta TempVar1          ; (3)
    nop                   ; (2)
    nop                   ; (2)
    lda (TileBmpPtr),y    ; (5)
    sta GRP0              ; (3)
    lda (TileBmpPtr+2),y  ; (5)
    sta GRP1              ; (3)
    lda (TileBmpPtr+4),y  ; (5)
    sta GRP0              ; (3)
    lda TempVar1          ; (3)
    stx COLUP0            ; (3)
    sta GRP1              ; (3)
    tsx                   ; (2)
    stx COLUP1            ; (3)
    dey                   ; (2)
    bpl RowScanline       ; (2*)
    sta WSYNC

; Go to the next row of tiles (or finish grid)
    lda #0                   ; Disable player (tile) graphics
    sta GRP0
    sta GRP1
    lda #GridPF2Space        ; and return to the "separator" playfield
    sta PF2

    inc CellCursor           ; Advance cursor (past the side sentinel)
    ldx CellCursor           ; and get its value
    lda CellTable,x

    cmp #CellSentinel        ; If it's a sentinel, move on
    beq FinishGrid

    sta WSYNC                ; otherwise just skip the setup and prepare
    sta WSYNC                ; another batch of tiles to display
    sta WSYNC
    jmp GridRowPreparation

FinishGrid:
    ldx #GridBottomHeight    ; We'll draw a part of the last separator here;
DrawBottomSeparatorLoop:     ; the remainder will be drawn during P1 score
    sta WSYNC                ; calculation
    dex
    bne DrawBottomSeparatorLoop

    inc ScoreBeingDrawn      ; Display score for P1 (even if invisible)
    jmp ScoreSetup


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTTOM SPACE BELOW GRID ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FrameBottomSpace:
    ldx #36
SpaceBelowGridLoop:
    sta WSYNC
    dex
    bne SpaceBelowGridLoop

;;;;;;;;;;;;;;
;; OVERSCAN ;;
;;;;;;;;;;;;;;

    lda #%01000010           ; Disable output
    sta VBLANK
    ldx #$00
    lda #ColSwitchMask
    bit SWCHB
    bne NoOverscanPALAdjust
    inx
NoOverscanPALAdjust:
    lda OverscanTime64T,x    ; Use a timer adjusted to the color system's TV
    sta TIM64T               ; timings to end Overscan, same as VBLANK

;;;;;;;;;;;;;;;;;;;;
;; INPUT CHECKING ;;
;;;;;;;;;;;;;;;;;;;;

; Joystick
    lda SWCHA
    ldx CurrentPlayer
    beq VerifyGameStateForJoyCheck
    asl                      ; If it's P1's turn, put their bits where P0s
    asl                      ; would be
    asl
    asl
VerifyGameStateForJoyCheck:
    and #JoyMask           ; Only player 0 bits

    ldx GameState            ; We only care for states in which we are waiting
    cpx #WaitingJoyRelease   ; for a joystick press or release
    beq CheckJoyRelease
    cpx #WaitingJoyPress
    bne EndJoyCheck

; If the joystick is in one of these directions, trigger the shift by
; setting the ShiftVector and changing mode
CheckJoyUp:
    cmp #JoyUp
    bne CheckJoyDown
    lda #UpShiftVector
    jmp TriggerShift

CheckJoyDown:
    cmp #JoyDown
    bne CheckJoyLeft
    lda #DownShiftVector
    jmp TriggerShift

CheckJoyLeft:
    cmp #JoyLeft
    bne CheckJoyRight
    lda #LeftShiftVector
    jmp TriggerShift

CheckJoyRight:
    cmp #JoyRight
    bne EndJoyCheck
    lda #RightShiftVector

TriggerShift:
    sta ShiftVector            ; We'll need the direction vector on the shift
    lda #Shifting
    sta GameState
    jmp EndJoyCheck

CheckJoyRelease:
    cmp #JoyMask
    bne EndJoyCheck

    lda #WaitingJoyPress       ; Joystick released, can accept shifts again
    sta GameState

EndJoyCheck:

;;;;;;;;;;;;;;;;;
;; SHIFT BOARD ;;
;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #Shifting
    beq StartShift             ; Have to do this instead of bne EndShift
    jmp EndShift               ; because the routine is > 128 bytes!

; Outer loop will traverse the entire cell map in the *opposite* order of the
; movement, that is, from the beginning for right/up and from end for left/down,
; so they stack and merge as expected. Let's setup these parameters

StartShift:
    lda #0                     ; So far, no merges happened
    sta DidMerge

SetDirection:
    lda ShiftVector
    bmi NegativeVector

PositiveVector:
    ldx #LastDataCellOffset    ; Start from the last cell
    ldy #FirstDataCellOffset-1 ; Stop when we pass the first one
    lda #$FF                   ; Go backwards (-1)
    jmp SetShiftParams

NegativeVector:
    ldx #FirstDataCellOffset   ; Start from the first cell
    ldy #LastDataCellOffset+1  ; Stop when we pass the last one
    lda #$01                   ; Go forward (+1)

SetShiftParams:
    sty ShiftEndOffset
    sta TilesLoopDirection

    lda #WaitingJoyRelease     ; We won't animate or add tiles unless some
    sta GameState              ; movement happens

; Notice that X will keep the offset of the cell being processed (main loop),
; and will advance until we have no more processable tile positions.
;
; Whenever we have a "good" tile on X (not empty or sentinel), we'll start
; pushing it on the vector direciton (inner/"push" loop), until we can't
; push anyomre.

CheckIfXIsPushable:
    lda CellTable,x
    cmp #CellSentinel
    beq AdvanceToNext             ; Skip empty cells
    cmp #CellEmpty
    beq AdvanceToNext             ; Skip sentinels
    stx OffsetBeingPushed
    jmp StartPush                 ; This one is good, start pushing it

AdvanceToNext:
    txa                           ; Move X to the next candidate offset
    clc
    adc TilesLoopDirection
    tax
    cpx ShiftEndOffset
    beq TurnNoiseSetup            ; Processed all tiles, shift is done!
    jmp CheckIfXIsPushable        ; Check the new candidate
TurnNoiseSetup:
    lda GameState
    cmp #WaitingJoyRelease
    beq EndShift                  ; No shift => no noise
    ldx #ShiftNoise
    lda DidMerge
    beq StartNoise                ; Shift with no merge => shift noise
    ldx #MergeNoise               ; Shift & merge => merge noise
StartNoise:
    stx AUDC0
    lda #90
    sta AUDF0
    jmp EndShift

; Inner loop will push the tile currenlty picked by the outer loop towards
; the desired direction, until hitting an unmergeable tile

StartPush:
    lda CellTable,x
    sta CurrentValue              ; Keep the current tile's value
    stx OffsetBeingPushed         ; Initialize inner loop counter

PushCurrentTile:                  ; Inner loop begins here
    clc
    lda OffsetBeingPushed
    adc ShiftVector
    tay
    lda CellTable,y          ; A <= value of next cell in the vector direction

    cmp #CellEmpty
    bne NotEmpty             ; We won't move if the next cell is not empty

MoveCurrentToNext:
    lda #ShowingMerged       ; If we move at least once, we can show merged
    sta GameState            ; animations (if any)

    lda CurrentValue
    sta CellTable,y          ; Set next cell to current value

    lda #CellEmpty
    ldy OffsetBeingPushed
    sta CellTable,y          ; Clear current cell

    clc
    lda OffsetBeingPushed
    adc ShiftVector
    sta OffsetBeingPushed    ;   - make next the one being pushed

    jmp PushCurrentTile      ; Keep pushing

NotEmpty:
    cmp #MergedMask
    bcs AdvanceToNext        ; Can't merge if next cell has already been merged
    cmp CurrentValue
    bne AdvanceToNext        ; Only merge if value matches

Merge:
    inc CurrentValue         ; Multiply by 2 in log (that is, add 1 to exponent)
    lda #MergedMask
    ora CurrentValue         ; Add the "merged" bit (so it doesn't match others)
    sta CurrentValue
    sta DidMerge             ; Signal the merge
    jmp MoveCurrentToNext    ; Move the multiplied cell to the target position

EndShift:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMAINDER OF OVERSCAN ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

WaitForOverscanEndLoop:
    lda INTIM                   ; Wait until the timer signals the actual end
    bne WaitForOverscanEndLoop  ; of the overscan period

    sta WSYNC
    jmp StartFrame

    ORG $FFFA

    .WORD Initialize
    .WORD Initialize
    .WORD Initialize

    END

; The MIT License (MIT)

; Copyright (c) 2014 Carlos Duarte do Nascimento (Chester)
;
; Original 2048 game Copyright (c) 2014 Gabriele Cirulli

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
