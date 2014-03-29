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
; Build it with DASM (http://dasm-dillon.sourceforge.net/)
;   dasm 2048.asm -o2048.bin -f3

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
GridColor:
   .byte $12,$22
TileColor:
   .byte $EC,$3C

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
ShowingMerged     = 5  ; => AddingRandomTile

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

NoGridColor = $00

TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)
GridSeparatorHeight = 10

GridPF0 = $00            ; Grid sides are always clear, minus last bit
GridPF1 = $01
GridPF2Tile  = %10011001 ; Grid has "holes" for numbers
GridPF2Space = %11111111 ; but is solid between the tiles

PlayerTwoCopiesWide = $02 ; P0 and P1 drawing tiles: 0 1 0 1
PlayerThreeCopies   = $03 ; P0 and P1 drawing score: 010101
VerticalDelay       = $01 ; Delays writing of GRP0/GRP1 for 6-digit score

AnimationFrames = 11  ; Time limit for merged tile / new tile animations

JoyP0Up    = %11100000      ; Masks to test SWCHA for joystick movement
JoyP0Down  = %11010000
JoyP0Left  = %10110000
JoyP0Right = %01110000
JoyMaskP0  = %11110000

ColSwitchMask = %00001000   ; Mask to test SWCHB for TV TYPE switch
GameResetMask = %00000001   ; Mask to test SWCHB for GAME RESET switch

; Amount to add to move to a direciton in the cell grid, in two's complement
RightShiftVector = $01     ;  1
LeftShiftVector  = $FF     ; -1
DownShiftVector  = $05     ;  5
UpShiftVector    = $FB     ; -5


;;;;;;;;;
;; RAM ;;
;;;;;;;;;

; The table has 16 cells + 13 sentinels = 29 (0x1D) bytes
CellTable = $80

; Loop counter for address of the "current" cell
CellCursor = $9D ;($80+$1D)  ;

; Frame count based RNG, used to add tiles and title screen rainbow
RandomNumber       = $9E

; Counter to display "animated" tiles (merged / new)
AnimationCounter   = $9F

; Added to the tile address to produce the merge animation
AnimationDelta     = $A0

; 6-digit score is stored in BCD (each nibble = 1 digit => 3 bytes)
ScoreBCD           = $A1

;:: SPACE ($A4, $A5)

; $A6-$A7 have different uses in various kernel routines:

TempVar1 = $A6               ; General use variable
TempVar2 = $A7               ; General use variable

LineCounter    = $A6         ; Counts lines while drawing the score
TempDigitBmp   = $A7         ; Stores intermediate part of 6-digit score

GameState = $A8;

; Tile shift routine variables
ShiftVector        = $A9     ; What to add to get to "next" tile in current direction
TilesLoopDirection = $AA     ; +1 for left/up, -1 for right/down
OffsetBeingPushed  = $AB     ; Position in cell table of the tile being pushed
ShiftEndOffset     = $AC     ; Position in which we'll stop processing
CurrentValue       = $AD     ; Value of that tile

; $B0-$BB will point to the address of the graphic for each
; digit (6x2 bytes) or tile (4x2 bytes) currently being drawn

; FIXME have just one rowtilebmp, maybe TileBmpPtr

DigitBmpPtr = $B0
RowTileBmp1 = $B0            ; Each of these points to the address of the
RowTileBmp2 = $B2            ; bitmap that will be drawn on the current/next
RowTileBmp3 = $B4            ; row of the grid, and must be updated before
RowTileBmp4 = $B6            ; the row is drawn


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

InitialValues:
    lda #$F0
    sta HMBL
    lda #$00
    sta REFP0
    sta REFP1

;;;;;;;;;;;;;;;;;;;
;; PROGRAM SETUP ;;
;;;;;;;;;;;;;;;;;;;

; Pre-fill the graphic poitner MSBs, so we only have to
; figure out the LSBs for each tile or digit
    lda #>Tiles        ; MSB of tiles/digits page
    ldx #11            ; 12-byte table (6 digits), zero-based
FillMsbLoop:
    sta RowTileBmp1,x
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

    lda #TitleScreen
    sta GameState

    lda #NoGridColor                         ; Hide the grid separator
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
    ldx #$00                     ; Colors change between NTSC and PAL
    lda #ColSwitchMask
    bit SWCHB
    bne NoColorPALAdjust
    inx
NoColorPALAdjust:
    lda GridColor,x
    sta COLUPF                    ; Show the grid separator

    lda TileColor,x               ; Tiles (players) with fixed color
    sta COLUP0
    sta COLUP1

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
    sta ScoreBCD
    sta ScoreBCD+1
    sta ScoreBCD+2

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
    lda GameState
    cmp #TitleScreen
    bne NoRainbow
    lda RandomNumber
    sta COLUP0
    eor #$FF
    sta COLUP1
NoRainbow:

;;;;;;;;;;;;;;;;;;;;;;
;; CONSOLE SWITCHES ;;
;;;;;;;;;;;;;;;;;;;;;;

    lda SWCHB                 ; GAME RESET restarts the game at any time
    bit GameResetMask
    beq Restart

    lda GameState             ; Fire button only restarts at title screen
    cmp #TitleScreen
    bne NoRestart
    lda INPT4
    bmi NoRestart
Restart:
    jmp StartNewGame
NoRestart:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POST-SHIFT MANAGEMENT (MERGE ANIMATION & SCORE UPDATE) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #ShowingMerged
    bne ResetAnimationCounter     ; No animation: just keep the counter ready
    dec AnimationCounter          ; Animating: decrease counter
    bne DoneCounterManagement
    lda #AddingRandomTile         ; Animation done, let's add a tile...
    sta GameState
    ldx FirstDataCellOffset       ; ...and clear merged bit from tiles
CountScoreLoop:
    lda CellTable,x
    cmp #MergedMask
    bmi ClearMergedBit            ; Not merged, just clear the bit
CountScore:
    and #ClearMergedMask
    asl
    tay                           ; Now Y = offset of tile values table
    sed                           ; We'll work in BCD

    clc
    lda TileValuesBCD-3,y
    adc ScoreBCD+2
    sta ScoreBCD+2                ; score "low byte" += table LSB

    lda TileValuesBCD-4,y
    adc ScoreBCD+1
    sta ScoreBCD+1                ; score "middle byte" += table MSB + carry

    lda #0
    adc ScoreBCD
    sta ScoreBCD                  ; score "high byte" += carry

    cld
    lda CellTable,x               ; Restore original value
ClearMergedBit:
    and #ClearMergedMask          ; Clear bit and store
    sta CellTable,x
    inx
    cpx #LastDataCellOffset+1
    bne CountScoreLoop            ; Go to the next, up to the last tile
ResetAnimationCounter:
    lda #AnimationFrames          ; Keep this counter initialized
    sta AnimationCounter          ; for the next animation
DoneCounterManagement:

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

    ldx #40
SpaceAboveLoop:
    sta WSYNC
    dex
    bne SpaceAboveLoop
    sta WSYNC

;;;;;;;;;;;;;;;;;
;; SCORE SETUP ;;
;;;;;;;;;;;;;;;;;

; Score setup scanline 1:
    lda GameState
    cmp #TitleScreen
    bne YesScore             ; No score on title screen

NoScore:
    ldx #12
ScoreSpaceLoop:
    sta WSYNC
    dex
    bne ScoreSpaceLoop
    jmp ScoreCleanup


; Score setup scanlines 2-3:
; player graphics triplicated and positioned like this: P0 P1 P0 P1 P0 P1

YesScore:
    sta WSYNC
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
    sta HMOVE         ; (3)

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

;;;;;;;;;;;;;;;;
;; GRID SETUP ;;
;;;;;;;;;;;;;;;;

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
; set graphic pointers' LSBs to the 4 tiles

GridRowPreparation:
    ldy #0             ; (2)   ; Y = column (*2) counter

UpdateTileBitmapAddressLoop:
    ldx CellCursor       ; (3) ; A = current grid cell value.
    lda CellTable,x      ; (4)
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
    sta RowTileBmp1,y  ; (5)   ; Store LSB (MSB is fixed)

    iny                ; (2)
    iny                ; (2)
    inc CellCursor     ; (5)
    sta WSYNC
    cpy #8             ; (2)
    bne UpdateTileBitmapAddressLoop ; (2 in branch fail)

; Separator scanline 9
; change playfield (after the beam draws the last separator one)
; and initialize counter

    REPEAT 18    ; (54 = 18x3)
        bit $00
    REPEND

    ldy #TileHeight-1  ; (2)   ; Initialize tile scanline counter
                               ; (goes downwards and is zero-based)

    lda #GridPF2Tile   ; (2)   ; Change to the "tile" playfield
    sta PF2            ; (3)

    ; no STA wsync (will do it in the grid row loop)

;;;;;;;;;;;;;;
;; GRID ROW ;;
;;;;;;;;;;;;;;

RowScanline:
    sta WSYNC
    REPEAT 7     ; (12 = 6x12)
        nop
    REPEND

    lda (RowTileBmp1),y
    sta GRP0
    lda (RowTileBmp2),y
    sta GRP1

    nop
    nop
    nop
    nop

; Here is the magic that makes A B A B into A B C D: when the beam is between
; the first copies and the second copies of the players, change the bitmaps:
    lda (RowTileBmp3),y
    sta GRP0
    lda (RowTileBmp4),y
    sta GRP1
    dey
    bpl RowScanline
    sta WSYNC

; Go to the next row (or finish grid)
    lda #0                   ; Disable player (tile) graphics
    sta GRP0
    sta GRP1
    lda #GridPF2Space        ; and return to the "space" playfield
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
    ldx #GridSeparatorHeight
DrawBottomSeparatorLoop:
    sta WSYNC
    dex
    bne DrawBottomSeparatorLoop

    lda #0                   ; Disable playfield (grid)
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTTOM SPACE BELOW GRID ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #50
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
    and #JoyMaskP0           ; Only player 0 bits

    ldx GameState            ; We only care for states in which we are waiting
    cpx #WaitingJoyRelease   ; for a joystick press or release
    beq CheckJoyRelease
    cpx #WaitingJoyPress
    bne EndJoyCheck

; If the joystick is in one of these directions, trigger the shift by
; setting the ShiftVector and changing mode
CheckJoyUp:
    cmp #JoyP0Up
    bne CheckJoyDown
    lda #UpShiftVector
    jmp TriggerShift

CheckJoyDown:
    cmp #JoyP0Down
    bne CheckJoyLeft
    lda #DownShiftVector
    jmp TriggerShift

CheckJoyLeft:
    cmp #JoyP0Left
    bne CheckJoyRight
    lda #LeftShiftVector
    jmp TriggerShift

CheckJoyRight:
    cmp #JoyP0Right
    bne EndJoyCheck
    lda #RightShiftVector

TriggerShift:
    sta ShiftVector            ; We'll need the direction vector on the shift
    lda #Shifting
    sta GameState
    jmp EndJoyCheck

CheckJoyRelease:
    cmp #JoyMaskP0
    bne EndJoyCheck

    lda #WaitingJoyPress       ; Joystick released, can accept shifts again
    sta GameState

EndJoyCheck:

;;;;;;;;;;;;;;;;;
;; SHIFT BOARD ;;
;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #Shifting
    beq SetDirection           ; Have to do this instead of bne EndShift
    jmp EndShift               ; because the routine is > 128 bytes!

; Outer loop will traverse the entire cell map in the *opposite* order of the
; movement, that is, from the beginning for right/up and from end for left/down,
; so they stack and merge as expected. Let's setup these parameters

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
    beq FinishShift               ; Processed all tiles, shift is done!
    jmp CheckIfXIsPushable        ; Check the new candidate

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
    sta GameState            ; animations (if any), which will add the new tile

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
    jmp MoveCurrentToNext    ; Move the multiplied cell to the target position

FinishShift:

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
