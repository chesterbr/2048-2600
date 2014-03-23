;
; 2048 2600
; =========
;
; A port of the 2048 game to the Atari 2600
;
; © 2014 Carlos Duarte do Nascimento (chesterbr)
; <cd@pobox.com | @chesterbr | http://chester.me>
;
; Latest version:
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
; [1] http://skilldrick.github.io/easy6502/
; [2] http://www.slideshare.net/chesterbr/atari-2600programming

    PROCESSOR 6502
    INCLUDE "vcs.h"

    ORG $F000                ; We'll include the tile bitmaps at a known and
    INCLUDE "graphics.asm"   ; aligned address, so we only calculate the LSB

;;;;;;;;;
;; RAM ;;
;;;;;;;;;

RowTileBmp1 = $80            ; Each of these points to the address of the
RowTileBmp2 = $82            ; bitmap that will be drawn on the current/next
RowTileBmp3 = $84            ; row of the grid, and must be updated before
RowTileBmp4 = $86            ; the row is drawn

CellTable = $88              ; 16 cells + 13 sentinels = 29 (0x1D) bytes

CellCursor = $A5 ;($88+$1D)  ; Loop counter for address of the "current" cell

TempVar1 = $A6               ; General use variable
TempVar2 = $A7               ; General use variable

GameState = $A8;

; Tile shift routine variables
ShiftVector        = $A9     ; What to add to get to "next" tile in current direction
TilesLoopDirection = $AA     ; +1 for left/up, -1 for right/down
OffsetBeingPushed  = $AB     ; Position in cell table of the tile being pushed
ShiftEndOffset     = $AC     ; Position in which we'll stop processing
CurrentValue       = $AD     ; Value of that tile

RandomNumber       = $AE     ; Frame count based RNG, used to add tiles



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

; Possible values of GameState
WaitingJoyPress   = 0
Shifting          = 1
AddingRandomTile  = 2
WaitingJoyRelease = 3


CellTableYOffset     = 5  ; How much we +/- to move up/down a line on the table

; Some relative positions on the cell table
; Notice how we go to last data cell: Top-Left + 3 rows down + 3 columns right
; (FYI: add another row down and you'd have the last sentinel)
FirstDataCellOffset = 5
LastDataCellOffset  = FirstDataCellOffset + (CellTableYOffset * 3) + 3
LastCellOffset      = LastDataCellOffset + CellTableYOffset

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

GridColor = $12
TileColor = $EC

TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)
GridSeparatorHeight = 10

GridPF0 = $00            ; Grid sides are always clear, minus last bit
GridPF1 = $01
GridPF2Tile  = %10011001 ; Grid has "holes" for numbers
GridPF2Space = %11111111 ; but is solid between the tiles

JoyP0Up    = %11100000      ; Masks to bit-test SWCHA for joystick movement
JoyP0Down  = %11010000
JoyP0Left  = %10110000
JoyP0Right = %01110000
JoyMaskP0  = %11110000

; Amount to add to move to a direciton in the cell grid, in two's complement
RightShiftVector = $01     ;  1
LeftShiftVector  = $FF     ; -1
DownShiftVector  = $05     ;  5
UpShiftVector    = $FB     ; -5

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
    lda #GridColor
    sta COLUPF

    lda #TileColor      ; Players will be used to draw the tiles (numbers)
    sta COLUP0
    sta COLUP1


InitialValues:
    lda #$F0
    sta HMBL
    lda #$00
    sta REFP0
    sta REFP1

;;;;;;;;;;;;;;;;;;;;;;
;; GRID PREPARATION ;;
;;;;;;;;;;;;;;;;;;;;;;

; Pre-fill the tile bitmap MSBS, so we only have to
; figure out the LSBs for each tile
    lda #>Tiles
    ldx #7
FillMsbLoop:
    sta RowTileBmp1,x
    dex
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

StartFrame:
    lda #%00000010
    sta VSYNC
    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC
    sta WSYNC

VBlank:



    REPEAT 35
        sta WSYNC
    REPEND
    ldx #0         ; scanline counter
    stx VBLANK
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


; Separator scanlines 2 and 3:
; player graphics duplicated and positioned like this: P0 P1 P0 P1

    lda #$02    ; (2)        ; Duplicate the players (with some space between)
    sta NUSIZ0  ; (3)
    sta NUSIZ1  ; (3)

    REPEAT 9    ; (27 = 9x3) ; Position P0 close to the beginning of 1st tile
        bit $00
    REPEND
    sta RESP0   ; (3)

    bit $00     ; (3)        ; and P1 close to the beginning of the second
    sta RESP1   ; (3)
    sta WSYNC

    lda #$F0                 ; Fine-tune player positions to fill the grid
    sta HMP0
    lda #$10
    sta HMP1
    sta WSYNC
    sta HMOVE
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRID ROW PREPARATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Separator scanlines 4-7:
; calculate tile address LSB for the 4 tiles, one per scanline

GridRowPreparation:
    ldy #0             ; (2)   ; Y = column (*2) counter

UpdateTileBitmapAddressLoop:
    ldx CellCursor     ; (3)   ; A = current grid cell value.
    lda CellTable,x    ; (4)

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

MultiplicationDone:
    sta RowTileBmp1,y  ; (5)   ; Store LSB (MSB is fixed)

    iny                ; (2)
    iny                ; (2)
    inc CellCursor     ; (5)
    sta WSYNC
    cpy #8             ; (2)
    bne UpdateTileBitmapAddressLoop ; (2 in branch fail)

; Separator scanline 8:

    REPEAT 18    ; (54 = 18x3) ; Switch playfield (after the beam draws it)
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




Overscan:
    lda #%01000010
    sta VBLANK               ; Disable output

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
    sta WSYNC

;;;;;;;;;;;;;;;;;
;; SHIFT BOARD ;;
;;;;;;;;;;;;;;;;;

    lda GameState
    cmp #Shifting
    bne EndShift               ; Not shifting

; Outer loop will traverse the entire cell map in the *opposite* order of the
; movement, that is, from the beginning for right/up and from end for left/down,
; so they stack and merge as expected. Let's setup these parameters

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
    bne NotEmpty             ; We won't move if the cell is not empty

MoveCurrentToNext:
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
    bne AdvanceToNext;       ; Only merge if value matches

Merge:
    inc CurrentValue         ; Multiply by 2 in log (that is, add 1 to exponent)
    lda #MergedMask
    ora CurrentValue         ; Add the "merged" bit (so it doesn't match others)
    sta CurrentValue
    jmp MoveCurrentToNext    ; Move the multiplied cell to the target position

FinishShift:
    lda #AddingRandomTile    ; Upon finishing the shift, we'll add a new tile
    sta GameState

    ldx FirstDataCellOffset  ; Clear the merged bit (restoring tile values)
ClearMergeBitLoop:
    lda CellTable,x
    and #ClearMergedMask
    sta CellTable,x
    inx
    cpx #LastDataCellOffset+1
    bne ClearMergeBitLoop


EndShift:
    ; FIXME we surely spent more than a scanline, figure out something
    ; (idea: each processed tile in a single scanline; clear loop in
    ;  its own scanline as well)
    sta WSYNC

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
    cmp #Wall3Repl
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
    lda #WaitingJoyRelease
    sta GameState             ; Wait for joystick release before a new shift

EndRandomTile:
    inc RandomNumber         ; Feed the random number generator
    sta WSYNC

    REPEAT 27
        sta WSYNC
    REPEND
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
