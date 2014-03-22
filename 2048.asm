;
; 2048.asm
;
; Add some explanation
;
;   dasm 2048.asm -2048.bin -f3
;

; Cell Tables
; -----------
;
; We store each player's grid in a "cell table", which can contain one
; of these values:
;
;   0  = empty cell
;   1  = "1" tile
;   2  = "2" tile
;   3  = "4" tile
;   4  = "8" tile
;   ...
;   (k = "2^(k-1)" tile)
;   11 = "2048" tile
;   12 = "4096" tile
;   13 = "8192" tile
;   255 (0xFF) = "sentinel" tile (see below)
;
; In theory, we'd use 16 positions in memory for a 4x4 tile grid. Moving
; left/right would mean adding/subtracting one position and moving up/down
; would be done by adding/subtracting 4 positions.
;
; However, we'd need to do complicated boundaries checking, so we'll surround
; the grid with "sentinel" tiles That would theoretically need 20 extra tiles,
; as depicted here (S = sentinel, . = number tile or empty cell):
;
;   SSSSSS
;   S....S
;   S....S
;   S....S
;   S....S
;   SSSSSS
;
; But we can save some space by removing th left-side sentinels, since the
; memory position before those will be a sentinel anyway (the previous line's
; right-side sentinel).
;
; We can also cut the first and last sentinel (no movement can reach those),
; ending with this layout (s = previous sentinel in memory will be used):
;
;    SSSSS
;   s    S
;   s    S
;   s    S
;   s    S
;   sSSSS
;
; We'd add/subtract 5 to move up/down a line, and also would add 5 to get
; from the cell map start to the first effective cell.

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


;;;;;;;;;;;;;;;
;; CONSTANTS ;;
;;;;;;;;;;;;;;;

CellEmpty    = 0         ; Special cell values (see header)
Cell2048     = 11
CellSentinel = 255

CellGridYOffset     = 5  ; How much we +/- to move up/down a line on the grid

; Some relative positions on the grid
; bottom-right = Top-Left + 3 rows down + 3 columns right
; add another row down and you have the last sentinel
FirstDataCellOffset = 5
LastDataCellOffset  = FirstDataCellOffset + (CellGridYOffset * 3) + 3
LastCellOffset      = LastDataCellOffset + CellGridYOffset

GridColor = $12
TileColor = $EC

TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)
GridSeparatorHeight = 10

GridPF0 = $00            ; Grid sides are always clear, minus last bit
GridPF1 = $01
GridPF2Tile  = %10011001 ; Grid has "holes" for numbers
GridPF2Space = %11111111 ; but is solid between the tiles

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
    sta VBLANK      ;
    REPEAT 30
        sta WSYNC
    REPEND
    jmp StartFrame

    ORG $FFFA

    .WORD Initialize
    .WORD Initialize
    .WORD Initialize

    END

;
; Copyright 2011-2013 Carlos Duarte do Nascimento (Chester). All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are
; permitted provided that the following conditions are met:
;
;    1. Redistributions of source code must retain the above copyright notice, this list of
;       conditions and the following disclaimer.
;
;    2. Redistributions in binary form must reproduce the above copyright notice, this list
;       of conditions and the following disclaimer in the documentation and/or other materials
;       provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY CHESTER ''AS IS'' AND ANY EXPRESS OR IMPLIED
; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES;  LOSS OF USE, DATA, OR PROFITS;  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
; The views and conclusions contained in the software and documentation are those of the
; authors and should not be interpreted as representing official policies, either expressed
; or implied, of Chester.
;

