;
; 2048.asm
;
; Add some explanation
;
;   dasm 2048.asm -2048.bin -f3
;

    PROCESSOR 6502
    INCLUDE "vcs.h"

    ORG $F000

;;;;;;;;;
;; RAM ;;
;;;;;;;;;

RowTileBmp1 = $80            ; Each of these points to the address of the
RowTileBmp2 = $82            ; bitmap that will be drawn on the current/next
RowTileBmp3 = $84            ; row of the grid, and must be updated before
RowTileBmp4 = $86            ; the row is drawn

;;;;;;;;;;;;;;;
;; CONSTANTS ;;
;;;;;;;;;;;;;;;

GridColor = $12
TileColor = $EC

TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)

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

;;;;;;;;;;;;;;;;;;;
;; GENERAL SETUP ;;
;;;;;;;;;;;;;;;;;;;

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

PosGrid SET RowTileBmp1
TileAddr SET Tiles + (11*11)
    REPEAT 5
    lda #<TileAddr
    sta PosGrid
PosGrid SET PosGrid + 1
    lda #>TileAddr
    sta PosGrid
PosGrid SET PosGrid + 1
TileAddr SET TileAddr + TileHeight
    REPEND



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

;;;;;;;;;;;;;;;;;;;;;;;;
;; GRID TOP SEPARATOR ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Top separator scanline 1:
; configure grid playfield
    lda #GridPF0
    sta PF0
    lda #GridPF1
    sta PF1
    lda #GridPF2Space
    sta PF2
    sta WSYNC

; Top separator scanlines 2 and 3:
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

; Top separator scanline 4 (last one)
    ldy #TileHeight-1          ; Initialize tile scanline counter
                               ; (goes downwards and is zero-based)

    REPEAT 20    ; (60 = 20x3) ; Ensure we are past the grid horizontal area
        bit $00
    REPEND

    lda #GridPF2Tile           ; Change to the "tile" playfield
    sta PF2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRID TILE ROW (AND BOTTOM SEPARATOR) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TileScanline:
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

    lda (RowTileBmp3),y
    sta GRP0
    lda (RowTileBmp4),y
    sta GRP1
    dey
    bpl TileScanline;

    ; FIXME
    lda #0
    sta GRP0
    sta GRP1
    lda #GridPF2Space
    sta PF2


    REPEAT 120
        sta WSYNC
    REPEND



Overscan:
    lda #%01000010
    sta VBLANK      ;
    REPEAT 30
        sta WSYNC
    REPEND
    jmp StartFrame

    INCLUDE "graphics.asm"

    ; Temp, just to clean
    REPEAT 50
        .BYTE %00000000
    REPEND

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

