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

Initialize:             ; Cleanup from macro.h (by Andrew Davie/DASM)
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

InitialValues:
    lda #$AB
    sta COLUP0
    sta COLUP1
    lda #$F0
    sta HMBL
    lda #$FF
    sta COLUPF
    lda #$30
    sta CTRLPF
    lda #$02
    sta NUSIZ0
    sta NUSIZ1
    lda #$00
    sta REFP0
    sta REFP1

StartFrame:
    lda #%00000010
    sta VSYNC
    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

VBlank:
    sta WSYNC
    REPEAT 18
        nop
    REPEND
    sta RESP0
    nop
    sta RESP1

    sta WSYNC      ; First line positioned the ball

    lda #$F0
    sta HMP1
    sta HMOVE

    REPEAT 35
        sta WSYNC
    REPEND
    ldx #0         ; scanline counter
    stx VBLANK
    ldy #$00

Scanline:
    sta WSYNC
    ;sta HMOVE
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    lda t2048,y
    sta GRP1
    lda t1024,y
    sta GRP0

    lda t512,y
    sta GRP1
    lda t256,y
    sta GRP0
    iny

    ; lda TileBitmaps,y
    ; sta GRP1
    ; dey
    ; lda TileBitmaps,y
    ; sta GRP0
    ; iny
    ; lda TileBitmaps,y
    ; sta GRP0
    ; iny



    inx
    cpx #90
    bne Scanline

Overscan:
    lda #%01000010
    sta VBLANK      ;
    REPEAT 30
        sta WSYNC
    REPEND
    jmp StartFrame

TileBitmaps:
t2048:
    .BYTE %00000000
    .BYTE %11101110
    .BYTE %00101010
    .BYTE %11101010
    .BYTE %10001010
    .BYTE %11101110
    .BYTE %00000000
    .BYTE %10101110 ; 2048
    .BYTE %10101010
    .BYTE %11101110
    .BYTE %00101010
    .BYTE %00101110
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000

t1024:
    .BYTE %00000000
    .BYTE %01001110
    .BYTE %01001010
    .BYTE %01001010
    .BYTE %01001010
    .BYTE %01001110
    .BYTE %00000000
    .BYTE %11101010 ; 1024
    .BYTE %00101010
    .BYTE %11101110
    .BYTE %10000010
    .BYTE %11100010
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000

t512:
    .BYTE %00000000
    .BYTE %00111000
    .BYTE %00100000
    .BYTE %00111000
    .BYTE %00001000
    .BYTE %00111000
    .BYTE %00000000
    .BYTE %01001110
    .BYTE %01000010
    .BYTE %01001110
    .BYTE %01001000
    .BYTE %01001110
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000

t256:
    .BYTE %00000000
    .BYTE %00111000
    .BYTE %00001000
    .BYTE %00111000
    .BYTE %00100000
    .BYTE %00111000
    .BYTE %00000000
    .BYTE %11101110
    .BYTE %10001000
    .BYTE %11101110
    .BYTE %00101010
    .BYTE %11101110
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000


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

