
;;;;;;;;;;;;;;
;; GRAPHICS ;;
;;;;;;;;;;;;;;

; Tiles are 8 x 11 to partially compensate for
; Atari's pixel ratio (approx. 1.66:1)

Tiles:

    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000
    .BYTE %00000000

    .BYTE %11111111
    .BYTE %10000001
    .BYTE %10000001
    .BYTE %10011111
    .BYTE %10011111
    .BYTE %10000001
    .BYTE %10000001
    .BYTE %11111001
    .BYTE %10000001
    .BYTE %10000001
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11111001
    .BYTE %11111001
    .BYTE %11111001
    .BYTE %11111001
    .BYTE %10000001
    .BYTE %10000001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11000011
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %11000011
    .BYTE %11000011
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %11000011
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11010001
    .BYTE %11010101
    .BYTE %11010101
    .BYTE %11010101
    .BYTE %11010001
    .BYTE %11010111
    .BYTE %11010111
    .BYTE %11010111
    .BYTE %11010111
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11001001
    .BYTE %11101011
    .BYTE %11101011
    .BYTE %11101011
    .BYTE %11001001
    .BYTE %11101101
    .BYTE %11101101
    .BYTE %11101101
    .BYTE %11001001
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %10001101
    .BYTE %10101101
    .BYTE %10101101
    .BYTE %10101101
    .BYTE %10001101
    .BYTE %10110001
    .BYTE %10110101
    .BYTE %10110101
    .BYTE %10110101
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11111111
    .BYTE %01001000
    .BYTE %01011010
    .BYTE %01011010
    .BYTE %01001000
    .BYTE %01101010
    .BYTE %01101010
    .BYTE %01001000
    .BYTE %11111111
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11111111
    .BYTE %00100100
    .BYTE %01110100
    .BYTE %01110100
    .BYTE %00100100
    .BYTE %10101101
    .BYTE %10101101
    .BYTE %00100101
    .BYTE %11111111
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11111111
    .BYTE %00110100
    .BYTE %10110101
    .BYTE %10110101
    .BYTE %00110100
    .BYTE %01110110
    .BYTE %01110110
    .BYTE %00110100
    .BYTE %11111111
    .BYTE %11111111

    .BYTE %10001110
    .BYTE %10111110
    .BYTE %10001000
    .BYTE %11101010
    .BYTE %10001010
    .BYTE %11111111
    .BYTE %11101000
    .BYTE %11101010
    .BYTE %11101010
    .BYTE %11101010
    .BYTE %11101000

    .BYTE %11101000
    .BYTE %11101010
    .BYTE %10001000
    .BYTE %10101010
    .BYTE %10101000
    .BYTE %11111111
    .BYTE %10001000
    .BYTE %10111010
    .BYTE %10001010
    .BYTE %11101010
    .BYTE %10001000

    .BYTE %11101000
    .BYTE %11101010
    .BYTE %10001000
    .BYTE %10101011
    .BYTE %10001011
    .BYTE %11111111
    .BYTE %11101000
    .BYTE %11101010
    .BYTE %10001010
    .BYTE %10101010
    .BYTE %10101000

    .BYTE %11101000
    .BYTE %11101011
    .BYTE %10001000
    .BYTE %10101110
    .BYTE %10001000
    .BYTE %11111111
    .BYTE %11000101
    .BYTE %11010101
    .BYTE %11000101
    .BYTE %11010101
    .BYTE %11000101
TitleScreenTiles:

    .BYTE %11111111
    .BYTE %11000011
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %11000011
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11000011
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10011001
    .BYTE %10000001
    .BYTE %10000011
    .BYTE %10011111
    .BYTE %10011001
    .BYTE %11000011
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11111001
    .BYTE %11111011
    .BYTE %11111001
    .BYTE %11111011
    .BYTE %11111111
    .BYTE %10010101
    .BYTE %10110101
    .BYTE %10110001
    .BYTE %10010111
    .BYTE %11111111

    .BYTE %11111111
    .BYTE %11001011
    .BYTE %10011011
    .BYTE %10101011
    .BYTE %11011001
    .BYTE %11111111
    .BYTE %11001001
    .BYTE %10011101
    .BYTE %10101011
    .BYTE %11011001
    .BYTE %11111111
Digits
zero
  .byte $7E ; |.XXXXXX.|
  .byte $72 ; |.XXX..X.|
  .byte $72 ; |.XXX..X.|
  .byte $72 ; |.XXX..X.|
  .byte $7E ; |.XXXXXX.|
one
  .byte $1C ; |...XXX..|
  .byte $1C ; |...XXX..|
  .byte $1C ; |...XXX..|
  .byte $1C ; |...XXX..|
  .byte $1C ; |...XXX..|
two
  .byte $7E ; |.XXXXXX.|
  .byte $40 ; |.X......|
  .byte $7E ; |.XXXXXX.|
  .byte $0E ; |....XXX.|
  .byte $7E ; |.XXXXXX.|
three
  .byte $7E ; |.XXXXXX.|
  .byte $4E ; |.X..XXX.|
  .byte $1C ; |...XXX..|
  .byte $4E ; |.X..XXX.|
  .byte $7E ; |.XXXXXX.|
four
  .byte $1C ; |...XXX..|
  .byte $1C ; |...XXX..|
  .byte $7E ; |.XXXXXX.|
  .byte $5C ; |.X.XXX..|
  .byte $7C ; |.XXXXX..|
five
  .byte $7E ; |.XXXXXX.|
  .byte $0E ; |....XXX.|
  .byte $7E ; |.XXXXXX.|
  .byte $40 ; |.X......|
  .byte $7E ; |.XXXXXX.|
six
  .byte $7E ; |.XXXXXX.|
  .byte $4E ; |.X..XXX.|
  .byte $7E ; |.XXXXXX.|
  .byte $40 ; |.X......|
  .byte $7E ; |.XXXXXX.|
seven
  .byte $0E ; |....XXX.|
  .byte $0E ; |....XXX.|
  .byte $0E ; |....XXX.|
  .byte $4E ; |.X..XXX.|
  .byte $7E ; |.XXXXXX.|
eight
  .byte $7E ; |.XXXXXX.|
  .byte $4E ; |.X..XXX.|
  .byte $7E ; |.XXXXXX.|
  .byte $72 ; |.XXX..X.|
  .byte $7E ; |.XXXXXX.|
nine
  .byte $7E ; |.XXXXXX.|
  .byte $02 ; |......X.|
  .byte $7E ; |.XXXXXX.|
  .byte $72 ; |.XXX..X.|
  .byte $7E ; |.XXXXXX.|

