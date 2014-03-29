#
# 2048-2600 needs its graphics to be upside down and right-to-left
# (ok, I could have used REFPn for the later). This script converts
# them to an .asm source file that can be used on the main program.
#

glyphs = <<END

;;;;;;;;;;;;;;
;; GRAPHICS ;;
;;;;;;;;;;;;;;

; Tiles are 8 x 11 to partially compensate for
; Atari's pixel ratio (approx. 1.66:1)

Tiles:
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX
.   XXXXXXXX

.
.    XXXXXX
.    XXXXXX
.        XX
.    XXXXXX
.    XXXXXX
.    XX
.    XX
.    XXXXXX
.    XXXXXX
.

.
.    XX  XX
.    XX  XX
.    XX  XX
.    XXXXXX
.    XXXXXX
.        XX
.        XX
.        XX
.        XX
.

.
.     XXXX
.    XX  XX
.    XX  XX
.     XXXX
.     XXXX
.    XX  XX
.    XX  XX
.    XX  XX
.     XXXX
.

.
.     X X
.     X X
.     X X
.     X X
.     X XXX
.     X X X
.     X X X
.     X X X
.     X XXX
.

.
.     XX XX
.      X  X
.      X  X
.      X  X
.     XX XX
.      X X
.      X X
.      X X
.     XX XX
.

.
.    X  X X
.    X  X X
.    X  X X
.    X  XXX
.    XXX  X
.    X X  X
.    X X  X
.    X X  X
.    XXX  X
.

.
.
.   X XX XXX
.   X  X X X
.   X  X X X
.   X XX XXX
.   X X  X X
.   X X  X X
.   X XX XXX
.
.

.
.
.   XX XX X
.    X X  X
.    X X  X
.   XX XX XX
.   X   X XX
.   X   X XX
.   XX XX XX
.
.

.
.
.   XX  X XX
.   X   X  X
.   X   X  X
.   XX  X XX
.    X  X X
.    X  X X
.   XX  X XX
.
.

.      X XXX
.      X X X
.      X X X
.      X X X
.      X XXX
.
.    XXX X X
.      X X X
.    XXX XXX
.    X     X
.    XXX   X

.    XXX XXX
.      X X X
.    XXX X X
.    X   X X
.    XXX XXX
.
.    X X XXX
.    X X X X
.    XXX XXX
.      X X X
.      X XXX

.    X X XXX
.    X X X X
.    XXX X X
.      X X X
.      X XXX
.
.    XXX X
.    X X X
.    XXX XXX
.      X X X
.      X XXX

.     XXX X
.     X X X
.     XXX X
.     X X X
.     XXX X
.
.    XXX XXX
.    X X   x
.    XXX XXX
.      X X
.      X XXX

TitleScreenTiles:
.
.     XXXX
.    XX  XX
.    XX  XX
.    XX  XX
.    XX  XX
.    XX  XX
.    XX  XX
.    XX  XX
.     XXXX
.

.
.     XXXX
.    XX  XX
.    XX
.    XXXXX
.    XXXXXX
.    XX  XX
.    XX  XX
.    XX  XX
.     XXXX
.

.
.    XX X
.    X  XXX
.    X  X X
.    XX X X
.
.        X
.        XX
.        X
.        XX
.

.
.     X  XX
.    X X X
.    XX   X
.     XX XX
.
.     X  XX
.    X X X
.    XX  X
.     XX X
.

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

END

def write_asm_file(glyphs)
  File.open('graphics.asm', 'w') do |file|
    glyphs.lines.map(&:rstrip).each do |line|
      if line.start_with? '.'
        glyph(line)
      else
        file.puts normal(line)
      end
    end
  end
end

@glyph = []

def glyph(line)
  @glyph << "    .BYTE %#{(line[4..11] || "     ").ljust(8).tr(' ', '1').tr('^1', '0')}"
end

def normal(line)
  return line if @glyph.count == 0

  @glyph << line
  result = @glyph.reverse.join("\n");
  @glyph = []
  result
end

write_asm_file(glyphs)

