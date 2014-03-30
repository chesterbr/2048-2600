#!/usr/bin/env ruby
#
# graphics_gen.rb
#
# Builds the graphics.asm file from the template below.
#
# Graphic glyphs are built with "X" and spaces, and surrounded on
# the template with either "!" (X = bit "0") or "|" (X = bit "1"),
# so we can easily draw tiles, score digits, etc.
#
# In either case, glyphs are written upside-down, so we can use
# (cheaper) decreasing counter loops
#

@glyphs = <<END

;;;;;;;;;;;;;;
;; GRAPHICS ;;
;;;;;;;;;;;;;;

; Tiles are 8 x 11 to partially compensate for
; Atari's pixel ratio (approx. 1.66:1)

Tiles:
|        |
|        |
|        |
|        |
|        |
|        |
|        |
|        |
|        |
|        |
|        |

|        |
| XXXXXX |
| XXXXXX |
|     XX |
| XXXXXX |
| XXXXXX |
| XX     |
| XX     |
| XXXXXX |
| XXXXXX |
|        |

|        |
| XX  XX |
| XX  XX |
| XX  XX |
| XXXXXX |
| XXXXXX |
|     XX |
|     XX |
|     XX |
|     XX |
|        |

|        |
|  XXXX  |
| XX  XX |
| XX  XX |
|  XXXX  |
|  XXXX  |
| XX  XX |
| XX  XX |
| XX  XX |
|  XXXX  |
|        |

|        |
|  X X   |
|  X X   |
|  X X   |
|  X X   |
|  X XXX |
|  X X X |
|  X X X |
|  X X X |
|  X XXX |
|        |

|        |
|  XX XX |
|   X  X |
|   X  X |
|   X  X |
|  XX XX |
|   X X  |
|   X X  |
|   X X  |
|  XX XX |
|        |

|        |
| X  X X |
| X  X X |
| X  X X |
| X  XXX |
| XXX  X |
| X X  X |
| X X  X |
| X X  X |
| XXX  X |
|        |

|      X |
|      X |
|      X |
|      X |
|      X |
|        |
|XXX XXX |
|  X X X |
|XXX XXX |
|X   X X |
|XXX XXX |

|    XXX |
|      X |
|    XXX |
|    X   |
|    XXX |
|        |
|XXX X   |
|X   X   |
|XXX XXX |
|  X X X |
|XXX XXX |

|    XXX |
|    X   |
|    XXX |
|      X |
|    XXX |
|        |
|  X XXX |
|  X   X |
|  X XXX |
|  X X   |
|  X XXX |


|  X XXX |
|  X X X |
|  X X X |
|  X X X |
|  X XXX |
|        |
|XXX X X |
|  X X X |
|XXX XXX |
|X     X |
|XXX   X |

|XXX XXX |
|  X X X |
|XXX X X |
|X   X X |
|XXX XXX |
|        |
|X X XXX |
|X X X X |
|XXX XXX |
|  X X X |
|  X XXX |

|X X XXX |
|X X X X |
|XXX X X |
|  X X X |
|  X XXX |
|        |
|XXX X   |
|X X X   |
|XXX XXX |
|  X X X |
|  X XXX |

| XXX X  |
| X X X  |
| XXX X  |
| X X X  |
| XXX X  |
|        |
|XXX XXX |
|X X   x |
|XXX XXX |
|  X X   |
|  X XXX |

TitleScreenTiles:
|        |
|  XXXX  |
| XX  XX |
| XX  XX |
| XX  XX |
| XX  XX |
| XX  XX |
| XX  XX |
| XX  XX |
|  XXXX  |
|        |

|        |
|  XXXX  |
| XX  XX |
| XX     |
| XXXXX  |
| XXXXXX |
| XX  XX |
| XX  XX |
| XX  XX |
|  XXXX  |
|        |

|        |
| XX X   |
| X  XXX |
| X  X X |
| XX X X |
|        |
|     X  |
|     XX |
|     X  |
|     XX |
|        |

|        |
|  X  XX |
| X X X  |
| XX   X |
|  XX XX |
|        |
|  X  XX |
| X X X  |
| XX  X  |
|  XX X  |
|        |

Digits:
! XXXXXX !
! XX  XX !
! XX  XX !
! XX  XX !
! XXXXXX !

!   XX   !
! XXXX   !
!   XX   !
!   XX   !
! XXXXXX !

! XXXXXX !
!     XX !
! XXXXXX !
! XX     !
! XXXXXX !

! XXXXXX !
!     XX !
!   XXXX !
!     XX !
! XXXXXX !

! XX  XX !
! XX  XX !
! XXXXXX !
!     XX !
!     XX !

! XXXXXX !
! XX     !
! XXXXXX !
!     XX !
! XXXXXX !

! XXXXXX !
! XX     !
! XXXXXX !
! XX  XX !
! XXXXXX !

! XXXXXX !
!     XX !
!     XX !
!     XX !
!     XX !

! XXXXXX !
! XX  XX !
! XXXXXX !
! XX  XX !
! XXXXXX !

! XXXXXX !
! XX  XX !
! XXXXXX !
!     XX !
! XXXXXX !

END

@glyph = []         # Digit or tile being reversed
@graphics_asm = ""  # Final file contents

def parse_glyphs
  @glyphs.lines.map(&:rstrip).each do |line|
    if line.start_with? '|'
      tile(line)
    elsif line.start_with? '!'
      digit(line)
    else
      normal(line)
    end
  end
end

def tile(line)
  @glyph << "    .BYTE %#{line[1..8].tr(' ', '1').tr('^1', '0')}\n"
end

def digit(line)
  @glyph << "    .BYTE %#{line[1..8].tr(' ', '0').tr('^0', '1')}\n"
end

def normal(line)
  if @glyph.count > 0
    @graphics_asm << @glyph.push(line).reverse.join
    @glyph = []
  end

  @graphics_asm << line << "\n"
end

parse_glyphs
File.write 'graphics.asm', @graphics_asm
