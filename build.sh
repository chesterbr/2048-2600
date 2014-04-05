#!/bin/bash
#
# This is my (ugly) build script. You'll likely need to adapt it
# (at least set the proper program locations)

# Program locations
RUBY=ruby
DASM=./dasm-mac
STELLA=/Applications/Stella.app/Contents/MacOS/Stella

# Expected ROM size in bytes
ROM_SIZE=2048

rm -f 2048.bin
$RUBY graphics_gen.rb
$DASM 2048.asm -o2048.bin -s2048.sym -f3
if [ -e 2048.bin ] &&  [ `wc -c < 2048.bin` -eq $ROM_SIZE ]
then
  $STELLA 2048.bin
fi
