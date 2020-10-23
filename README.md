2048 2600
=========

A port of the [2048][1] game to the [Atari 2600][4].

(because there aren't enough [versions][11] of this game :laughing:)

## Cartridge Art (click to zoom) and Screenshot
[![Art by Jenn Harrison](https://raw.githubusercontent.com/chesterbr/2048-2600/main/cart-small.jpg)](https://raw.githubusercontent.com/chesterbr/2048-2600/main/cart-main.jpg) &nbsp; &nbsp; ![(simulated - 236-0873 was Bozo's phone number in Brazil)](http://chester.me/img/2014/03/2048-2600.png?refresh=3 "(simulated - 236-0873 was Bozo's phone number in Brazil)")

## Instructions

### Game Modes

There are two modes: one-player and two-player. You can use GAME SELECT to switch between them (the selected game will auto-start).

On the title screen, the left joystick button will start a single-player game, and the right joystick button will start a two-player game. When a game is over, the buttons restart the game appropriately.

### One-Player Game

This mode follows the original 2048 as closely as possible.

Move the joystick to shift the tiles into a direction. Tiles with the same value will join, and you'll earn the joined tile's value in points (shown above the board).

You win by forming the 2048 tile. The game continues until there is no movement possible, and the current high score will be displayed below the board.

The high score will be erased if you turn the console/emulator off (it's an Atari game, after all) or if you start a two-player game.

### Two-Player Game

Both players use same board in turns. The current player is the one with the bright score, and keeps playing (and scoring) as long as they keep merging. If a shift happens and no tile is merged, it's the other player's turn.

The game ends when there is no possible movement, and the player with the highest score wins. Either fire button restarts the two-player game.

### PAL

The game supports PAL Atari systems - just change the TV TYPE switch to the B/W position (the COLOR position is NTSC).

## Running

### On the Browser

[Click here to play](http://javatari.org/?ROM=https://github.com/chesterbr/2048-2600/raw/master/2048.bin) on your browser, thanks to [Paulo Augusto Peccin][37]'s [Javatari.js](https://github.com/ppeccin/javatari.js).

### On an Emulator or a Real Atari

I suggest using [Stella][13], or a real Atari with an [Harmony][14] cart. In either case, just download [2048.rom][2] and run as a normal Atari game.

If you are running on a PAL Atari/TV, put the TV TYPE switch on the B•W position.

## Bulding / Development info

All the juicy details are included on [2048.asm][3] (the main source file), and a bit easier to read [on this blog post][12].

## Author and License

© 2014 by Carlos Duarte do Nascimento (Chester)

Portions contributed and © by their respective authors, see below.

Cartridge Art © 2014 by Jenn Harrison

Based on the [original 2048 game][1] by Gabriele Cirulli, which is based on [1024 by Veewo Studio][15] and conceptually similar to [Threes by Asher Vollmer][16].

This software is released under the [MIT license][9], and pull requests are
welcome. Keep in mind that by submitting a pull request, you are agreeing
to the licensing terms and licensing your contribution under them (without
undermining any of your other rights).

If you have any questions/comments/rants, feel free to contact me on [Twitter][7], on my [blog][8], or simply [write an e-mail][10]!

## Contributors

- [Ben Combee][40] - Fixed the bug that prevented the last tile from being picked;
- [SvOlli (Sven Oliver Moll)][41] - Implemented PAL mode using the TV TYPE switch.
- [Jenn Harrison][42] - Cartridge Art

## Special Thanks

- [Lucas][30], [Diogo][31], and [Bani][32], for support and suggestions.
- All the nice people at the [AtariAge forum][33], for making lots of information available.

[1]: https://github.com/gabrielecirulli/2048
[2]: https://github.com/chesterbr/2048-2600/blob/master/2048.bin?raw=true
[3]: https://github.com/chesterbr/2048-2600/blob/master/2048.asm
[4]: http://atariage.com/2600/history.html
[5]: https://www.youtube.com/watch?v=Pw02kibMs3E
[6]: http://emils.github.io/2048-multiplayer/
[7]: http://twitter.com/chesterbr
[8]: http://chester.me
[9]: https://github.com/gabrielecirulli/2048/blob/master/LICENSE.txt
[10]: mailto:cd@pobox.com?subject=2048+2600
[11]: http://phenomist.wordpress.com/2048-variants/
[12]: http://chester.me/archives/2014/03/2048-2600-the-2048-game-for-the-Atari-2600/
[13]: http://stella.sourceforge.net/
[14]: http://harmony.atariage.com/Site/Harmony.html
[15]: https://itunes.apple.com/us/app/1024!/id823499224
[16]: http://asherv.com/threes/
[30]: http://github.com/lxfontes
[31]: http://github.com/dterror
[32]: http://github.com/bani
[33]: http://atariage.com/forums/forum/50-atari-2600-programming/
[34]: https://github.com/jsdf
[35]: http://jamesfriend.com.au/2600/2048/
[36]: http://javatari.org/games/2048
[37]: https://github.com/ppeccin
[38]: http://javatari.org/
[40]: https://github.com/unwiredben
[41]: http://svolli.org/atari2600/
[42]: https://twitter.com/jennofour
