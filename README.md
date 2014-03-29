2048 2600
=========

A port of the [2048][1] game to the [Atari 2600][4].

(because there aren't enough [versions][11] of this game :laughing:)

## Screenshot

![2048 2600](http://chester.me/img/2014/03/2048-2600.png "2048 2600")

## Running

### On the Browser

There are (at least) two options to run it without installing any software:
- [Play it on JSMESS][35] (thanks to [James Friend][34])
- [Play it on javatari][36] (thanks to javatari creator, [Paulo Augusto Peccin][37])

### On an Emulator or a Real Atari

I suggest using [Stella][13], or a real Atari with an [Harmony][14] cart. In either case, just download [2048.rom][2] and run as a normal Atari game.

If you are running on a PAL Atari/TV, put the TV TYPE switch on the B•W position.

## Bulding / Development info

All the juicy details are included on [2048.asm][3] (the main source file), and a bit easier to read [on this blog post][12].

## Author and License

© 2014 by Carlos Duarte do Nascimento (Chester)

Portions contributed and © by their respective authors, see below.

Based on the [original 2048 game][1] by Gabriele Cirulli

This software is released under the [MIT license][9], and pull requests are
welcome. Keep in mind that by submitting a pull request, you are agreeing
to the licensing terms and licensing your contribution under them (without
undermining any of your other rights).

If you have any questions/comments/rants, feel free to contact me on [Twitter][7], on my [blog][8] or simply [write an e-mail][10]!

## Contributors

- [Ben Combee][40] - Fixed the bug that prevented the last tile from being picked;
- [SvOlli (Sven Oliver Moll)][41] - Implemented PAL mode using the TV TYPE switch.

## Special Thanks

- [Lucas][30], [Diogo][31], and [Bani][32], for support and suggestions.
- All the nice people at the [AtariAge forum][33], for making lots of information available.

## Pending
### Known Bugs
- Very infrequently we have an extra scanline. Seems to happen when lots of tiles are shifted.

### Things that would be nice

- Time-based multiplayer like [Emil Stolarsky's][6]
- Turn-based multiplayer (survival-style), one or two players
- An easter egg (after all, Atari 2600 [pioneered][5] the genre)
- Different colors for each tile value (this one is **hard**. I've almost
managed to do it, but I'm missing a few cycles to change the colors quick enough)
- A better font for the scores? Better tiles?

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
[30]: http://github.com/lxfontes
[31]: http://github.com/dterror
[32]: http://github.com/bani
[33]: http://atariage.com/forums/forum/50-atari-2600-programming/
[34]: https://github.com/jsdf
[35]: http://jamesfriend.com.au/2600/2048/
[36]: http://javatari.org/games/2048
[37]: https://github.com/ppeccin
[40]: https://github.com/unwiredben
[41]: http://svolli.org/atari2600/
