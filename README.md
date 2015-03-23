# 99 Haskell ([99haskell.org][99haskell])

![preview][preview]

## What

Solve live Haskell coding problems based on [H-99][h99] in the browser to
strengthen your understanding of the language.

## How

99 Haskell loads users' code in GHCi in their own Docker containers and calls
the functions with set arguments. It then parses GHCi's output to see if the
returned value matches the expected value.

## Development

99 Haskell requires Docker and uses the [haskell:7.8 Docker image][image].

Compile [Script.hs][script]* into *Script.js* with [Fay][fay] (`fay
Script.hs`).  Then `cabal run` will build and run the 99 Haskell executable on
port 3000.

\* *Probably the worst written part of the program. JS in Haskell is weird.*

Creating/modifying problems is easy, simply edit [Problems.hs][problems]. Note
you will have to restart 99 Haskell for problem changes to take effect
(`runhaskell Main.hs` makes this process quick). 99 Haskell uses H-99's
problems, hence the name. Wording can (and often should) be changed for
clarity, but the expected input/output needs to remain the same. 99 Haskell
automatically links every problem to its corresponding [H-99 solution][h99 s].
Some solution pages are quite bad, so editing the solution pages on haskell.org
is greatly appreciated.

Some very basic security measures have been put in place, but things are far
from ideal. 99 Haskell doesn't host any sensative content, but any time spent
making it harder for a kiddie to [tear down the site][xkcd] is huge.

99 Haskell was made as a "learn by doing" exersise for myself to learn the
Haskell language. Any contributions for the sake of cleaning up bad code is
welcome, and would benefit me and possibly others who may look through the
source.

## Production

In production you should remove `middleware logStdoutDev` from the bottom of
[Main.hs][main].

[99haskell]: http://www.99haskell.org
[preview]: preview.gif
[h99]: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
[image]: https://registry.hub.docker.com/_/haskell/
[script]: Script.hs
[problems]: Problems.hs
[fay]: https://github.com/faylang/fay/wiki
[h99 s]: https://wiki.haskell.org/99_questions/Solutions
[xkcd]: https://xkcd.com/932/
[main]: Main.hs
