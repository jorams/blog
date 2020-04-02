---
title: Pong over the Network
published: 2014-12-23
---

On the lowest floor of building T of [Windesheim][] there are two large screens
(with touch support) with built-in computers. Our current project is to build an
application (in [C#][]) for on such a screen. Part of the assignment was to come
up with ideas for what to put on there. Our ideas basically went in two
directions.

1. A game, possibly split over both of the screens.
2. An application that presents information in an interesting way.

The product owner decided we'd implement the second idea, mostly because it's a
lot more manageable. Personally, however, I'm more interested in building a game
split over two computers, connected over the network. A simple game to play
around with that is [Pong][original-pong], so I decided to implement it.

The result of that is [here][pong]. It's overly simple, doesn't keep any sort of
score and somehow started running at nearly double speed every once in a while
on a friend's computer, but it mostly works. It's remarkable how much fun it can
be to play.

Originally I also wanted to use this as an excuse to learn a little modern
OpenGL, but I ended up going with a bunch of calls to `gl:vertex`: the old,
deprecated way. Modern OpenGL is a lot more complicated, so it will have to wait
until another day, when I actually find a need for it.

[pong]: https://github.com/jorams/pong
[Windesheim]: http://www.windesheim.nl/
[C#]: https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29
[original-pong]: https://en.wikipedia.org/wiki/Pong
