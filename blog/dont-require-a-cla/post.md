---
title: Don't Require a CLA
published: 2015-01-16
---

This is an essay I wrote for English class a few months ago. I wanted to improve
it and then post it on here, but that didn't happen and it ended up getting
buried deep in a directory structure. I'm posting it now with some links added,
some errors fixed and some sentences worded differently.

---

It's hard to deny that Open source and [Free software][foss] has gotten big.
Since [Richard Stallman][rms] started the [GNU][] operating system in September
1983 --- an attempt to create an operating system composed of free software that
could be a drop-in replacement for the proprietary Unix --- the community has
evolved greatly. When in 1989 Stallman published the
[GNU General Public License][gpl] that enforces the "free" in "free software",
much of the operating system was already done. Then in 1991 Linus Torvalds
released [Linux][], allowing the GNU operating system to run on its own. These
days this and other open source software is found everywhere, but one issue that
keeps coming up is licensing. Many projects led by companies require
contributors to sign a [Contributor License Agreement][cla]. I think this is a
bad development.

For most open source software there's an unwritten rule that the code you
contribute to the project is released under the same license as the software
already uses. Every contributor maintains his or her copyright, but they release
their code under a license like the GPL, the [MIT license][mit], or similar.
This has certain implications. For one, sometimes the original developer want to
release the software using a different license. This is only possible if all
contributors agree to it. It could also have implications for enforcing the
license, because that can only be done by the copyright holder(s).

The [Free Software Foundation][fsf] and the [Apache Software Foundation][asf],
among others, introduced CLAs to clear up the situation. You assign the
copyright of your changes over to the FSF, or grant a license to the Apache
Foundation. This agreement clears up the legal standing of your contributions,
making it easier for the FSF or Apache Foundation to enforce the license in
court.

The requirement to sign a CLA has advantages, but it also has disadvantages. It
dramatically increases the barrier for contributing to projects that require
them, discouraging people from contributing if they aren't completely committed
to the project. It removes the appeal of quickly submitting a fix for a minor
issue, while fixing small things is often an easy way to get familiar with a
codebase, later making it easier to make bigger contributions.

Aside from making contributions a lot "heavier", some CLAs also have other
implications. Signing your copyright over to the FSF or the ASF isn't a big
issue, because these are entities with the specific goal of facilitating the
development of free software. Most companies, however, have a very different
goal: profit. If you sign your copyright over to a company like Facebook you're
basically providing free work, allowing them to use your code while they have no
obligation to keep their work available to you.

When starting an open source project, consider the consequences of requiring a
CLA. If you're very concerned about the legal status of contributions, consider
handing the reign of the project over to an organization like the Apache
Foundation or the Free Software Foundation instead of requiring contributors to
sign a CLA of your own. There are many people who have signed their CLAs and it
makes your intentions clear. However, even if you're concerned, consider if it's
really that big of a problem. Huge open source projects like Linux and
[FreeBSD][] have been running for years without a CLA. They have been very
successful and their lack of a CLA hasn't been a problem.

[foss]: https://en.wikipedia.org/wiki/Free_software
[rms]: https://en.wikipedia.org/wiki/Richard_Stallman
[GNU]: https://en.wikipedia.org/wiki/GNU
[gpl]: https://en.wikipedia.org/wiki/GNU_General_Public_License
[Linux]: https://en.wikipedia.org/wiki/Linux
[cla]: https://en.wikipedia.org/wiki/Contributor_License_Agreement
[mit]: http://choosealicense.com/licenses/mit/
[fsf]: https://www.fsf.org/
[asf]: https://www.apache.org/
[FreeBSD]: http://www.freebsd.org/
