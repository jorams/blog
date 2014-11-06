---
layout: post
title:  The Joy of Text
date:   2014-11-06
---

HTTP/2 is a [binary protocol][http2-binary], whereas HTTP versions before that
are plain text protocols. The change to a binary protocol was made to make
parsing more efficient. [Binary protocols][] are designed solely for computers
to read and write, while [plain text protocols][] are readable and writable by
humans. The switch of HTTP to a binary protocol makes sense, because the speed
of websites is incredibly important when billions of them are loaded every day.

Binary protocols are also boring. If you've never used [telnet(1)][] or
[netcat(1)][] to connect to an [IRC][] server, you should give it a try. It's
easy:

```bash
$ nc irc.server.name 6667

# Tell the server who you are
nick test132
user test132 0 * :Test132
#< A whole bunch of information, the MOTD, etc.

# Join a channel
join #test
#< topic, users

# Every once in a while, you receive a PING
#< PING :server.host.name
pong

# Chat in a channel:
privmsg #test This is a test

# Leave a channel:
part #test

# When you're done, quit
quit
```

Using an IRC client is usually easier, but it's not required. Even on a computer
without an IRC client you can use IRC. It also makes the protocol easier to work
with; want to implement IRC in your program but having some trouble? Just
connect manually and experiment!

Besides human-readable protocols, plain text over [sockets][] offers more.
[MUDs][] are text-based games you can play by similarly connecting to a server
over the network using telnet. [Many][lost-souls] of those [games][medievia]
have been [online][threshold] for many [years][alteraeon] (these are just four
examples).

When you connect to a MUD, you connect to a vast world you can explore filled
with adventures and people to talk to. What can be put into a MUD is really only
limited to what can be described in text. You're going to be reading a lot, so
it's not for everyone, but the shear amount of detail will blow you away. (At
least it did to me.) Thousands of "rooms" with detailed descriptions, hundreds
of different items, enemies, landscapes. All you need to fill hours of your time
is your imagination and a small program invented in the 60's.

If you want more than your little terminal can offer you, there are MUD
[clients][] that can offer you more. Most of them support some sort of scripting
to automate certain tasks for you, some of them support automatic mapping. (Some
MUDs, like Medievia, also offer integrated maps).

It may not always be the most efficient way to build a protocol, so it may not
always be the best choice, but there's also value in simplicity. Plain text is
flexible and allows for easy experimentation, so if you're building something it
might just be a good choice.

```
   `.rL              `             Alter Aeon            '               oo,
  /mdo-/-+          `/                                   \'           `.+/yms`
 .mMNy+/::+`        `s.                                  +\         `.+:/smMN+
 :NMh.  `-/o:        /s                                  s:       `+o/"` `+NMh
 -NMs      `:`       `s+    `.                    .'    /+        :-      -mMy
  hMm-                 +s.  'o                    o`   +/                 oNN:
  -dMh-                 ``   s.                  .s   ''                `/mNo
   .yNm+.       .:+s+.       ++.                .++       :oo:.       `-sNN+
    '+dNmyo+++ooyhyo-                                     `/yhyo+///+ohmNh-
      `\ymNNNmNNmds:`             `.         .'            .+hdmmmmNNNms:`
         ':oyhdmds/`               +:       :+              -odNNmdyo:`
              ````        \"'+.    ``       ''    .+'"/      ``````
                           ",0_\                 /_0,"
```

(Alter Aeon's connection banner, `telnet alteraeon.com 3000`.)


[http2-binary]: https://http2.github.io/faq/#why-is-http2-binary
[Binary protocols]: https://en.wikipedia.org/wiki/Binary_protocol
[plain text protocols]: https://en.wikipedia.org/wiki/Text-based_protocol
[IRC]: https://en.wikipedia.org/wiki/Internet_Relay_Chat
[netcat(1)]: http://man.cx/netcat
[telnet(1)]: http://man.cx/telnet
[SMTP]: https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
[HTTP]: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol
[base64]: https://en.wikipedia.org/wiki/Base64
[sockets]: https://en.wikipedia.org/wiki/Network_socket
[MUDs]: https://en.wikipedia.org/wiki/MUD
[ANSI colors]: https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
[lost-souls]: http://lostsouls.org/
[medievia]: http://www.medievia.com/
[threshold]: http://www.thresholdrpg.com/
[alteraeon]: http://www.alteraeon.com/
[clients]: http://www.mudconnect.com/resources/Mud_Resources:Mud_Clients.html
