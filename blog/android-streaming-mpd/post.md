---
title: Streaming music on Android using MPD
published: 2020-05-18
---

Most people I know have switched to Spotify for their music needs. I have not.
I love having a music library that contains exactly what I want it to contain,
and that does not change unless I change it. For a long time I had one problem:
I did not have access to my music collection on mobile. Instead I used the
[Bandcamp][bandcamp] app, [NewPipe][newpipe], or listened to podcasts instead.
This was doable, since I buy most of my music on Bandcamp, but it was certainly
less than ideal.

[bandcamp]: https://bandcamp.com/
[newpipe]: https://newpipe.schabi.org/

On desktop I use [MPD][mpd]. I love it, but because it colocates the music
library and the player it didn't work for my mobile needs. I can't keep a copy
of all my music on my phone. MPD can [output to a stream over HTTP][mpd-http],
but there is a significant delay between controls and actual changes in the
playback.

[mpd-http]: https://www.musicpd.org/doc/html/plugins.html#httpd

For a few years I stopped trying, but earlier this year I did some digging and
managed to get things working very nicely. Music is streamed straight from my
desktop and files are transcoded to reduce bandwidth usage. I'll describe my
setup.

[mpd]: https://www.musicpd.org/

## MPD on Android

The first step to working with MPD on Android is to install a client. I've
tried a few over the years, and ended up using MPDroid most of the time.
([F-Droid][mpdroid-fdroid], [Play Store][mpdroid-play-store]), It works fairly
well, but I had some random issues every time I used it (not very often). This
time I found [MAFA][mafa], and it is much better. It's not free, but it has
almost every feature I want and is stable, polished, and user-friendly.

[mpdroid-fdroid]: https://f-droid.org/en/packages/com.namelessdev.mpdroid/
[mpdroid-play-store]: https://play.google.com/store/apps/details?id=com.namelessdev.mpdroid
[mafa]: https://play.google.com/store/apps/details?id=software.indi.android.mpd

For the actual playback I needed MPD itself as well, and it turns out MPD is
available for Android. ([F-Droid][mpd-fdroid], [Play Store][mpd-play-store].)
It's experimental and very bare-bones, but it works. The app really just runs
the daemon, and configuration is done by creating an `mpd.conf` in the external
storage directory.

[mpd-fdroid]: https://f-droid.org/en/packages/org.musicpd/
[mpd-play-store]: https://play.google.com/store/apps/details?id=org.musicpd

## Satellite setup

To be able to play my entire music collection on my phone, MPD needs to be able
to access the music files. Files are read from the [music
directory][mpd-music-directory]. Most often that is a local directory, like
`~/music`, but [it does not have to be][mpd-storage-plugins]. One of the other
options is to use an http(s) URL.

An easy way to set this up is to run a web server in the music directory:

```
python -m http.server --bind --directory ~/music 8000
```

This web server will serve all files from the music directory to the network on
port 8000. Now MPD can read from there:

```conf
# /storage/emulated/0/mpd.conf
music_directory "http://192.168.1.100:8000/"
```

[mpd-music-directory]: https://www.musicpd.org/doc/html/user.html#configuring-the-music-directory
[mpd-storage-plugins]: https://www.musicpd.org/doc/html/plugins.html#storage-plugins

An important caveat is that MPD can not scan for music files in a directory
served over HTTP. Instead, we can configure the [proxy database
plugin][mpd-proxy-plugin]. This makes MPD query another MPD instance for all
library-related functionality.

```
# /storage/emulated/0/mpd.conf
database {
  plugin "proxy"
  host "192.168.1.100"
}
```

[mpd-proxy-plugin]: https://www.musicpd.org/doc/html/plugins.html#proxy

And that's it! The result is that I can play everything in my library on my
phone as if it were local, as long as my desktop is reachable (over VPN).

(Satellite setup is described [here][satellite-docs] in the MPD documentation.)

[satellite-docs]: https://www.musicpd.org/doc/html/user.html#satellite-setup

The entire `mpd.conf` on my phone contains only those two snippets:

```
# /storage/emulated/0/mpd.conf
music_directory "http://192.168.1.100:8000/

database {
  plugin "proxy"
  host "192.168.1.100"
}
```

## Bandwidth usage

One of the reasons I like Bandcamp is that it provides all music in
[FLAC][flac] format. These files are often very large, in the tens of
megabytes. That's fine on desktop, but mobile internet access still involves
data caps. I have a cheap data plan with only a few GB of data per month, and
I'd prefer not to upgrade. Streaming FLAC files burns through bandwidth
extremely quickly, so after a few days I decided I needed to do something about
that.

[flac]: https://xiph.org/flac/

In the past I sometimes transcoded music to a more space-efficient format for
storage on my phone, but because we are using the desktop music library we get
all the large files. Luckily, MPD does not actually care whether the file it
receives for playback uses the same encoding as the file indexed in the
library.

Since I'd prefer not to have to maintain transcoded copies of every single file
in my library, I ended up writing a simple web server that transcodes files on
the fly (by calling out to [FFmpeg][ffmpeg]). You can find the code and some
basic documentation on [Sourcehut][tms-srht] or [GitHub][tms-github]. I used
[Go][golang] for this, primarily because it has a web server with support for
things like [range requests][range-requests] built in.

```bash
transcoding-music-server \
    --target ~/.music.transcoded \
    --origin ~/music \
    --bind :8000
```

[ffmpeg]: https://ffmpeg.org/
[tms-srht]: https://git.sr.ht/~joram/transcoding-music-server
[tms-github]: https://github.com/jorams/transcoding-music-server
[golang]: https://golang.org/
[range-requests]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests

Using this instead of the Python web server, all music is transcoded to
bandwidth-efficient [Opus][opus]. The savings are significant: A random
sampling of files shows a 50 MB FLAC ends up transcoded to an Opus file of 4
MB, and an MP3 file of 10 MB ends up transcoded to an Opus file of 2 MB.

[opus]: https://www.opus-codec.org/

## Conclusion

The above setup works very well for me. When I want to play music on my phone I
open MAFA and select some music or continue the previously playing song, just
like I would with a more conventional player. Cover images are also handled
automatically.

Setting up the basics was surprisingly easy. The required configuration is
minimal, and everything worked immediately. Setting up transcoding obviously
took a bit more effort, but the server is only 85 lines long ([cloc][cloc]
calls it 69 lines of code).

[cloc]: https://github.com/AlDanial/cloc

If you still maintain a music library of your own and use MPD, this is a pretty
good way to get some of the benefits of a streaming service.

Not everything is perfect yet:

- I can't use headset controls to toggle playback

    I have sent a suggestion for this to the MAFA developer, but because it
    does not play the actual music this may prove to be hard. MPD itself also
    can't handle it [without migrating to a different sound
    API][mpd-sound-api].

[mpd-sound-api]: https://github.com/MusicPlayerDaemon/MPD/issues/500#issuecomment-473688358

- I need to be connected to my home network through VPN

    I currently use the built-in Android VPN settings, but those are very
    limited. Using it as an always-on VPN results in degraded performance when
    connected to my home network, but it's not possible to automatically toggle
    the VPN when I (dis)connect to my home network. I have set up
    [Tasker][tasker] to automatically open the VPN settings at the right time,
    but toggling involves some manual action.

    The IPSec-based VPN directly to my router also does not seem particularly
    fast. Since I don't want to expose MPD to the internet, I will have to find
    a better way to handle this.

[tasker]: https://play.google.com/store/apps/details?id=net.dinglisch.android.taskerm
