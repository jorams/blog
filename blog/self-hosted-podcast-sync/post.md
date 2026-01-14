---
title: Self-hosting podcast synchronization
published: 2026-01-14
---

A few months ago I figured I should set up synchronization for my different
podcast clients. I was specifically looking for something to host for myself.
The clients I use ([AntennaPod][antennapod] and [Kasts][kasts]) support both
[gpodder.net][gpodder] and [GPodder Sync for Nextcloud][gpodder-nextcloud].

[antennapod]: https://antennapod.org/
[kasts]: https://apps.kde.org/kasts/
[gpodder]: https://gpodder.net/
[gpodder-nextcloud]: https://apps.nextcloud.com/apps/gpoddersync

Gpodder.net is a free hosted service. I don't really have a use for most of its
features. [Self-hosting the service][mygpo-self-hosting] looks daunting because
it's very much designed for the gpodder.net hosted service.

[mygpo-self-hosting]: https://gpoddernet.readthedocs.io/en/latest/dev/installation.html

The Nextcloud GPodder Sync app is much simpler. [Its API][nextcloud-api] is
inspired by very limited elements of the gpodder.net API. The biggest problem
is that I don't use [Nextcloud][nextcloud].

[nextcloud-api]: https://github.com/thrillfall/nextcloud-gpodder#api
[nextcloud]: https://nextcloud.com/

There is also [oPodSync][opodsync] which is compatible with both APIs. It
should be easy to self-host in a PHP environment, but I don't have one of those
and I was planning on keeping it that way.

[opodsync]: https://fossil.kd2.org/opodsync/doc/main/README.md

After some deliberation I decided to build something for myself in Elixir.

## Implementation

My requirements were simple:

- Compatible with the API of the Nextcloud GPodder Sync app
- Easy to self-host

Notably I didn't need multi-user support or authentication. I host the server
on my [Tailscale][tailscale] network (using [Headscale][headscale] for the
control server) and don't expose it to the internet.

[tailscale]: https://tailscale.com/
[headscale]: https://headscale.net/

First I needed to simulate a [Nextcloud login][nextcloud-login] which turned
out to be [very easy][login-implementation].

[nextcloud-login]: https://docs.nextcloud.com/server/stable/developer_manual/client_apis/LoginFlow/index.html#login-flow-v2
[login-implementation]: https://codeberg.org/jorams/pod-sync/src/branch/master/lib/pod_sync_web/controllers/login_controller.ex

Then there are two endpoints: subscriptions and episode actions.

Subscriptions should be provided by the API as a list of URLs to add and a list
of subscriptions to remove since a specified timestamp. Changes are uploaded by
similarly specifying URLs to add and URLs to remove. I implemented this by
storing a subscription URL, whether or not the subscription is active, and when
that last changed.

Episode actions are more complicated: The [gpodder.net
API][gpodder-episode-actions] seems to maintain a history of actions applied to
each episode. After some minor testing I discovered the Nextcloud GPodder Sync
app does much less: It doesn't store the _history_ of episode actions, it only
stores the latest action for an episode. I decided to do the same.

[gpodder-episode-actions]: https://gpoddernet.readthedocs.io/en/latest/api/reference/events.html

And with that the application [is done][repo]. It's built on [Phoenix][phoenix]
and uses [Ecto][ecto] to store data in an [SQLite][sqlite] database, and can be
deployed in a container. The code probably won't win any beauty contests, but
it's simple enough for that not to matter much.

[phoenix]: https://www.phoenixframework.org/
[ecto]: https://hexdocs.pm/ecto/
[sqlite]: https://sqlite.org/

## Deployment

I deployed the application using [Podman][podman], specifically as a systemd
service using [Quadlet][quadlet].

[podman]: https://podman.io/
[quadlet]: https://docs.podman.io/en/latest/markdown/podman-systemd.unit.5.html

```systemd
# /etc/containers/systemd/pod-sync.container

[Unit]
Description=PodSync

[Container]
Image=oci-archive:/path/to/container-images/pod-sync.tar.gz
Pull=never
PublishPort=127.0.0.1:4000:4000
Volume=pod-sync-data:/data
Environment=DATABASE_PATH=/data/pod_sync.sqlite
Environment=PORT=4000
Environment=PHX_HOST=podsync.example.com
Environment=SECRET_KEY_BASE=<generate using mix phx.gen.secret>

[Service]
Restart=on-failure

[Install]
WantedBy=default.target
```

The application is served using [Caddy][caddy], which handles TLS. It is
configured not to serve anything to the internet.

[caddy]: https://caddyserver.com/

```caddy
podsync.example.com {
    @not-tailnet not remote_ip 127.0.0.0/8 100.64.0.0/10 fd7a:115c:a1e0::/48
    abort @not-tailnet
    reverse_proxy 127.0.0.1:4009
}
```

Headscale then provides all devices in the tailnet a DNS record pointing to the
server on the tailnet.

```yaml
dns:
  extra_records:
    - { name: podsync.example.com, type: A, value: 100.64.0.123 }
```

## Code

You can find the repository [on Codeberg][repo], along with some further
instructions.

[repo]: https://codeberg.org/jorams/pod-sync
