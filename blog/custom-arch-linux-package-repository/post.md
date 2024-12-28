---
title: Managing Arch Linux using a custom package repository
published: 2023-08-10
updated: 2024-09-28
---

I use [Arch Linux](https://archlinux.org/) (btw), and while the default package
repositories are pretty complete, for some things I need the
[AUR](https://aur.archlinux.org/). For several years I managed packages,
including those from the AUR and the odd custom package, separately on each
individual machine. This worked fine, but it was annoying to keep things up to
date and in sync. To clean that up I recently set up a custom package
repository.

## The basics

A [Pacman](https://archlinux.org/pacman/) package repository is a relatively
simple (compressed) TAR file, and it turns out they're pretty easy to manage.
The commands to do so ([`repo-add` and
`repo-remove`](https://man.archlinux.org/man/repo-add.8)) are included in the
[`pacman`](https://archlinux.org/packages/core/x86_64/pacman/) package, so I
didn't need to install anything special.

```sh
# Set up a directory for a repository
mkdir ./repo
# Include a package file
cp package-0.0.0-1-any.pkg.tar.zst ./repo/

# Add the package to the repo, creating the repo if it does not exist.
repo-add ./repo/custom-repo.db.tar.zst ./repo/package-0.0.0-1-any.pkg.tar.zst

# Remove the package
repo-remove ./repo/custom-repo.db.tar.zst package
```

Simply using `repo-add` once is enough to set up a usable package repository,
but Pacman is not aware of it yet. To make it available it needs a section in
[`pacman.conf`](https://man.archlinux.org/man/pacman.conf.5). Note that the
name in brackets should match the name of the repo file:

```conf
[custom-repo]
SigLevel = Optional TrustAll
Server = file:///path/to/repo
```

That's it. This repository is now a first-class citizen. It will be refreshed
on the next invocation of `pacman -Sy`, and packages in it can be installed
using `pacman -S`.

To make this package repository available on all my machines I synchronize the
directory using [Syncthing](https://syncthing.net/) and add it to `pacman.conf`
on each machine once. When a package needs to be added or updated, I only need
to do so once.

This works very well, and removes the need to build each package several times,
but there are a few problems left.

**Update 2024-09-28:** [As of pacman 7.0.0][pacman7] it supports a
`DownloadUser` setting which is enabled by default in `pacman.conf`. This user
(called `alpm`) needs access to the repo and package files for the package
repository. To make this easy I created a directory outside my home directory,
owned by my user and the group `alpm`. I do everything in that directory.

[pacman7]: https://archlinux.org/news/manual-intervention-for-pacman-700-and-local-repositories-required/

```sh
# Choose some location you like
sudo mkdir /var/custom-pacman-package-repo
sudo chown myuser:alpm /var/custom-pacman-package-repo

# Do everything in there
cd /var/custom-pacman-package-repo
mkdir ./repo
...
```

## Building in a chroot

When building custom packages using
[`makepkg`](https://man.archlinux.org/man/makepkg.8), it's common to add the
`-s` flag to install the dependencies required to build the package. These
dependencies will stay around even if they're not otherwise needed, which can
be solved by adding `-r`. It's also pretty easy to miss incorrectly specified
dependencies because something happens to already be installed, and fixing that
takes slightly more work.

This problem can be solved by using
[`makechrootpkg`](https://man.archlinux.org/man/makechrootpkg.1) to build
packages in a clean [chroot](https://en.wikipedia.org/wiki/Chroot). It requires
a bit more setup. First of all it needs a clean chroot to work with, which can
be created using the [`mkarchroot`](https://man.archlinux.org/man/mkarchroot.1)
command. Both of these are available in the
[`devtools`](https://archlinux.org/packages/extra/any/devtools/) package.

Since I want to be able to build packages for my repository that depend on
other packages in my repository, I also need the same `pacman.conf` from the
host system to be available in the chroot (the actual repo directory comes
later). Let's create the chroot:

```sh
# Install devtools
sudo pacman -S devtools

# Create a directory inside which to create a directory for the chroot
mkdir -p "./root"

# Create the chroot with just base-devel and dependencies installed
mkarchroot -C /etc/pacman.conf "./root/root" base-devel
```

This creates a clean chroot in the `root/root` directory, which `makechrootpkg`
will *not* directly be building packages in. Instead a working copy of this
chroot will be created when actually building a package.

Note that this clean chroot should be periodically kept up-to-date, otherwise
every new package build will spend time updating all outdated packages:

```sh
# Run pacman -Syu inside the chroot
arch-nspawn "./root/root" pacman -Syu
# Alternatively recreate the chroot completely
```

To build a package using `makechrootpkg` I pass in the following options:

- `-c` clean the chroot, resetting the working copy to the clean starting point
- `-u` updates the working copy, really just running `pacman -Syu`
- `-r` specifies the root directory to use. This should point to one directory
  above the actual chroot, so `./root`.
- `-D` specifies a directory to bind-mount into the chroot. I point this at my
  custom package repository.

```sh
repodir="$(realpath repo)"

# The package directory should contain a PKGBUILD
cd package
makechrootpkg -c -u -r ./root -D "$repodir"
```

The end result is a freshly built package and an unaffected host system. I can
add this package to my repository just like before:

```sh
pkgfile="package-0.0.0-1-any.pkg.tar.zst"
cp "$pkgfile" "$repodir/"
repo-add "$repodir/custom-repo.db.tar.zst" "$repodir/$pkgfile"
```

With this I now have a custom package repository and I build packages in a
clean environment. On other machines I can install these packages by name, so
only one machine needs to do notable work. That's a nice improvement over the
previous workflow, but there's more to do.

## Custom metapackages

There's a lot of overlap in the packages installed on each of my machines. When
installing a new machine I end up installing many of the same packages, and
sometimes I forget one until I run into the fact that it's missing some time
later. It would be nice to keep this in sync.

A normal installation of Arch Linux usually starts with the installation of the
[`base`](https://archlinux.org/packages/core/any/base/) package. This package
does not actually contain any files, but instead it only specifies a list of
dependencies. Installing the `base` package is really just a convenient
shortcut for installing each of those dependencies. `base` is a *metapackage*.

Now that we have a personal package repository, we can use the same technique
to conveniently manage the installation of a lot of other packages too.

I ended up dividing the set of packages I want to install into a few different
sets, some of which are pretty arbitrary:

- `j--meta-base` includes `base` itself as well as other things I want to be
  available on all systems.
- `j--meta-tools` includes various other tools that I will practically always
  want to have installed, like `emacs`. These tools don't need a graphical
  environment to be available.
- `j--meta-graphical` includes Xorg and a large variety of other packages for a
  graphical interface.
- `j--meta-audio` includes sound-related packages like `pipewire`, `mpd`, and
  interfaces for them.
- `j--meta-wireless` includes packages for wireless communication, like
  `connman` and `bluez`. They aren't always necessary on a desktop machine.
- `j--meta-gaming` includes `steam`, `lutris`, `discord` and similar.
- `j--meta-dev` includes programming-related packages, like `elixir`, `yarn`,
  `go`, etc.

Now on each machine I install any combination of these metapackages, and when I
add something else it's automatically included in the next update on each
machine.

## Wrapping this up

I wrapped up all of the above into a
[`./mkrepo`](https://git.sr.ht/~joram/dotfiles/tree/a5a80d23adcbeab072c17f3cdbc7fb7da960bb39/item/aur/mkrepo)
script that, when run without arguments, rebuilds every package and adds it to
the repository. When given a package name as an argument it only builds the
specified package. It's not perfect, but it works nicely.

## What's left?

Setting up this repository has made it much easier to manage multiple machines,
but synchronizing the repository using Syncthing means I first need to install
Syncthing when setting up a new system. I still first bootstrap a new system
using `base`, while it would be nicer to bootstrap using `j--meta-base`.
Whenever I need to do this next I plan to solve it by hosting the repository
online and adding it to the installation disk.

To install packages from the AUR I currently copy them into my repository,
which means I need to keep them up to date manually. I've primarily done it
this way because I don't want to deal with git submodules, but with a larger
number of packages it will get tedious.

I still don't have a great workflow for keeping packages up to date when
necessary. I just notice breakage or another reason to update a package, then
rebuild it. I assume there are better solutions for this, but I haven't looked
into it yet.

Lastly there are still some things I keep outside of packages, most notably
[StumpWM](https://stumpwm.github.io/). I rebuild StumpWM outside of the regular
update process relatively frequently, so I'm happy to keep it unpackaged. This
does mean after every update of [SBCL](https://sbcl.org/) I need to rebuild the
binary again, so it might change at some point.
