# `empress`: MPRIS media controls made simple

This tool functions as a simple command-line abstraction over the D-Bus [MPRIS
specification](https://specifications.freedesktop.org/mpris-spec/latest/) which
allows for querying and controlling media players.  It functions similarly to
[playerctl](https://github.com/altdesktop/playerctl/), but features more basic
controls and a more complex method of tracking which player you want to control.
`empress` uses its own D-Bus daemon to keep track of which players are currently
playing (and which players have been updated the most recently), and uses this
information to decide you want to pause Spotify rather than playing a minimized
YouTube video.

## Installation

To install `empress`, simply use Cargo:

```sh
$ cargo install empress
```

Then launch the daemon process in order to use it — you will probably want to
put this in an rcfile or service definition:

```sh
$ empress server
```

The server will gracefully shut down if interrupted or signalled with `SIGTERM`.

## Usage

Once you have the daemon running, you can communicate with it using one of
several subcommands, e.g.:

```sh
$ empress play-pause
```

The current list of commands includes, `next`, `previous`, `pause`,
`play-pause`, `stop`, and `play`, which all function as you would expect.  For
more help you can always run `empress help`.
