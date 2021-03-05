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
put this in an rcfile (or see below for setting up a service):

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

## Installing `empress` as a Service

`empress` can be installed as a service to allow autostarting and lifecycle
management using D-Bus and systemd.  To do this, run the following:

```sh
$ scripts/install-services.sh -l <path to empress binary>
```

This will install the D-Bus and systemd service files into `~/.local/share`.  If
you installed `empress` with `cargo install`, the path to the binary will
probably look something like `/home/<user>/.cargo/bin/empress`.

If you want to make a system-wide installation, simply omit the `-l` flag.  This
will install the session service files into `/usr` instead (but note that
`empress` **never** runs as a D-Bus system bus).

### Uninstalling

To remove installed service definitions, simply run:

```sh
$ scripts/install-services.sh -rl
```

Like with above, remove the `-l` to uninstall the service files from the system
folders.

