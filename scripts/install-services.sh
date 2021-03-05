#/bin/bash

function usage() {
  cat <<EOF >&2
Usage: scripts/install.sh [FLAGS] [PATH]

  Installs (or uninstalls) service files for empress. PATH is an absolute path
  to the empress binary, and is required if -r is not present.

Flags:
  -h    Display this message and quit.
  -l    Install service files to a local directory.
  -n    Generate service files in target/ but do not install them.
  -r    Remove already-installed service files.
EOF
}

set -e

loc=''
noop=''
remove=''

while getopts "hlnr" opt; do
  case $opt in
    h)
      usage
      exit 0
      ;;
    l)
      loc='1'
      ;;
    n)
      noop='1'
      ;;
    r)
      remove='1'
      ;;
    \?)
      usage
      exit 1
      ;;
  esac
done

shift $((OPTIND - 1))

if [[ -z "$remove" ]] && (( $# != 1 )); then
  usage
  exit 1
fi

cd "$(dirname "$0")/.."

systemd_file=empress.service
dbus_file=net.ryan_s.Empress1.service

if [[ -z "$remove" ]]; then
  sed -e "s:/path/to/empress:$1:" "etc/$systemd_file.in" >"target/$systemd_file"
  sed -e "s:/path/to/empress:$1:" "etc/$dbus_file.in" >"target/$dbus_file"
fi

[[ -n "$noop" ]] && exit 0

systemd_dir="/usr/lib/systemd/user/"
dbus_dir="/usr/share/dbus-1/services/"

if [[ -n "$loc" ]]; then
  base="$XDG_DATA_HOME"

  [[ -n "$base" ]] || base="$HOME"

  systemd_dir="$base/.local/share/systemd/user"
  dbus_dir="$base/.local/share/dbus-1/services"
fi

mkdir -p "$systemd_dir"
mkdir -p "$dbus_dir"

fail=0

if [[ -z "$remove" ]]; then
  (
    install -vDm644 "target/$systemd_file" -t "$systemd_dir" && \
    install -vDm644 "target/$dbus_file" -t "$dbus_dir"
  ) || fail=$?
else
  mv -v "$systemd_dir/$systemd_file" /tmp || fail=$?
  mv -v "$dbus_dir/$dbus_file" /tmp || fail=$?
fi

if (( $fail != 0 )); then
  echo "ERROR: Installation failed."
  exit $fail
fi
