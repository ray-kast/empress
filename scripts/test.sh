#!/bin/bash

set -e

cd "$(dirname "$0")/.."

cargo build --bin empress

echo $'\x1b[1mStarting server...\x1b[m'
target/debug/empress server &
sleep 0.2

echo $'\x1b[1mRunning client...\x1b[m'
target/debug/empress play-pause

echo $'\x1b[1mStopping server...\x1b[m'
kill -TERM %1
sleep 0.2

echo $'\x1b[1mDone.\x1b[m'
