#!/bin/sh
# Verifies that --cross-runner-arg preserves spaces inside a single argument!
if [ "$1" != "--magic-tag" ] || [ "$2" != "hello world" ]; then
    echo "Runner wrapper received wrong args: $*" >&2
    exit 1
fi
shift 2
exec "$@"
