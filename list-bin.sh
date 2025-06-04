#!/usr/bin/env bash
jq -r '."install-plan"[] | select(.stage == "host") | ."bin-file" | strings' $1
