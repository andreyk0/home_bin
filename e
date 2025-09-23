#!/bin/bash
set -e

base_args=("--alternate-editor=" "-nw" "--tty" "--create-frame")

if [ $# -eq 0 ] && ! [ -t 0 ]; then
  # --- Pager Mode: Capture STDIN to a temporary file ---
  temp_file=$(mktemp)
  trap 'rm -f "$temp_file"' EXIT INT TERM HUP
  cat > "$temp_file"
  emacsclient "${base_args[@]}" "$temp_file"
else
  exec emacsclient "${base_args[@]}" "$@"
fi
