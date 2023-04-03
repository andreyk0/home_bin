#!/bin/bash
#
# https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html
#
TERM=alacritty-direct exec emacsclient --alternate-editor= -nw "$@"
