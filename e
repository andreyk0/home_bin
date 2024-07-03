#!/bin/bash
#
# https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html
#
exec emacsclient --alternate-editor= -nw "$@"
