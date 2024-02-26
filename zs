#!/bin/sh
if [ -z "$1" ] ; then
  n="${PWD##*/}"
else
  n="$1"
fi
exec nice -n 20 zellij -s "$n"
