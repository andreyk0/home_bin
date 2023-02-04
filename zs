#!/bin/sh
exec nice -n 20 zellij -s "$1"
