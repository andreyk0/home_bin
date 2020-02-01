#!/bin/sh

# https://lore.kernel.org/linux-btrfs/16325152.4fYaUy9WYm@merkaba/T/#m37980a902ee5afd0b504dd15604713a0d03cc746

btrfs sub create sub_tmp

mkdir sub_tmp/single

head -c 2047 /dev/urandom > sub_tmp/single/inline_file

for x in $(seq 1 18); do 
	cp -a sub_tmp/single sub_tmp/double
	mv sub_tmp/double sub_tmp/single/$x
done

sync

btrfs sub del sub_tmp

