#!/bin/bash

for file in tests/*.py
do
	res=${file%.py}.res
	expect=${file%.py}.txt
	in=${file%.py}.in
	if [ -f $in ]; then
		../ptipython.sh $file < $in > $res 2> /dev/null
	else
		../ptipython.sh $file > $res 2> /dev/null
	fi
	if diff $res $expect > /dev/null; then
		echo -e "\033[0;32m[OK] $file\033[0m"
	else
		echo -e "\033[0;31m[KO] $file\033[0m"
		diff $res $expect
		echo ""
	fi
done
