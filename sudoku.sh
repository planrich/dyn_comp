#! /bin/bash
#
# sudoku.sh
# Copyright (C) 2013 rich <planrichi@gmail.com>
#
# Distributed under terms of the MIT license.
#


./dist/build/am/am test/sudoku.am \
    | awk '$1 ~ /evaluated/ { print $2 };' \
    | sed -e 's/\]$//; s/\,\]\,/\n/g; s/[\$()\[]//g; s/\,/ /g' \
    | awk 'BEGIN { r = 0; print "solved sudoku:\n" }; r % 3 == 0 { print '\n'; }; { r++; print $1,$2,$3," ", $4, $5, $6, " ", $7, $8, $9 }'

