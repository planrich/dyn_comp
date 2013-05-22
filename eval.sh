#! /bin/bash
#
# eval.sh
# Copyright (C) 2013 rich <planrichi@gmail.com>
#
# Distributed under terms of the MIT license.
#

for i in $(seq 0 6); do
    sed -e "s/sudoku_solve (sudoku 5)/sudoku_solve (sudoku $i)/" test/sudoku.am > test.am
    time -f "\nAM $i => %E" ./cabal-dev/bin/am test.am
done

for i in $(seq 0 6); do
    time -f "\nHUGS $i => %E" runhugs eval/sudoku.hs $i
done

for i in $(seq 0 6); do
    time -f "\nHUGS $i => %E" ./eval/sudoku $i
done

