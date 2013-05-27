
for i in $(seq 0 6); do
    sed -e "s/sudoku_solve (sudoku 5)/sudoku_solve (sudoku $i)/" test/sudoku.am > test.am
    time -f "\nAM $i => %E" ./cabal-dev/bin/am test.am\
        | awk '$1 ~ /evaluated/ { print $2 };' \
        | sed -e 's/\]$//; s/\,\]\,/\n/g; s/[\$()\[]//g; s/\,/ /g;' \
        | awk 'BEGIN { r = 0 }; r % 3 == 0 { print '\n'; }; { r++; print $1,$2,$3," ", $4, $5, $6, " ", $7, $8, $9 }' > "am.sol$i"
done
