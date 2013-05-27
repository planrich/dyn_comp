
for i in $(seq 0 6); do
    ./eval/sudoku $i\
        | awk '$1 ~ /evaluated/ { print $3 };' \
        | sed -e 's/\]$//; s/\]/\n/g; s/[\$()\[]//g; s/\,/ /g' \
        | awk 'BEGIN { r = 0; }; r % 3 == 0 { print '\n'; }; { r++; print $1,$2,$3," ", $4, $5, $6, " ", $7, $8, $9 }' > "ghc.sol$i"
done
