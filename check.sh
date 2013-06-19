#! /bin/bash
#
# check.sh
# Copyright (C) 2013 rich <planrichi@gmail.com>
#
# Distributed under terms of the MIT license.
#


for i in $(seq 0 6); do
    echo "checking solution: $i"
    diff "ghc.sol$i" "am.sol$i"
    if [ "$?" -ne "0" ]; then
        echo "ERROR in solution $i"
    fi
done


