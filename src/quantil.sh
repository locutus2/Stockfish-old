#/usr/bin/bash
f=$1
p=$2
n=`wc -l $f|cut -d' ' -f1`
echo "n="$n;
np=$[$n * $p / 100]
echo "np="$np
q=`cut -d' ' -f1- $f|head -$np|tail -1`
echo $q
