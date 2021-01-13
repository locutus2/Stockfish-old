#/usr/bin/bash
f=$1
p=$2
d=$3
n=`grep ' '$d'$' $f|wc -l|cut -d' ' -f1`
echo "n="$n;
np=$[$n * $p / 100]
echo "np="$np
q=`grep ' '$d'$' $f|cut -d' ' -f1-|head -$np|tail -1`
echo $q
