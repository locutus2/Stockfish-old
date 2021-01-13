#/usr/bin/bash
f=$1
s=$2

n=`wc -l $f|cut -d' ' -f1`

p=$s
while [ $p -lt 100 ]
do
	#echo "n="$n;
	np=$[$n * $p / 100]
	#echo "np="$np
	q=`cut -d' ' -f1- $f|head -$np|tail -1`
	#echo $q
	echo $p $q
	
	p=$[$p + $s]
done
