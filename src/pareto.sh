#/usr/bin/bash

export LC_ALL=C
grep Hits | sort +8nr +2nr | gawk 'BEGIN{n=0; print "\n--- pareto ---";} {x=$3; if(x >= n) { n = x; print $0;}}'
