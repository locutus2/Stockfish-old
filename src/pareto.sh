#/usr/bin/bash

export LC_ALL=C
grep Hits | sort +8nr | gawk 'BEGIN{n=0; print "--- pareto ---";} {x=$3; if(x >= n) { n = x; print $0;}}'
