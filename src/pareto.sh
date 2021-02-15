#/usr/bin/bash

export LC_ALL=C
grep Hits | sort +8nr +10n +2nr | gawk 'BEGIN{n=1000; m=0; print "\n--- pareto ---";} {x=$11; y=$3; if(x < n || (x == n && y > m)) { n = x; m = y; print $0;} }'
