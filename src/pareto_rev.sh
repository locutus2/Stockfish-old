#/usr/bin/bash

export LC_ALL=C
grep Hits | sort +8n +2nr +10n | gawk 'BEGIN{n=0; m = 1000; print "\n--- pareto_rev ---";} {x=$3; y=$11; if(x > n || (x == n && y < m)) { n = x; m = y; print $0;}}'
