#!/bin/bash
SUFFIX=$1
LOG=log${SUFFIX}.txt
RESULT=result${SUFFIX}.txt
echo $LOG $RESULT
head -1 $LOG
grep 'Err=[0-9]' $LOG |cut -d' ' -f1,2|cut -d'=' -f2,3|tr '.' ','|sed 's/PErr=//' >$RESULT; 
tail $RESULT; 
grep epoch $LOG|tail -1
