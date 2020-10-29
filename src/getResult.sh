#!/bin/bash
SUFFIX=$1
LOG=log${SUFFIX}.txt
RESULT=result${SUFFIX}.txt
echo $LOG $RESULT
grep 'Err=[0-9]' $LOG |cut -d' ' -f1|cut -d'=' -f2|tr '.' ',' >$RESULT; 
tail $RESULT; 
grep epoch $LOG|tail -1
