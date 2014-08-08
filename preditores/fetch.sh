#!/bin/sh

WGET_OPTS="--load-cookies=cookies.txt"
URLBASE="http://www.projetosenad.ufjf.br/report/log/index.php?id=3&chooselog=1&user=0&date=0&modid&modaction&group=0&perpage=10000&page="

#for i in 1 ... 10
#do
#   echo ${WGET_OPTS} "${URL_BASE} ${i}"
#done


for i in {0..5}
do
  wget -k ${WGET_OPTS} ${URLBASE}$i 
done



