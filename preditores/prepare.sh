#!/bin/sh

# go to logs folder
cd logs

for i in {302..486}

do  
  # Remove the first and second line of every txt
  sed '1,3d' <class$i.csv>class_$i.txt
  
  # Insert class number 
  awk -F, -v variable=$i '{$1=variable "\t"  $1}1' OFS=, class_$i.txt >> class.final.$i.txt

  # Remove temporary files
  rm -f class_$i.txt class$i.txt

done

# Merge all files into log.txt
cat *.txt >> log.txt
