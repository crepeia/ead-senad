#!/bin/sh

# Remove lines, insert column with the filename and merge all of them.

# Class start with 302 and ends with 486.

# Navigate to folder


AWKOPT1="'{$1=\"" 
AWKOPT2="\" \"\t\"  $1}1'"

cd logs

for i in {302..305}
do

  CLASS="Class"$i

  # Remove the first and second lines
  sed '1,3d' <class$i.txt>class_$i.txt

  # Insert a column with the file name
  # awk -F, '{$1="NA "\t"  $1}1' OFS=, class_$i.txt >> class.final.$i.txt

  awk -F, ${AWKOPT1}${CLASS}${AWKOPT2} OFS=, class_$i.txt >> class.final.$i.txt


  rm -f class_$i.txt class$i.txt

done

cat *.txt >> log.txt
