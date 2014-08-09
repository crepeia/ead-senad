#!/bin/sh

# Remove lines, insert column with the filename and merge all of them.

# Class start with 302 and ends with 486.

# Navigate to folder
cd logs

for i in {302..305}
do
  # Remove the first and second lines
  # awk 'NR > 2 { print }' < class$i.txt

  # Insert a column with the file name
  awk -F, '{$1=++i FS "class$i"}1' OFS=, class$i.txt   
done



http://unix.stackexchange.com/questions/37790/how-do-i-delete-the-first-n-lines-of-an-ascii-file-using-shell-commands
http://www.theunixschool.com/2012/11/awk-examples-insert-remove-update-fields.html

