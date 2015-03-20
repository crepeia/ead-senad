#!/bin/sh

# This script fetch all moodle log datas by web. Be advised: the courses names are given by moodle and they do not represent the formal name.

# Create variables to concatenate in wget command.
WGET_OPTS="--load-cookies=cookies.txt"
URLBASE="http://www.projetosenad.ufjf.br/report/log/index.php?chooselog=1&showusers=1&showcourses=1&id=3&group="
URLSUFFIX="&user=&date=0&modid=&modaction=&logformat=downloadascsv"

# Fetch logs by class
# Class start with 302 and ends with 486.

# Create directory for log files
mkdir logs

for i in {302..486}
do
  wget -O logs/class$i.csv ${WGET_OPTS} "${URLBASE}$i${URLSUFFIX}"
done
