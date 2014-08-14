---
title: "README"
author: "Henrique Pinto Gomide"
output:
  pdf_document:
    toc: yes
  html_document:
    toc: yes
  word_document: default
---


# How to download the log files

This file aims to help you collect all log files from Moodle when no direct access to mysql tables is available.


## Get Started

The following commands work on linux and maybe on a mac. You will be able to download all log files. Before you start, you will need to use a firefox extension to export your cookies. You can download it here: https://addons.mozilla.org/en-US/firefox/addon/export-cookies/

After the extension download, open your portal with your admin and password. Once you are logged in, go to "tools" in Firefox and click on export cookies. Save the file and put it in the project folder (eg '$ cd ~/ead-senad/preditores').

If you don't have a password or login, reach me at henriquepgomide@gmail.com


## Download files

To download, you need to open the terminal on Linux and navigate to this folder (eg '$ cd ~/ead-senad/preditores'). After that, run the script _fetch.sh_. Make sure you have the _cookies.txt_ file in the folder (Read the previous section to know more). 

This is going to take a looong time. You should mind that at the middle of the course the whole log file is estimated to have ~ 700 MB with two million lines.

If everything went right, you will be able to see all files in the 'log' folder. 


## Cleaning up

Before open the log file in R enviroment, you will need to use the 'prepare.sh' script. Run in on Terminal. This script removes the first two lines of every file, insert a column with the class number and then merge all log files into a single file called 'log.txt'.

## Open the log.txt in R

Ok. Just open the log.R script and follow the recipe.

# Help?

Reach me at henriquepgomide@gmail.com




