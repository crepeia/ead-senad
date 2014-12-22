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

# Introduction

This repository has the undentified data e scripts created to analyse the online course "Prevenção do uso de drogas" (Substance use disorders prevention in Schools for teachers)

The research is conducted by Crepeia - Center for Research, Intervention and Evaluation for Alcohol & Drugs from Universidade Federal de Juiz de Fora.

You can reach us at www.ufjf.br/crepeia

# What about 'preditores' folder?

The preditores folder contains scripts from two studies studies that describe the course utilisation by users and evaluate how well the main project was implemented. 

The folder is divided into the following folders:

## Data

As the name points out, the folder has all data. 

## Study 1

The folder has files used to analyse participants characteristics and course usage. It is divided into:

* 'Estudo 1- caracterizacao.R' - R script with all analyses to identify users characteristics.
* 'est1 - relatorioMapa' - with an R script who answers the question: Is there any geographical difference among course completers and dropous? - The answer is no.
* 'figs - estudo 1' - All figures from script 'Estudo 1 - caracterizacao.R'

## Study 2

The folder has files with analyses from the paper: '' (draft). It is divided into:

* Estudo 2 - avalicaoProcesso.R - R script with all data comparing beliefs before/after the course.
* est2 - textMining - R script used to mine text from questionnaires and create the categories used in the main analyses. The full procedure is described in the paper.
* figs - estudo 2 - Fancy charts


## Logs

This folder has an R and Shell scripts to download Logs from Moodle. The main advantage of this procedure is that you do not need a Mysql access do download all Moodle data. Needs some hacking to work with different courses.

### Get Started

The following commands work on linux and maybe on a mac. You will be able to download all log files. Before you start, you will need to use a firefox extension to export your cookies. You can download it here: https://addons.mozilla.org/en-US/firefox/addon/export-cookies/

After the extension download, open your portal with your admin and password. Once you are logged in, go to "tools" in Firefox and click on export cookies. Save the file and put it in the project folder (eg '$ cd ~/ead-senad/preditores').

If you don't have a password or login, reach me at henriquepgomide@gmail.com


### Download files

To download, you need to open the terminal on Linux and navigate to this folder (eg '$ cd ~/ead-senad/preditores'). After that, run the script _fetch.sh_. Make sure you have the _cookies.txt_ file in the folder (Read the previous section to know more). 

This is going to take a looong time. The log file is about 1,1GB.

If everything went right, you will be able to see all files in the 'log' folder. 


### Cleaning up

Before open the log file in R enviroment, you will need to use the 'prepare.sh' script. Run in on Terminal. This script removes the first two lines of every file, insert a column with the class number and then merge all log files into a single file called 'log.txt'.

### Open the log.txt in R

Ok. Just open the log.R script and follow the recipe.

# Help?

Reach me at henriquepgomide@gmail.com





