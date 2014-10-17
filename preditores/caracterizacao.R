# Definir diretório de trabalho
setwd("caracterizacao/")

# Pacotes
library("tm")    # Trabalhar com strings
library("psych") # Função describe
library("reshape2")

# SOCIODEMOGRAFICO ----
## Ler banco
sociodemografico  <- read.csv("sociodemografico.csv")

## Retirar questões desnecessárias
sociodemografico <- sociodemografico[, -c(40:86)]

## Criar variável nome completo 
sociodemografico$nomecompleto <- paste(sociodemografico$nome, sociodemografico$sobrenome)
sociodemografico$nomecompleto <- stripWhitespace(tolower(sociodemografico$nomecompleto))

## Converter para Character
sociodemografico$nomecompleto  <- as.character(sociodemografico$nomecompleto)

## Criar banco somente com pessoas que consentiram participar
sociodemografico  <- subset(sociodemografico, sociodemografico$termo == "Sim")

# LOGS TRATADOS ----
dfCast <- read.csv("logsTratados.csv")
dfCast$.id  <- as.character(dfCast$.id)
dfCast$nomecompleto  <- stripWhitespace(tolower(dfCast$.id))

# NOTAS DOS ALUNOS ----
notas  <- read.csv("notas.csv", na.strings="-")
notas <- notas[, -c(5,15,16,17,18,20,21,31)]
summary(notas)

## Soma das notas em foruns
notas$somaForum  <- notas$forum1 + notas$forum2 + notas$forum3 + notas$forum4 + notas$forum5 + notas$forum6 + notas$forum7 +notas$forum8 + notas$forum9 + notas$forum10 + notas$forum11 + notas$forum12 + notas$forum13 + notas$forum14 + notas$forum15

##  Atividade dos alunos no fórum

notas$f1 <- ifelse(is.na(notas$forum1), 0, 1)
notas$f2 <- ifelse(is.na(notas$forum2), 0, 1)
notas$f3 <- ifelse(is.na(notas$forum3), 0, 1)
notas$f4 <- ifelse(is.na(notas$forum4), 0, 1)
notas$f5 <- ifelse(is.na(notas$forum5), 0, 1)
notas$f6 <- ifelse(is.na(notas$forum6), 0, 1)
notas$f8 <- ifelse(is.na(notas$forum8), 0, 1)
notas$f9 <- ifelse(is.na(notas$forum9), 0, 1)
notas$f10 <- ifelse(is.na(notas$forum10), 0, 1)
notas$f11 <- ifelse(is.na(notas$forum11), 0, 1)
notas$f12 <- ifelse(is.na(notas$forum12), 0, 1)
notas$f13 <- ifelse(is.na(notas$forum13), 0, 1)
notas$f14 <- ifelse(is.na(notas$forum14), 0, 1)
notas$f15 <- ifelse(is.na(notas$forum15), 0, 1)

## Soma das notas dos questionários
notas$somaQues <- notas$quesm1 + notas$quesm2 + notas$quesm3

## Soma das notas das atividades colaborativas
notas$somaAtivcol  <- notas$ativcolm1 + notas$ativcolm2 + notas$ativcolm4

# Mesclar bancos ----
## Mesclar e preservar todos os valores
bancoFinal  <- merge(sociodemografico, dfCast, by.x = "nomecompleto", by.y = "nomecompleto")



