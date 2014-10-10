# Pacotes
library("tm")    # Trabalhar com strings
library("psych") # Função describe

# Abrir banco de dados de atividades ----
sociodemografico  <- read.csv("../preditores/caracterizacao/sociodemografico.csv")

## Retirar questões inuteis
sociodemografico <- sociodemografico[, -c(40:86)]

## Criar variável nome completo 
sociodemografico$nomecompleto <- paste(sociodemografico$nome, sociodemografico$sobrenome)
sociodemografico$nomecompleto <- stripWhitespace(tolower(sociodemografico$nomecompleto))


## Criar banco somente com pessoas que consentiram participar
sociodemografico  <- subset(sociodemografico, sociodemografico$termo == "Sim")

## Abrir banco de logs Tratados
dfCast <- read.csv("caracterizacao/logsTratados.csv")

# Mesclar bancos ----
dfCast$.id  <- as.character(dfCast$.id)
dfCast$nomecompleto  <- stripWhitespace(tolower(dfCast$.id))

sociodemografico$nomecompleto  <- as.character(sociodemografico$nomecompleto)

## Mesclar e preservar todos os valores
bancoFinal  <- merge(sociodemografico, dfCast, by.x = "nomecompleto", by.y = "nomecompleto")


