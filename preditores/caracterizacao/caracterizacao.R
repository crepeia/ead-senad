# Pacotes
library("tm")       # Trabalhar com strings
library("psych")    # Função describe
library("plyr")     # Função ldply
library("ggplot2")  # Gráficos

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

## Notas
notas  <- read.csv("caracterizacao/notas.csv")


####################################################################################################
## ARTIGO 1 -----------------------------------------------------------------------------------------
## Descrição de um curso online para prevenção do uso de drogas para educadores de Escolas Públicas.
####################################################################################################

# Abrir banco de dados
socioDemo  <- read.csv("../praticas-profissionais/praticasprofissionais_df.csv")

########################################
# CARACTERÍSTICAS SOCIODEMOGRAFICAS ----
########################################

# Funçao para criar tabelas com porcentagem
porcentagem <- function(var){
  a  <- round(prop.table(table(var)),3)*100
  print(cbind(sort(a, decreasing = TRUE)))
}

# Idade
## Recodificar variável para numérica
socioDemo$age  <- as.numeric(as.character(socioDemo$idade))
## Excluir usuários que inseriram o ano de nascimento
socioDemo$age <- ifelse(socioDemo$age > 70, NA, socioDemo$age)

## Média de idade
mean(socioDemo$age, na.rm = TRUE)
## Desvio Padrão 
sd(socioDemo$age, na.rm = TRUE)

## Sexo
porcentagem(socioDemo$sexo)

## Escolaridade
porcentagem(socioDemo$escolaridade)

## Ocupaçao
porcentagem(socioDemo$ocupacao)

## Contato Anterior
porcentagem(socioDemo$contatoanterior)

## Onde lida
porcentagem(socioDemo$lida.onde)

## Motivo Curso
porcentagem(socioDemo$motivocurso)

########################################
# SATISFAÇÃO COM O CURSO  ----
########################################

## Material Didático
porcentagem(socioDemo$materialdidatico)

## Prazo Atividades
porcentagem(socioDemo$prazoatividades)

## Interação entre pares
porcentagem(socioDemo$interacaopares)

## Organização curso
porcentagem(socioDemo$organizacaocurso)

## Importância da ajuda do tutor
porcentagem(socioDemo$import.ajud.tutor)

## Participação em outro curso
porcentagem(socioDemo$part.outrocurso)

########################################
# NOTAS                             ----
########################################

## Ler banco com notas
notas  <- read.csv("caracterizacao/notas.csv", dec = ",", na.strings=c("NA","-"))

# Listar variáveis com notas
summary(notas[,7:36])

# Fóruns ------
# =============

# Selecionar somente os foruns
notasForum  <- notas[, c("forum1", "forum2", "forum3","forum4","forum5","forum6","forum7","forum8","forum9","forum10","forum11","forum12","forum13","forum14","forum15")]

# Notas medias dos foruns
round(sapply(notasForum, mean, na.rm = TRUE),2)

# Desvio padrão
round(sapply(notasForum, sd, na.rm = TRUE),2)

# Preparacao dos dados para realizacao do grafico
forum  <- lapply(notasForum, is.na)
forumTeste <- lapply(forum, table)
dfForum <- ldply(forumTeste)
colnames(dfForum)  <- c("forum", "atividade", "falta")
dfForum$forum  <- as.factor(dfForum$forum)

# Grafico
plot(dfForum[,2], xlab="Fóruns", ylab="Número de acessos", type = "l", col = "blue", xlim=c(1,15))



