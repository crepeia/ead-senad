# Pacotes
# Remember to install packages before loading them!

library("psych")    # Função describe
library("plyr")     # Função ldply
library("ggplot2")  # Gráficos

# Função Multiplot - This function allows creation of multiple plots using the ggplot2 package.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Abrir banco de dados de atividades ----
sociodemografico  <- read.csv("logs/sociodemografico.csv", na.strings="-")

## Retirar questões inuteis
sociodemografico <- sociodemografico[, -c(40:86)]

## Criar variável nome completo 
sociodemografico$nomecompleto <- paste(sociodemografico$nome, sociodemografico$sobrenome)
sociodemografico$nomecompleto <- stripWhitespace(tolower(sociodemografico$nomecompleto))

## Criar banco somente com pessoas que consentiram participar
sociodemografico  <- subset(sociodemografico, sociodemografico$termo == "Sim")

## Abrir banco de logs Tratados
dfCast <- read.csv("logs/logsTratados.csv")

## Abrir banco de atividades colaborativas
ativCol  <- read.csv("logs/atividadesColaborativas-respostas.csv", dec = ",", na.strings="-")

## Criar banco somente com pessoas que consentiram participar
ativCol  <- subset(ativCol, ativCol$termo == "Sim")

# Mesclar bancos ----
dfCast$.id  <- as.character(dfCast$.id)
dfCast$nomecompleto  <- tolower(gsub(" ", "", dfCast$.id))

sociodemografico$nomecompleto  <- as.character(sociodemografico$nomecompleto)
sociodemografico$nomecompleto  <- tolower(gsub(" ", "", sociodemografico$nomecompleto))

## Mesclar e preservar todos os valores
bancoFinal  <- merge(sociodemografico, dfCast, by.x = "nomecompleto", by.y = "nomecompleto", all = TRUE)

## Notas
notas  <- read.csv("logs/notas.csv")


####################################################################################################
## ARTIGO 1 -----------------------------------------------------------------------------------------
## Descrição de um curso online para prevenção do uso de drogas para educadores de Escolas Públicas.
####################################################################################################

# Abrir banco de dados
socioDemo  <- read.csv("preditores/data/sociodemografico.csv", na.strings="-")

# Criar banco somente com pessoas que consentiram participar
socioDemo  <- subset(socioDemo, socioDemo$termo == "Sim")

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
porcentagem(ativCol$material)

## Prazo Atividades
porcentagem(ativCol$flexprazo)

## Interação entre pares
porcentagem(ativCol$interacaocol)

## Organização curso
porcentagem(ativCol$organizado)

## O quão importante é o auxílio de seu tutor
porcentagem(ativCol$freqauxitutor)

## Participação em outro curso
porcentagem(ativCol$partoutrocurso)

########################################
# NOTAS                             ----
########################################

## Ler banco com notas
notas  <- read.csv("preditores/data/notas.csv", dec = ".", na.strings=c("NA","-"))

names(notas)
# Listar variáveis com notas
summary(notas[,2:29])

# ===============================
# Fóruns ------
# ===============================

# Selecionar somente os foruns
notasForum  <- notas[, c("forum1", "forum2", "forum3","forum4","forum5","forum6","forum7","forum8","forum9","forum10","forum11","forum12","forum13","forum14","forum15")]

# Notas medias dos foruns
round(sapply(notasForum, mean, na.rm = TRUE),2)

# Desvio padrão
round(sapply(notasForum, sd, na.rm = TRUE),2)

# Preparacao dos dados para realizacao do grafico
## Avaliar quem fez a atividade
forum  <- lapply(notasForum, is.na)

## Tabular os resultados
forumTeste <- lapply(forum, table)

## Criar dataframe
dfForum <- ldply(forumTeste)

## Nomes para as variáeveis do banco
colnames(dfForum)  <- c("forum", "atividade", "falta")

## Atribuir bons labels aos fóruns
dfForum[1,1] <- "01"; dfForum[2,1] <- "02"; dfForum[3,1] <- "03"; dfForum[4,1] <- "04"; dfForum[5,1] <- "05"; dfForum[6,1] <- "06"; dfForum[7,1] <- "07";dfForum[8,1] <- "08";dfForum[9,1] <- "09";dfForum[10,1] <- "10";dfForum[11,1] <- "11";dfForum[12,1] <- "12";dfForum[13,1] <- "13";dfForum[14,1] <- "14";dfForum[15,1] <- "15";
dfForum$forum  <- as.factor(dfForum$forum)

# Gráfico de participação nos fóruns - Bar plot - ggplot2
graphForuns <- ggplot(data = dfForum, aes(x = forum, y = atividade)) + geom_bar(stat="identity", fill = "#CCCCCC", colour="#666666") + xlab("Fórum") + ylab("Participação") + coord_cartesian(ylim=c(0,4500)) + theme_bw() + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(vjust=0.5, size=8), axis.title.y = element_text(face="bold", size=12), axis.text.y  = element_text(vjust=0.5, size=12))

# ===============================
# Atividades Colaborativas ------
# ===============================

## Selecionar somente os foruns - Faltam questões ainda: Recuperação dos módulos 3 e 4.
## Dúvida - O que são as recuperações gerais?
notasAticol <- notas[, c("ativcolm1", "ativcolm2","ativcolm1r", "ativcolm2r", "ativcolm3", "ativcolm4")]

## Substituir nota maior de quem fez recuperação

### Atividade 1
notasAticol$ativcolm1 <- ifelse(is.na(notasAticol$ativcolm1r), notasAticol$ativcolm1, notasAticol$ativcolm1r)
### Atividade 2
notasAticol$ativcolm2 <- ifelse(is.na(notasAticol$ativcolm2r), notasAticol$ativcolm2, notasAticol$ativcolm2r)

### Implementar para atividades 3 e 4

### Gráfico de boxplots com as notas
notasAticol <- notasAticol[, c("ativcolm1", "ativcolm2", "ativcolm3", "ativcolm4")]

# Preparacao dos dados para realizacao do grafico
## Avaliar quem fez a atividade
aticol  <- lapply(notasAticol, is.na)

## Tabular os resultados
aticolTeste <- lapply(aticol, table)

## Criar dataframe
dfAticol <- ldply(aticolTeste)

## Nomes para as variáeveis do banco
colnames(dfAticol)  <- c("Atividade", "Participou", "Nparticipou")

## Atribuir bons labels às atividades colaborativas
dfAticol[1,1] <- "01"; dfAticol[2,1] <- "02"; dfAticol[3,1] <- "03"; dfAticol[4,1] <- "04"

# Gráfico de participação nos atividades - Bar plot - ggplot2
graphAtiv <- ggplot(data = dfAticol, aes(x = Atividade, y = Participou)) + geom_bar(stat="identity", fill = "#CCCCCC", colour="#666666") + xlab("Atividade Colaborativa") + ylab("") + coord_cartesian(ylim=c(0,4500)) + theme_bw() + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(vjust=0.5, size=12), axis.title.y = element_text(face="bold", size=12), axis.text.y  = element_text(vjust=0.5, size=12))

# ===============================
# Questionários            ------
# ===============================

## Selecionar variáveis dos questionários
quest <- notas[, c("quesm1","quesm2","quesm3")]

## Criar porcentagens
# round(sapply(quest, mean, na.rm=TRUE),2)

## Tabular is.na
questTeste  <- lapply(quest, is.na)

## Tabular notas
questTeste <- lapply(questTeste, table)
 
## Criar dataframe
dfQuest <- ldply(questTeste)

## Nomes para as variáeveis do banco
colnames(dfQuest)  <- c("Questionario", "Participou", "Nparticipou")

## Atribuir bons labels às atividades colaborativas
dfQuest[1,1] <- "01"; dfQuest[2,1] <- "02"; dfQuest[3,1] <- "03"

# Gráfico de participação nos atividades - Bar plot - ggplot2
graphQuest <- ggplot(data = dfQuest, aes(x = Questionario, y = Participou)) + geom_bar(stat="identity", fill = "#CCCCCC", colour="#666666") + xlab("Questionário") + ylab("") + coord_cartesian(ylim=c(0,4500)) + theme_bw() + theme(axis.title.x = element_text(face="bold", size=12), axis.text.x  = element_text(vjust=0.5, size=12), axis.title.y = element_text(face="bold", size=12), axis.text.y  = element_text(vjust=0.5, size=12))

# ===============================
# Gráfico final            ------
# ===============================

multiplot(graphForuns, graphAtiv, graphQuest, cols=3)

