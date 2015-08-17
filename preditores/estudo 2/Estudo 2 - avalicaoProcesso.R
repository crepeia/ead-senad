####################################################################################################
## ARTIGO 2 ----------------------------------------------------------------------------------------
## Atividades colaborativas.
####################################################################################################

# Definir diretório com os bancos de dados.
setwd("logs/")

# Carregar pacotes
library(sjPlot)


## Abrir banco de atividades colaborativas
### PRE
ativColPre  <- read.csv("atividadesColaborativasPre.csv", dec = ",", na.strings="-", stringsAsFactor = FALSE)

### POS
ativColPos  <- read.csv("atividadesColaborativasPos.csv", dec = ",", na.strings="-", stringsAsFactor = FALSE)

names(ativColPos)

#####################
# PRE ---------------
#####################

# Dificultadores

ativColPre$BausenFam  <- ifelse(grepl("Ausência da família", ativColPre$barreiras),"Sim","Não")
ativColPre$BcomPais  <- ifelse(grepl("Pouca comunicação com os pais", ativColPre$barreiras),"Sim","Não")
ativColPre$BdrogasPais  <- ifelse(grepl("Uso de substâncias por familiares", ativColPre$barreiras),"Sim","Não")
ativColPre$BdrogasEsc  <- ifelse(grepl("Presença de drogas ilícitas no ambiente escolar", ativColPre$barreiras),"Sim","Não")
ativColPre$Btrafico  <- ifelse(grepl("Proximidade da rede de distribuição de drogas", ativColPre$barreiras),"Sim","Não")
ativColPre$BlimAlunos  <- ifelse(grepl("Ausência de limites dos alunos", ativColPre$barreiras),"Sim","Não")
ativColPre$BausenciaColEs  <- ifelse(grepl("Ausência de colaboração da equipe escolar", ativColPre$barreiras),"Sim","Não")
ativColPre$BausenciaAluEs  <- ifelse(grepl("Ausência dos alunos na escola", ativColPre$barreiras),"Sim","Não")
ativColPre$BregrasAluEs  <- ifelse(grepl("Ausência de regras no ambiente escolar", ativColPre$barreiras),"Sim","Não")

# Facilitadores

ativColPre$FprojEscol  <- ifelse(grepl("Desenvolvimento de projetos na escola", ativColPre$facilitadores),"Sim","Não")
ativColPre$FpresencaEquipe  <- ifelse(grepl("Presença de uma equipe para trabalhar a temática", ativColPre$facilitadores),"Sim","Não")
ativColPre$FregrasEscol  <- ifelse(grepl("Presença de regras no ambiente escolar", ativColPre$facilitadores),"Sim","Não")
ativColPre$FrespeitoAP  <- ifelse(grepl("Respeito na relação professor-aluno", ativColPre$facilitadores),"Sim","Não")
ativColPre$FestiAlun  <- ifelse(grepl("Estímulo aos alunos", ativColPre$facilitadores),"Sim","Não")
ativColPre$FpromCompConf  <- ifelse(grepl("Promoção de compromisso e confiança", ativColPre$facilitadores),"Sim","Não")
ativColPre$FvalAmbEsc  <- ifelse(grepl("Valorização do ambiente escolar", ativColPre$facilitadores),"Sim","Não")
ativColPre$FpartPaiCom  <- ifelse(grepl("Participação da comunidade e dos pais no trabalho de prevenção", ativColPre$facilitadores),"Sim","Não")
ativColPre$FapoioProj  <- ifelse(grepl("Apoio aos projetos em desenvolvimento", ativColPre$facilitadores),"Sim","Não")
ativColPre$FinteresseAlun  <- ifelse(grepl("Possuir alunos interessados na temática", ativColPre$facilitadores),"Sim","Não")
ativColPre$FrelAEC  <- ifelse(grepl("Relação saudável entre aluno-família-escola-comunidade", ativColPre$facilitadores),"Sim","Não")


#####################
# POS ---------------
#####################

# Dificultadores

ativColPos$BausenFam  <- ifelse(grepl("Ausência da família", ativColPos$barreiras),"Sim","Não")
ativColPos$BcomPais  <- ifelse(grepl("Pouca comunicação com os pais", ativColPos$barreiras),"Sim","Não")
ativColPos$BdrogasPais  <- ifelse(grepl("Uso de substâncias por familiares", ativColPos$barreiras),"Sim","Não")
ativColPos$BdrogasEsc  <- ifelse(grepl("Presença de drogas ilícitas no ambiente escolar", ativColPos$barreiras),"Sim","Não")
ativColPos$Btrafico  <- ifelse(grepl("Proximidade da rede de distribuição de drogas", ativColPos$barreiras),"Sim","Não")
ativColPos$BlimAlunos  <- ifelse(grepl("Ausência de limites dos alunos", ativColPos$barreiras),"Sim","Não")
ativColPos$BausenciaColEs  <- ifelse(grepl("Ausência de colaboração da equipe escolar", ativColPos$barreiras),"Sim","Não")
ativColPos$BausenciaAluEs  <- ifelse(grepl("Ausência dos alunos na escola", ativColPos$barreiras),"Sim","Não")
ativColPos$BregrasAluEs  <- ifelse(grepl("Ausência de regras no ambiente escolar", ativColPos$barreiras),"Sim","Não")

# Facilitadores

ativColPos$FprojEscol  <- ifelse(grepl("Desenvolvimento de projetos na escola", ativColPos$facilitadores),"Sim","Não")
ativColPos$FpresencaEquipe  <- ifelse(grepl("Presença de uma equipe para trabalhar a temática", ativColPos$facilitadores),"Sim","Não")
ativColPos$FregrasEscol  <- ifelse(grepl("Presença de regras no ambiente escolar", ativColPos$facilitadores),"Sim","Não")
ativColPos$FrespeitoAP  <- ifelse(grepl("Respeito na relação professor-aluno", ativColPos$facilitadores),"Sim","Não")
ativColPos$FestiAlun  <- ifelse(grepl("Estímulo aos alunos", ativColPos$facilitadores),"Sim","Não")
ativColPos$FpromCompConf  <- ifelse(grepl("Promoção de compromisso e confiança", ativColPos$facilitadores),"Sim","Não")
ativColPos$FvalAmbEsc  <- ifelse(grepl("Valorização do ambiente escolar", ativColPos$facilitadores),"Sim","Não")
ativColPos$FpartPaiCom  <- ifelse(grepl("Participação da comunidade e dos pais no trabalho de prevenção", ativColPos$facilitadores),"Sim","Não")
ativColPos$FapoioProj  <- ifelse(grepl("Apoio aos projetos em desenvolvimento", ativColPos$facilitadores),"Sim","Não")
ativColPos$FinteresseAlun  <- ifelse(grepl("Possuir alunos interessados na temática", ativColPos$facilitadores),"Sim","Não")
ativColPos$FrelAEC  <- ifelse(grepl("Relação saudável entre aluno-família-escola-comunidade", ativColPos$facilitadores),"Sim","Não")


# Join Data frames ----
ativColPre$time <- "pre"
ativColPos$time <- "pos"

df <- rbind(ativColPos[,11:31], ativColPre[,24:44])

for (i in 1:21) {
  df[, i] <- as.factor(df[, i])
}

sjp.grpfrq(df[,1], df[,21], coord.flip = TRUE, showCountValues = FALSE, weightBy = )

########################
# Data Analysis
########################

# Gráficos
labels <- c(
  "BAR - Ausência da família",
  "BAR - Comunicação com os pais",
  "BAR - Ausência da família",
  "BAR - Uso de drogas dos pais",
  "BAR - Presença de drogas no ambiente escolar",
  "BAR - Tráfico de drogas",
  "BAR - Ausência de limites dos alunos",
  "BAR - Ausência de colaboração da equipe",
  "BAR - Ausência do aluno na escola",
  "BAR - Ausência de regras dos alunos",
  "FAC - Alunos interessados na temática",
  "FAC - Presença de uma equipe para trabalhar a temática",
  "FAC - Estimulos aos alunos",
  "FAC - Relação saudável aluno família escola comunidade",
  "FAC - Desenvolvimento de projetos",
  "FAC - Apoio aos projetos",
  "FAC - Presença de regras",
  "FAC - Respeito na relação professor aluno",
  "FAC - Promoção de compromissos e confiança",
  "FAC - Valorização do ambiente escolar",
  "FAC - Participação da comunidade e dos pais"
)

par(mfcol = c(2,2))
for (i in 1:length(labels)){
  barplot(prop.table(table(df[,i], df$time),2), horiz = TRUE, main = labels[i], names.arg = c("Pós","Pré"))
}
  
# Tabelas de Frequência

propTable  <- function(x,y){
  printA <- round(prop.table(table(x, y),2),3)*100
  printB <- chisq.test(table(x,y))
  print(printA)
  cat("\n Resultados do Qui-quadrado: \n X² = ",printB$statistic, "df = ",printB$parameter, "p = ",printB$p.value)
}

# Ausência da família
propTable(df$BausenFam, df$time)

# Comunicação com os pais
propTable(df$BcomPais, df$time)

# Uso de drogas dos pais
propTable(df$BdrogasPais, df$time)

# Presença de drogas no ambiente escolar
propTable(df$BdrogasEsc, df$time)

# Tráfico de drogas
propTable(df$Btrafico, df$time)

# Ausência de limites dos alunos
propTable(df$BlimAlunos, df$time)

# Ausência de colaboração da equipe
propTable(df$BausenciaColEs, df$time)

# Ausência do aluno na escola
propTable(df$BausenciaAluEs, df$time)

# Ausência de regras dos alunos
propTable(df$BregrasAluEs, df$time)

################
# Facilitadores
##################

# Alunos interessados na temática
propTable(df$FinteresseAlun, df$time)

# Presença de uma equipe para trabalhar a temática
propTable(df$FpresencaEquipe, df$time)

# Estimulos aos alunos
propTable(df$FestiAlun, df$time)

# Relação saudável aluno família escola comunidade
propTable(df$FrelAEC, df$time)

# Desenvolvimento de projetos
propTable(df$FprojEscol, df$time)

# Apoio aos projetos
propTable(df$FapoioProj, df$time)

# Presença de regras nas escolas
propTable(df$FregrasEscol, df$time)

# Respeito na relação professor aluno
propTable(df$FrespeitoAP, df$time)

# Promoção de compromissos e confiança
propTable(df$FpromCompConf, df$time)

# Valorização do ambiente escolar
propTable(df$FvalAmbEsc, df$time)

# Participação da comunidade e dos pais
propTable(df$FpartPaiCom, df$time)

# GGPLOT2 Graphs

## Gráfico de barreiras
barreiras <- read.csv("../estudo 2/barreiras.csv", stringsAsFactors = FALSE)
barreiras <- melt(barreiras, id.vars = "Items", measure.vars = c("Pre","Pos"))
barreiras$variable <- factor(barreiras$variable, levels = c("Pos","Pre"))
names(barreiras) <- c("Itens", "Tempo", "value")
ggplot(barreiras, aes(x = reorder(Itens,value), y = value, fill = Tempo)) + geom_bar(stat="identity", position="dodge") + coord_flip() + theme_minimal(base_size = 16, base_family = "Times New Roman") + xlab("") + ylab("")

## Gráfico dos facilitadores
facilitadores <- read.csv("../estudo 2/facilitadores.csv", stringsAsFactors = FALSE)
facilitadores <- melt(facilitadores, id.vars = "Items", measure.vars = c("Pre","Pos"))
facilitadores$variable <- factor(facilitadores$variable, levels = c("Pos","Pre"))
names(facilitadores) <- c("Itens", "Tempo", "value")
ggplot(facilitadores, aes(x = reorder(Itens,value), y = value, fill = Tempo)) + geom_bar(stat="identity", position="dodge") + coord_flip() + theme_minimal(base_size = 16, base_family = "Times New Roman") + xlab("") + ylab("")


