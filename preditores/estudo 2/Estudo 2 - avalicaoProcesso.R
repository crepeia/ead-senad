########################
# A FAZER
########################

####################################################################################################
## ARTIGO 2 ----------------------------------------------------------------------------------------
## Atividades colaborativas.
####################################################################################################

## Abrir banco de atividades colaborativas

### PRE
ativColPre  <- read.csv("preditores/data/ativColPre.csv", dec = ",", na.strings="-", stringsAsFactor = FALSE)

### POS
ativColPos  <- read.csv("preditores/data/ativColPos.csv", dec = ",", na.strings="-", stringsAsFactor = FALSE)

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


# Tabelas de Frequência

propTable  <- function(x){
  cbind(round(prop.table(sort(table(x), decreasing = TRUE)),3))*100
}

# Ausência da família
propTable(ativColPre$BausenFam)

# Comunicação com os pais
propTable(ativColPre$BcomPais)

# Uso de drogas dos pais
propTable(ativColPre$BdrogasPais)

# Presença de drogas no ambiente escolar
propTable(ativColPre$BdrogasEsc)

# Tráfico de drogas
propTable(ativColPre$Btrafico)

# Ausência de limites dos alunos
propTable(ativColPre$BlimAlunos)

# Ausência de colaboração da equipe
propTable(ativColPre$BausenciaColEs)

# Ausência do aluno na escola
propTable(ativColPre$BausenciaAluEs)

# Ausência de regras dos alunos
propTable(ativColPre$BregrasAluEs)

# Facilitadores

# Alunos interessados na temática
propTable(ativColPre$FinteresseAlun)

# Presença de uma equipe para trabalhar a temática
propTable(ativColPre$FpresencaEquipe)

# Estimulos aos alunos
propTable(ativColPre$FestiAlun)

# Relação saudável aluno família escola comunidade
propTable(ativColPre$FrelAEC)

# Desenvolvimento de projetos
propTable(ativColPre$FprojEscol)

# Apoio aos projetos
propTable(ativColPre$FapoioProj)

# Presença de regras
propTable(ativColPre$FregrasEscol)

# Respeito na relação professor aluno
propTable(ativColPre$FrespeitoAP)

# Promoção de compromissos e confiança
propTable(ativColPre$FpromCompConf)

# Valorização do ambiente escolar
propTable(ativColPre$FvalAmbEsc)

# Participação da comunidade e dos pais
propTable(ativColPre$FpartPaiCom)


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


# Tabelas de Frequência

propTable  <- function(x){
  cbind(round(prop.table(sort(table(x), decreasing = TRUE)),3))*100
}

# Ausência da família
propTable(ativColPos$BausenFam)

# Comunicação com os pais
propTable(ativColPos$BcomPais)

# Uso de drogas dos pais
propTable(ativColPos$BdrogasPais)

# Presença de drogas no ambiente escolar
propTable(ativColPos$BdrogasEsc)

# Tráfico de drogas
propTable(ativColPos$Btrafico)

# Ausência de limites dos alunos
propTable(ativColPos$BlimAlunos)

# Ausência de colaboração da equipe
propTable(ativColPos$BausenciaColEs)

# Ausência do aluno na escola
propTable(ativColPos$BausenciaAluEs)

# Ausência de regras dos alunos
propTable(ativColPos$BregrasAluEs)

# Facilitadores

# Alunos interessados na temática
propTable(ativColPos$FinteresseAlun)

# Presença de uma equipe para trabalhar a temática
propTable(ativColPos$FpresencaEquipe)

# Estimulos aos alunos
propTable(ativColPos$FestiAlun)

# Relação saudável aluno família escola comunidade
propTable(ativColPos$FrelAEC)

# Desenvolvimento de projetos
propTable(ativColPos$FprojEscol)

# Apoio aos projetos
propTable(ativColPos$FapoioProj)

# Presença de regras
propTable(ativColPos$FregrasEscol)

# Respeito na relação professor aluno
propTable(ativColPos$FrespeitoAP)

# Promoção de compromissos e confiança
propTable(ativColPos$FpromCompConf)

# Valorização do ambiente escolar
propTable(ativColPos$FvalAmbEsc)

# Participação da comunidade e dos pais
propTable(ativColPos$FpartPaiCom)
