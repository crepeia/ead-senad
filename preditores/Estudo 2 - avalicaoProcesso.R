########################
# A FAZER
########################




####################################################################################################
## ARTIGO 2 ----------------------------------------------------------------------------------------
## Atividades colaborativas.
####################################################################################################

## Abrir banco de atividades colaborativas
ativCol  <- read.csv("atividadesColaborativas-respostas.csv", dec = ",", na.strings="-", stringsAsFactor = FALSE)

#####################
# PRE ---------------
#####################

# Dificultadores

ativCol$BausenFam  <- ifelse(grepl("Ausência da família", ativCol$barreiras),"Sim","Não")
ativCol$BcomPais  <- ifelse(grepl("Pouca comunicação com os pais", ativCol$barreiras),"Sim","Não")
ativCol$BdrogasPais  <- ifelse(grepl("Uso de substâncias por familiares", ativCol$barreiras),"Sim","Não")
ativCol$BdrogasEsc  <- ifelse(grepl("Presença de drogas ilícitas no ambiente escolar", ativCol$barreiras),"Sim","Não")
ativCol$Btrafico  <- ifelse(grepl("Proximidade da rede de distribuição de drogas", ativCol$barreiras),"Sim","Não")
ativCol$BlimAlunos  <- ifelse(grepl("Ausência de limites dos alunos", ativCol$barreiras),"Sim","Não")
ativCol$BausenciaColEs  <- ifelse(grepl("Ausência de colaboração da equipe escolar", ativCol$barreiras),"Sim","Não")
ativCol$BausenciaAluEs  <- ifelse(grepl("Ausência dos alunos na escola", ativCol$barreiras),"Sim","Não")
ativCol$BregrasAluEs  <- ifelse(grepl("Ausência de regras no ambiente escolar", ativCol$barreiras),"Sim","Não")

# Facilitadores

ativCol$FprojEscol  <- ifelse(grepl("Desenvolvimento de projetos na escola", ativCol$facilitadores),"Sim","Não")
ativCol$FpresencaEquipe  <- ifelse(grepl("Presença de uma equipe para trabalhar a temática", ativCol$facilitadores),"Sim","Não")
ativCol$FregrasEscol  <- ifelse(grepl("Presença de regras no ambiente escolar", ativCol$facilitadores),"Sim","Não")
ativCol$FrespeitoAP  <- ifelse(grepl("Respeito na relação professor-aluno", ativCol$facilitadores),"Sim","Não")
ativCol$FestiAlun  <- ifelse(grepl("Estímulo aos alunos", ativCol$facilitadores),"Sim","Não")
ativCol$FpromCompConf  <- ifelse(grepl("Promoção de compromisso e confiança", ativCol$facilitadores),"Sim","Não")
ativCol$FvalAmbEsc  <- ifelse(grepl("Valorização do ambiente escolar", ativCol$facilitadores),"Sim","Não")
ativCol$FpartPaiCom  <- ifelse(grepl("Participação da comunidade e dos pais no trabalho de prevenção", ativCol$facilitadores),"Sim","Não")
ativCol$FapoioProj  <- ifelse(grepl("Apoio aos projetos em desenvolvimento", ativCol$facilitadores),"Sim","Não")
ativCol$FinteresseAlun  <- ifelse(grepl("Possuir alunos interessados na temática", ativCol$facilitadores),"Sim","Não")
ativCol$FrelAEC  <- ifelse(grepl("Relação saudável entre aluno-família-escola-comunidade", ativCol$facilitadores),"Sim","Não")


# Tabelas de Frequência

propTable  <- function(x){
  cbind(round(prop.table(sort(table(x), decreasing = TRUE)),3))*100
}

# Ausência da família
propTable(ativCol$BausenFam)

# Comunicação com os pais
propTable(ativCol$BcomPais)

# Uso de drogas dos pais
propTable(ativCol$BdrogasPais)

# Presença de drogas no ambiente escolar
propTable(ativCol$BdrogasEsc)

# Tráfico de drogas
propTable(ativCol$Btrafico)

# Ausência de limites dos alunos
propTable(ativCol$BlimAlunos)

# Ausência de colaboração da equipe
propTable(ativCol$BausenciaColEs)

# Ausência do aluno na escola
propTable(ativCol$BausenciaAluEs)

# Ausência de regras dos alunos
propTable(ativCol$BregrasAluEs)

# Facilitadores

# Alunos interessados na temática
propTable(ativCol$FinteresseAlun)

# Presença de uma equipe para trabalhar a temática
propTable(ativCol$FpresencaEquipe)

# Estimulos aos alunos
propTable(ativCol$FestiAlun)

# Relação saudável aluno família escola comunidade
propTable(ativCol$FrelAEC)

# Desenvolvimento de projetos
propTable(ativCol$FprojEscol)

# Apoio aos projetos
propTable(ativCol$FapoioProj)

# Presença de regras
propTable(ativCol$FregrasEscol)

# Respeito na relação professor aluno
propTable(ativCol$FrespeitoAP)

# Promoção de compromissos e confiança
propTable(ativCol$FpromCompConf)

# Valorização do ambiente escolar
propTable(ativCol$FvalAmbEsc)

# Participação da comunidade e dos pais
propTable(ativCol$FpartPaiCom)


#####################
# POS ---------------
#####################

# Falta implementar
# 1. Análise pós.
