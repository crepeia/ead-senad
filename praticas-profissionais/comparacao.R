library(car)      # Load packge - used function Recode
library(psych)    # Load packge - used for EFA
library(mirt)     # CFA
library(xtable)   # Load xtable package
library(ltm)

#read data
praticasPro  <- read.csv("praticas-profissionais/praticasprofissionais_df.csv")

## Recode Social Perception Scale 
for (i in 32:68){
  praticasPro[,i]   <-  recode(praticasPro[,i], "'Concordo'=4 ; c('Concordo totalmente', 'Concordo Totalmente')=5 ; 'Discordo' = 2; c('Discordo totalmente','Discordo Totalmente') = 1;  'Nem discordo, nem concordo' = 3")                     

}
## Create variable - sum scales to remove NA's

praticasPro$scaleSum  <- rowSums(praticasPro[,32:68])

praticasPro <- subset(praticasPro, subset=praticasPro$termo=="Sim")

praticasPro$idade  <- as.numeric(as.character(praticasPro$idade))
praticasPro$idade[praticasPro$idade < 18 | praticasPro$idade > 68 ]  <- NA
summary(praticasPro$idade) # all
by(praticasPro$idade, praticasPro$sexo, describe) # by sex


# Create dataFrame with attitudes scale
fullScale  <- praticasPro[,32:68]
# Select items without NA's.
fullScale  <- fullScale[complete.cases(fullScale),]





#Analise Fatorial sem rotação - Escala Completa
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "none")
print.psych(faAll, digits=2, cut=0.4)

polyAll <- polychoric(fullScale)

pcAll <- principal(polyAll$rho, nfactors=2,rotate = "none")
print.psych(pcAll, digits=2, cut=0.4)





#Analise Fatorial com rotação-Escala Completa
faAll_rot <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin")
print.psych(faAll_rot, digits=2, cut=0.4)

polyAll <- polychoric(fullScale)

pcAll_rot <- principal(polyAll$rho, nfactors=2,rotate = "oblimin")
print.psych(pcAll_rot, digits=2, cut=0.4)



#______--------------------------------------------------------------------

##Analise Fatorial sem rotação-Escala reduzida

faAll <- fa.poly(fullScale, nfactors = 2, rotate = "none")
print.psych(faAll, digits=2, cut=0.4)

polyAll <- polychoric(fullScale)

pcAll <- principal(polyAll$rho, nfactors=2,rotate = "none")
print.psych(pcAll , digits=2, cut=0.4)

# Results - Items 1,8,11,15,18,need to be removed due to small loadings < (.4).

shortScale1  <- fullScale[, -c(1,8,11,15,18)]
faShortScale1 <- fa.poly(shortScale1, nfactors = 2, rotate = "none")
print.psych(faShortScale1, digits=2, cut=0.4)

polyAll1 <- polychoric(shortScale1)

pcAll1 <- principal(polyAll1$rho, nfactors=2,rotate = "none")
print.psych(pcAll1 , digits=2, cut=0.4)

# Results - Items 9,25,need to be removed due to small loadings < (.4).

shortScaleF <- shortScale1[,-c(7,20)]
faShortScaleF <-fa.poly(shortScaleF, nfactors = 2, rotate = "none")
print.psych(faShortScaleF, digits=2, cut=0.4)

polyAllF <- polychoric(shortScaleF)

pcAllf <- principal(polyAll1$rho, nfactors=2,rotate = "none")
print.psych(pcAllf , digits=2, cut=0.4)


##Analise Fatorial com rotação-Escala reduziada

faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin")
print.psych(faAll, digits=2, cut=0.4)
# Results - Items 11,25,need to be removed due to small loadings < (.4).

polyAll <- polychoric(fullScale)

pcAll <- principal(polyAll$rho, nfactors=2,rotate = "oblimin")
print.psych(pcAll , digits=2, cut=0.4)


shortScale1  <- fullScale[, -c(11,25)]
faShortScale1 <- fa.poly(shortScale1, nfactors = 2, rotate = "oblimin")
print.psych(faShortScale1, digits=2, cut=0.4)
# Results - Items 11,25,need to be removed due to small loadings < (.4).


shortScale2 <- shortScale1[,-c(17)]
faShortScale2 <-fa.poly(shortScale2, nfactors = 2, rotate = "oblimin")
print.psych(faShortScaleF, digits=2, cut=0.4)
# Results - Items 1,need to be removed due to small loadings < (.4).

shortScaleF <- shortScale2[,-c(1)]
faShortScaleF <-fa.poly(shortScaleF, nfactors = 2, rotate = "oblimin")
print.psych(faShortScaleF, digits=2, cut=0.4)

.
