###########################
# 2nd PAPER     -----------
###########################

###########################
# OPEN DATAFRAME
###########################

# Load Libraries
library(car)          # Load packge - used function Recode
library(psych)        # Load packge - used for EFA
library(mirt)         # CFA, GRM IRT Analysis
library(xtable)       # Load xtable package
library(ltm)          # LTM analysis - DEPRECATED. This package produces instable GRM models. 
library(RColorBrewer) # Better color palletes

# Select color pallette
myColors <- brewer.pal(5,"RdYlGn")
myColors[3] <- "yellow"

# Set seed
set.seed(123)

# Set trellis parameters
my.settings <- list(
  strip.background=list(col="#eeeeee"),
  strip.border=list(col="white"),
  superpose.line = list(col=myColors[1:5]),
  superpose.symbol = list(col=myColors[1:5]),
  axis.line = list(col = "#eeeeee")
)

# Open Data Frame
attitudesE  <- read.csv("atitudesEducadores_df.csv")

# Convert items into numeric
 for (i in c(33:71)){
   attitudesE[,i]   <-  Recode(attitudesE[,i], "'Discordo totalmente'=1 ; 'Discordo'=2 ; 'Nem discordo, nem concordo' = 3; 'Concordo' = 4; 'Concordo totalmente' = 5; else = NA")
   attitudesE[,i]   <-  as.numeric(attitudesE[,i])  
}

## Sum items to remove NA's
attitudesE$scaleSum  <- rowSums(attitudesE[,33:71])
  
# Subset with those who consented
attitudesE <- subset(attitudesE, attitudesE$termo == "Sim" & !is.na(attitudesE$scaleSum))
  
# Select Scale items
aScale  <-  attitudesE[,33:71]
  
## Reverse code items according to Theory
#   for (i in c(14,19,28,29,32,33,34,35)) {
#     aScale[,i] <- recode(aScale[,i], "1=5;2=4;4=2;5=1")
#   }

###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
polyAll <- polychoric(aScale)
# Unrotated PCA
faAll <- principal(polyAll$rho, nfactors = 2)
print.psych(faAll, digits=2, cut= .4)
# Rotated PCA
faAll_rot <- principal(polyAll$rho, nfactors = 2, rotate="oblimin")
print.psych(faAll_rot, digits=2, cut= .4)
# Results - Items 9,11,13,18,21,25,31,39 need to be removed due to small loadings < (.4).

## ANA, Como fazer o alfa de Cronbach, KMO, Bartlett
## Tire o '#' das linhas abaixo para executar
# KMO(aScale) # KMO
# bartlett.test(aScale) # Bartlett's test
# alpha(aScale, check.keys = TRUE) # Alfa da primeira versão da escala.

# SECOND EFA -------
# Remove items with low loadings detected in previous step.
shortScale  <- aScale[, -c(9,11,13,18,21,25,31, 39)]
polyShort <- polychoric(shortScale)
# Unrotated PCA
faShortScale <- principal(polyShort$rho, nfactors = 2)
print.psych(faShortScale, digits=2, cut= .4)
# Rotated PCA
faShortScale_rot <- principal(polyShort$rho, nfactors = 2, rotate="oblimin")
print.psych(faShortScale_rot, digits=2, cut= .4)


###########################
# CFA - BFACTOR
###########################

item_pos <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
efaVersion  <-   c(1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2,1,2,2)
theoryVersion <- c(1,1,2,1,1,1,1,1,1,2,1,1,2,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2)
dimShort <-cbind(item_pos, efaVersion, theoryVersion)

# EFA result
cfaEFA <- bfactor(shortScale, efaVersion)
summary(cfaEFA)
coef(cfaEFA)

# Theory result
cfaTheory <- bfactor(shortScale, theoryVersion)
summary(cfaTheory)
coef(cfaTheory)
# Results - We have a few items that have low factor loadings although they have good G loadings. I cannot judge whether we could proceed the IRT analysis using this factor model.

###########################
# IRT
###########################

# Create Short Scale - Version 1
aScale1 <- shortScale[, c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]
itemNames <- names(aScale1)
itemNames[1]

###########################
# With LTM package
###########################

# Grade Response Model
triShort1 <- grm(aScale1, constrained = FALSE)

# Estimate Coeficients
coef(triShort1)

# Para Ana - 
# Para gerar o gráfico use o script abaixo
plot(triShort1, type = "ICC", item = 1, main = "Item 8 + itemNames[1]" , ylab = "Rótulo do eixo Y", xlab = "Rótulo do eixo X")

# Plot graphs
pdf("mygraph.pdf")
par(mfrow = c(4,4))
for (i in 1:16) {  
plot(triShort1, type = "ICC", items = i,  main = paste("Item - ", itemNames[i]), ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}
dev.off()

# Create Short Scale - Version 2
aScale2 <- shortScale[, -c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]

# # Grade Response Model
triShort2 <- grm(aScale2)

# Estimate Coeficients
coef(triShort2)

# Plot graphs
pdf("mygraph_2.pdf")
par(mfrow = c(4,4))
for (i in 1:15) {  
  plot(triShort2, type = "ICC", items = i,  main = paste("Item - ", itemNames[i]), ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}
dev.off()

########################################
## Como gerar os scores baseado no LTM
#########################################

# Estimar escores
escoresFatorA <- factor.scores(triShort1)
escoresFatorB <- factor.scores(triShort2)

# Gerar histogramas por 4  fator
hist(escoresFatorA$score.dat$z1,
     xlab="Theta",
     main="Titulo do grafico")

hist(escoresFatorB$score.dat$z1,
     xlab="Theta", 
     main="Titulo do grafico")

###########################
# IRT - MIRT
###########################

## NOTE - There has been some concern on the function GRM of ltm package. You can see a forum discussion here: http://stats.stackexchange.com/questions/63891/is-r-output-reliable-specially-irt-package-ltm. In order to avoid this concern, I decided to use the grm estimation of mirt package, which is compared to other softwares.

# Create Scale from factor 1 - Version 1
factorA <- aScale1
factorB <- aScale2

modA <- mirt(factorA, itemtype = "graded", model = 1, message = FALSE)
modB <- mirt(factorB, itemtype = "graded", model = 1, message = FALSE)

# Summary
summary(modA)
summary(modB)

# Compute the M2 model fit statistic from Maydey-Olivares and Joe (2006)
M2(modA); M2(modB)

##########################################
# PLOTS
##########################################

coef(modA, IRTpars = TRUE)
coef(modB, IRTpars = TRUE)

# Plot - Show test information
itemplot(modA, type = "trace", item = 1)
plot(modA, type = "info")
plot(modB, type = "info")

# Plot - Realibility
plot(modA, type = "rxx")
plot(modB, type = "rxx")

# Plot - Information trace
plot(modA, type = "trace", par.settings = my.settings, main = "", auto.key=list(space="top", columns=5, title="", cex.title=1, cex.text = .3, text=c("Disc. Total.","Disc.","Nem Con., nem disc.","Conc.","Conc. Total.")))

plot(modB, type = "trace", par.settings = my.settings, main = "", auto.key=list(space="top", columns=5, title="", cex.title=1, cex.text = .3, text=c("Disc. Total.","Disc.","Nem Con., nem disc.","Conc.","Conc. Total.")))

# Plot - Info trace
plot(modA, type = "infotrace", par.settings = my.settings, main = "")
plot(modB, type = "infotrace", par.settings = my.settings, main = "")
# Conclusion - Items to be included in the next iteration: 
#              modC - pp002, pp003, pp004, pp006, pp017
#              modP - pp010, pp012, pp020, pp021, pp029,pp031,pp035,pp036

itemfit(modA)
itemfit(modB)

residuals(modA)
residuals(modB)

marginal_rxx(modA, theta_lim = c(-3, 3))
marginal_rxx(modB, theta_lim = c(-3, 3))

scoresFactorA  <- fscores(modA, full.scores = TRUE)
scoresFactorB <- fscores(modB, full.scores = TRUE)

dfFactorA <- data.frame(scoresFactorA)
dfFactorA$F1 <- as.numeric(dfFactorA$F1)
ggplot(dfFactorA, aes(F1)) + geom_histogram(binwidth = .25, aes(fill = ..count..)) + theme_bw() + xlab("Theta") + ylab("") + xlim(c(-4,4))

dfFactorB <- data.frame(scoresFactorB)
dfFactorB$F1 <- as.numeric(dfFactorB$F1)
ggplot(dfFactorB, aes(F1)) + geom_histogram(binwidth = .25, aes(fill = ..count..)) + theme_bw() + xlab("Theta") + ylab("") + xlim(c(-4,4))
