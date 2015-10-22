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

# Set trellis parameters
my.settings <- list(
  strip.background=list(col="#eeeeee"),
  strip.border=list(col="white"),
  superpose.line = list(col=myColors[1:5]),
  superpose.symbol = list(col=myColors[1:5]),
  axis.line = list(col = "#eeeeee")
)

# Open Data Frame
praticasPro  <- read.csv("praticasprofissionais_df.csv", stringsAsFactors = FALSE)

# Convert items into numeric
# for (i in c(32:68)){
#   praticasPro[,i]   <-  Recode(praticasPro[,i], "'Discordo totalmente'=1 ; 'Discordo'=2 ; 'Nem discordo, nem concordo' = 3; 'Concordo' = 4; 'Concordo totalmente' = 5; else = NA")
#   praticasPro[,i]   <-  as.numeric(praticasPro[,i])  
# }

## Sum items to remove NA's
praticasPro$scaleSum  <- rowSums(praticasPro[,32:68])

# Subset only with participants who consented - N 3247 agreed
praticasPro <- subset(praticasPro, praticasPro$termo == "Sim")

################
# DEMOGRAPHICS
################

## Age
praticasPro$age <- gsub("[[:alpha:]]", " ", praticasPro$idade) # Remove all alphabetic characters
praticasPro$age  <- as.numeric(praticasPro$age) # Convert into numeric
praticasPro$age[praticasPro$age < 18 | praticasPro$age > 68 ]  <- NA # Recode values with conversion problem into NA

## Sex
cbind(round(prop.table(sort(table(praticasPro$sexo), decreasing = TRUE)),3))*100

## Degree
cbind(round(prop.table(sort(table(praticasPro$escolaridade), decreasing = TRUE)),3))*100

## Contact 
cbind(round(prop.table(sort(table(praticasPro$contatoanterior), decreasing = TRUE)),3))*100

## Deal with
cbind(round(prop.table(sort(table(praticasPro$lidadiretamente), decreasing = TRUE)),3))*100

################
# SCALE COR
################

# Subset only with all cases completed
aScale <- subset(praticasPro, !is.na(praticasPro$scaleSum))

# Select Scale items
aScale  <-  aScale[,32:68]

## Reverse code items according to Theory
# for (i in c(14,19,28,29,32,33,34,35)) {
#   aScale[,i] <- recode(aScale[,i], "1=5;2=4;4=2;5=1")
# }

# Correlation Test among all variables
# rcor.test(aScale, method="kendall") - This is incredibly slow, so uncomment this line at your own risk.

################
# EFA AND IRT
################


###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
polyAll <- polychoric(aScale)
# Rotated PCA
faAll_rot <- principal(polyAll$rho, nfactors = 2, rotate="oblimin")
print.psych(faAll_rot, digits=2, cut= .4)
# Results - No tems were removed since all had loadings larger than .4


###########################
# CFA - BFACTOR
###########################

item_pos <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)
efaVersion  <-   c(1,1,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,2,2,2,2,1,1,1,2,2,1,2,2,2,2,1,1,2,2,1)
#theoryVersion <- c(1,1,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,2,2,2,2,1,1,1,2,2,1,2,2,2,2,1,1,2,2,1)
dimShort <-cbind(item_pos, efaVersion)
dimShort

# EFA result
cfaEFA <- bfactor(aScale, efaVersion)
summary(cfaEFA)
coef(cfaEFA)
M2(cfaEFA)

# Results - We have a significant number of items with bad loadings. Therefore, I recommend the use of alternative confirmatory models.

bScale <- aScale[, -c(2,9,11,14,15,18,24,25,26,28,33,34)]
efaVersion1  <-   c(1,1,1,1,1,1,1,2,2,1,2,1,2,2,2,2,1,2,2,2,2,2,1,2,1)

###########################
# IRT
###########################

## NOTE - There has been some concern on the function GRM of ltm package. You can see a forum discussion here: http://stats.stackexchange.com/questions/63891/is-r-output-reliable-specially-irt-package-ltm. In order to avoid this concern, I decided to use the grm estimation of mirt package, which is compared to other softwares.

# MIRT function with two correlated factors
## Exploratory model
mod1 <- mirt(aScale, itemtype = "graded", model = 2, message = FALSE)
mod1; summary(mod1);coef(mod1)

## Confirmatory model
model <- 'F1 = 1-8, 13,17,18, 23-25, 28,33,34,37 
          F2 = 9-12, 14,15, 16, 19-22, 26,27,29-32,35,36
          COV = F1*F2'

mod2 <- mirt(aScale, itemtype = "graded", model = model
             , message = FALSE)

# Create Scale from factor 1 - Version 1
factorA <- aScale[, c(1,2,3,4,5,6,7,8,13,17,18,23,24,25,28,33,34,37)]
factorB <- aScale[, -c(1,2,3,4,5,6,7,8,13,17,18,23,24,25,28,33,34,37)]

mod3 <- mirt(factorA, itemtype = "graded", model = 1, message = FALSE)
mod4 <- mirt(factorB, itemtype = "graded", model = 1, message = FALSE)

# Compute the M2 model fit statistic from Maydey-Olivares and Joe (2006)
M2(mod1); M2(mod2); M2(mod3); M2(mod4)
anova(mod1, mod2)


##########################################
# Improving Models with separated factors
##########################################

coef(mod3, IRTpars = TRUE)
coef(mod4, IRTpars = TRUE)

# Plot - Show test information
itemplot(mod3, type = "trace", item = 1)
plot(mod3, type = "info")
plot(mod4, type = "info")

# Plot - Realibility
plot(mod3, type = "rxx")
plot(mod4, type = "rxx")

# Plot - Information trace
plot(mod3, type = "trace", par.settings = my.settings, main = "", auto.key=list(space="top", columns=5, title="", cex.title=1, cex.text = .3, text=c("Disc. Total.","Disc.","Nem Con., nem disc.","Conc.","Conc. Total.")))

plot(mod4, type = "trace", par.settings = my.settings, main = "", auto.key=list(space="top", columns=5, title="", cex.title=1, cex.text = .3, text=c("Disc. Total.","Disc.","Nem Con., nem disc.","Conc.","Conc. Total.")))

# Plot - Info trace
plot(mod3, type = "infotrace", par.settings = my.settings, main = "")
plot(mod4, type = "infotrace", par.settings = my.settings, main = "")


# Plot items from scale Factor 1
pdf("plotFactor1.pdf")
for (i in 1:18){
print(itemplot(mod3, item = i, type = "trace", main = paste("Item ", names(factorA[i])), par.settings = my.settings,auto.key=list(space="top", columns=3, title="", cex.title=1, cex.text = .3, text=c("DT","D","M","C","CT"))))
}
dev.off()

# Plot items from scale Factor 
pdf("plotFactor2.pdf")
for (i in 1:19){
  print(itemplot(mod4, item = i, type = "trace", main = paste("Item ", names(factorB[i])), auto.key=list(space="top", columns=5, title="", cex.title=1, cex.text = .3, text=c("DT","D","M","C","CT"))))
}
dev.off()

itemfit(mod3)
itemfit(mod4)

residuals(mod3)
residuals(mod4)


# This line below create multiple graphs. Still need work to be implemented.
# grid.arrange(itemplot(mod3, item = 1, type = "trace"),itemplot(mod3, item = 2, type = "trace"), ncol=2, newpage = TRUE )


##################
# BIN
##################

# Grade Response Model
triShort1 <- grm(factorA)

# Estimate Coeficients
coef(triShort1)

# Plot graphs
par(mfrow = c(4,4))
for (i in 1:16) {  
plot(triShort1, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}

# Create Short Scale - Version 2
aScale2 <- aScale[, -c(1,2,3,4,5,6,7,8,13,17,18,23,24,25,28,33,34,37)]

# # Grade Response Model
factorB <- grm(aScale2)

# Estimate Coeficients
coef(factorB)

# Plot graphs
par(mfrow = c(4,4))
for (i in 1:15) {  
  plot(triShort2, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}