# Load Libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)


# Artigo 3 
preditores <- read.csv("estudo 3/preditores.csv", na.strings = "-")
preditores <- preditores[, -1]
preditores <- preditores[, -16]
preditores$aprovado <- factor(preditores$aprovado, levels = c("Não","Sim"))
preditores$idade <- as.numeric(preditores$idade)
preditores$idade <- ifelse(preditores$idade < 18 | preditores$idade > 80, NA, preditores$idade)


# CREATE DATA PARTITION
trainIndex <- createDataPartition(preditores$aprovado, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- preditores[trainIndex,]
final <- preditores[-trainIndex,]

# MODELING

###################
# CART
###################

ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

mpartModel  <- train(aprovado ~ . , data = preditores, 
                     method="rpart", metric = "Accuracy",
                     trControl = ctrl)

mpartModel  <- train(aprovado ~ . , data = preditores, 
                     method="rpart", metric = "Accuracy")

confusionMatrix(mpartModel$pred, mpartModel$obs)
predict(mpartModel, final)

mpartModel <- rpart(aprovado ~ ., data = preditores, control = c(cp = "0.15"))
fancyRpartPlot(mpartModel)

###################
# GBM - BOOST
###################

boostModel  <- train(aprovado ~ . , data = preditores, 
                     method="gbm", metric = "Accuracy",
                     trControl = ctrl)
plot(boostModel)

###################
# SVM
###################

svmTrain  <- train(aprovado  ~ . , data = preditores, 
                   method="svmRadial", metric = "Accuracy",
                   trControl = ctrl)

svmTrain

###################
# C50
###################

C50  <- train(aprovado  ~ . , data = preditores, 
                   method="C5.0", metric = "Accuracy",
                   trControl = ctrl)

C50


###################
# RANDOM FOREST
###################

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
m_rf  <- train(aprovado ~ . , data = preditores, 
               method="rf", metric = "Accuracy",
               trControl = ctrl, tuneGrid = grid_rf)
