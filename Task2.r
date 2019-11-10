library(GA)
library(caret)
library(dplyr)
library(parallel)
library(doParallel)
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

data <- read.csv("C:\\Users\\david\\Downloads\\DLBCL.csv", header=TRUE, sep=",")
data$X = NULL
data$atclass = NULL

trainIndex <- createDataPartition(data$class, p = .8, list = FALSE, times = 1)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

ctrl <- trainControl(
  number = 3,
  method="cv"
)

treeModel <- function(binSelection)
{
  columns <- seq(1, length(binSelection))
  selectedCol <- columns[as.logical(binSelection)]
  selectedCol <- append(selectedCol, length(binSelection) + 1, after = length(selectedCol))
  
  training <- select(dataTrain, selectedCol)
  
  plsFit <- train(
    class ~ .,
    data = training,
    method = "ctree",
    trControl = ctrl
  )
  
  return(plsFit)
}

fitnessFunc <- function(binSelection)
{
  if (sum(binSelection) < 2 || sum(binSelection) > 1000)
  {
    return(-1)
  }
  else 
  {
    plsFit <- treeModel(binSelection)
    
    return(mean(plsFit$results$Accuracy)/sum(binSelection))
  }
}

GA <- ga(
  type = "binary", 
  fitness = fitnessFunc, 
  crossover = gabin_uCrossover, 
  elitism = 2, 
  pmutation = 0.2,
  nBits = 1070, 
  maxiter = 100,
  run = 10, 
  popSize = 40, 
  parallel = T, 
  keepBest = TRUE)

summary(GA)

#number of features
sum(GA@solution[1,])

#accuracy of test set and everything combined
finalFit <- treeModel(GA@solution[1,])
predicted <- predict(finalFit, dataTest)
confusionMatrix(predicted, dataTest$class)
predicted <- predict(finalFit, data)
confusionMatrix(predicted, data$class)

#columns <- seq(1, 1070)
#selectedCol <- columns[as.logical(GA@solution[1,])]
#selectedCol <- append(selectedCol, 1071, after = length(selectedCol))

#saving <- select(data, selectedCol)

#write.csv(saving, 'DLBCL2.csv')
