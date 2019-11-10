library(GA)
library(parallel)
library(doParallel)
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

data <- read.csv("D:\\Documents\\User_Files\\School\\Faks\\3_letnik\\IS\\Assignment1\\DLBCL.csv", header=TRUE, sep=",")
data$X = NULL
data$atclass = NULL

ctrl <- trainControl(
  number = 3,
  method="cv"
)

fitnessFunc <- function(binSelection)
{
  if (sum(binSelection) < 2 || sum(binSelection) > 1000)
  {
    return(-1)
  }
  else 
  {
    columns <- seq(1, length(binSelection))
    selectedCol <- columns[as.logical(binSelection)]
    selectedCol <- append(selectedCol, 1071, after = length(selectedCol))
    
    training <- select(data, selectedCol)
    
    plsFit <- train(
      class ~ .,
      data = training,
      method = "ctree",
      trControl = ctrl
    )  
    
    return(mean(plsFit$results$Accuracy) + 1/sum(binSelection))
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
