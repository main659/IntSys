# 1.
f <- function(x, y)
{
  z <- (1 - x)^2 + exp(1) * (y - x^2)^2
  
  z
}

# 2.
library(GA)

x <- seq(-1,1, length=40)
y <- x
z <- outer(x,y,f)

monitor_1 <- function(obj) 
{
  index <- match(max(obj@fitness), obj@fitness)
  point <- obj@population[index,1:2]
  points(trans3d(point[1],point[2],f(point[1],point[2]),p1mat), col="chartreuse", pch=4)
}

monitor_2 <- function(obj) 
{
  index <- match(max(obj@fitness), obj@fitness)
  point <- obj@population[index,1:2]
  points(trans3d(point[1],point[2],f(point[1],point[2]),p2mat), col="violet", pch=4)
}

monitor_3 <- function(obj) 
{
  index <- match(max(obj@fitness), obj@fitness)
  point <- obj@population[index,1:2]
  points(trans3d(point[1],point[2],f(point[1],point[2]),p3mat), col="yellow", pch=4)
}

monitor_4 <- function(obj) 
{
  index <- match(max(obj@fitness), obj@fitness)
  point <- obj@population[index,1:2]
  points(trans3d(point[1],point[2],f(point[1],point[2]),p4mat), col="darkred", pch=4)
}

par(mfrow=c(1, 4), mar=c(1,1,1,1))
p1mat <- persp(x,y,z, col="lightblue", theta=110, phi=5, main="gareal_blxCrossover, pmutation=0.1")
GA1 <- ga(type = "real-valued", fitness = function(x) f(x[1], x[2]), monitor = monitor_1, maxiter = 100, popSize = 50, lower = c(-1, -1), upper = c(1, 1), crossover = gareal_blxCrossover, pmutation = 0.1)
points(trans3d(GA1@solution[1],GA1@solution[2],f(GA1@solution[1],GA1@solution[2]),p1mat), col="chartreuse", pch=21)

p2mat <- persp(x,y,z, col="lightblue", theta=110, phi=5, main="gareal_blxCrossover, pmutation=0.5")
GA2 <- ga(type = "real-valued", fitness = function(x) f(x[1], x[2]), monitor = monitor_2, maxiter = 100, popSize = 50, lower = c(-1, -1), upper = c(1, 1), crossover = gareal_blxCrossover, pmutation = 0.5)
points(trans3d(GA2@solution[1],GA2@solution[2],f(GA2@solution[1],GA2@solution[2]),p2mat), col="violet", pch=21)

p3mat <- persp(x,y,z, col="lightblue", theta=110, phi=5, main="gareal_laplaceCrossover, pmutation=0.1")
GA3 <- ga(type = "real-valued", fitness = function(x) f(x[1], x[2]), monitor = monitor_3, maxiter = 100, popSize = 50, lower = c(-1, -1), upper = c(1, 1), crossover = gareal_laplaceCrossover, pmutation = 0.1)
points(trans3d(GA3@solution[1],GA3@solution[2],f(GA3@solution[1],GA3@solution[2]),p3mat), col="yellow", pch=21)

p4mat <- persp(x,y,z, col="lightblue", theta=110, phi=5, main="gareal_laplaceCrossover, pmutation=0.5")
GA4 <- ga(type = "real-valued", fitness = function(x) f(x[1], x[2]), monitor = monitor_4, maxiter = 100, popSize = 50, lower = c(-1, -1), upper = c(1, 1), crossover = gareal_laplaceCrossover, pmutation = 0.5)
points(trans3d(GA4@solution[1],GA4@solution[2],f(GA4@solution[1],GA4@solution[2]),p4mat), col="darkred", pch=21)

GA1
plot(GA1)
summary(GA1)

