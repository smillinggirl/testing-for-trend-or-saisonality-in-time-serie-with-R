
#HADJ-ALI  
#WISSAL 
#GRP 11

install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(ggplot2)
DF<- read.csv("C:/Users/PC_/Downloads/MATIC-USD.csv", stringsAsFactors=TRUE)
View(DF)
X<-DF$High
dim(DF)
MA<-stats::filter(X, filter=array(1/10,dim=10), method = c("convolution"),
           sides = 2, circular = FALSE)
plot(X,type='l')
    
lines(MA,col='red')

#la tendance:

par(mfrow = c(1, 2))
prix.ts <- ts(DF$High, frequency = 12)
acf(prix.ts, na.action = na.omit)
diff.prix.ts <- diff(prix.ts, lag = 1, differences = 1)
acf(diff.prix.ts, na.action = na.omit)
plot(prix.ts, col = "blue")


plot(diff.prix.ts, col = "orangered2")
time <- c(1:nrow(DF))
DF$Date<-time
reg <- lm(DF$High~ time + I(time^2) + I(time^3), data = DF)
par(mfrow = c(1, 2))
plot(DF$Date, DF$High, type = "l", xlab = "CHANGES",
     ylab = "Ind. Prix", col = "blue")
lines(DF$Date, DF$High, col = "red", lwd = 2)

n <- 100
const <- rep(1, n)
f1 <- function(x) x
f2 <- function(x) pmax(x - 0.25, 0)
f3 <- function(x) pmax(x - 0.5, 0)
f4 <- function(x) pmax(x - 0.8, 0)
x <- seq(0, 1, length = n)
design <- as.matrix(data.frame(const = const, f1 = f1(x), f2 = f2(x),
                               f3 = f3(x), f4 = f4(x)))
matplot(x, y = design, type = "l", lty = 1, ylab = "", main = "truncated power functions q = 1")
set.seed(150)

install.packages("mgcv")
install.packages("nlme")
library(mgcv)
library(nlme)

coef <- runif(5, -1, 1)
f <- design %*% coef
plot(x, f, type = "l", col = "BLUE", lwd = 2)
abline(v = c(0.25, 0.5, 0.8), lty = "dashed")
library(mgcv)

g <- gam(High ~ s(time, k = 10), data = DF)
plot(DF$Date, DF$High, type = "l", xlab = "",
     ylab = "Ind. Prix. Conso. Ménages (INSEE)", col = "blue",
     lwd = 2)
lines(DF$Date, g$fitted, col = "red", lwd = 2)


#la Saisonalité

MA <- filter(DF$High, filter = array(1/48, dim = 48),
             method = c("convolution"), sides = 2, circular = FALSE)
plot(DF$Date, DF$High, type = "l", xlab = "",
     ylab = "VARIATIONS ", col = "seagreen4", lwd = 1)
lines(DF$Date, MA, col = "red", lwd = 2)


MA2 <- filter(MA, filter = array(1/(3 * 20), dim = 3 * 20), method = c("convolution"),
              sides = 2, circular = FALSE)
plot(DF$Date, DF$High, type = "l", xlab = "",
     ylab = "PEOCESSUS", col = "seagreen4", lwd = 1)

lines(DF$Date, MA, col = "red", lwd = 2)
lines(DF$Date, MA2, col = "blue", lwd = 2)

n <- 100
t <- c(1:n)
w = 2 * pi/5
S <- cos(w * t)
eps <- rnorm(n, 0, 1)
X <- S + eps
X <- ts(X, frequency = 5)
par(mfrow = c(1, 2))
acf(X)
acf(diff(X, lag = 5, differences = 1))

