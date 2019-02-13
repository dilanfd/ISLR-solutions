## ISLR 5.3 Lab. p. 195


## Estimating the accuracy of a Linear regression model.

## import the library
library('ISLR')
library('MASS')

boot.fn <- function(data, index){
    lm.auto <- lm(mpg~horsepower, data = data, subset = index)
    return(coef(lm.auto))
}

## Using the boot package and boot function
library(boot)

## Q: 5.R.R3
boot_Xy.fn <- function(data, index){
    lm.Xy <- lm(formula = y ~ ., data = Xy, subset = index)
    return(coef(lm.Xy))
}


## Block bootstrap
new.rows <- c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
new.XY <- Xy[new.rows, ]

new.boot.Xy.fn <- function(data, index){
    lm.XY <- lm(formula = y ~ ., data = new.XY, subset = index)
    return(coef(lm.XY))
}

ts.boot.fn <- function(data){
    fit <- lm(y~., data)
    return(coef(fit))
}

## time series boot
tsboot(Xy, ts.boot.fn, R = 1000, l = 100, sim = "fixed")

