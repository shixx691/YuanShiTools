#' Calculate Mean, Variane, SD
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))
func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' MLE of gamma distribution
#'
#' Computes the liklihood of a gamma distribution
#'
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' func3(rnorm(10))
func3 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}

#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func4(d)
func4 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Weighted mean, var, sd with user checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){
  
  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))
  
  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)
  
  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))
  
  stopifnot(!is.na(d$x))
  stopifnot(!is.na(d$p))
  
  stopifnot(!is.nan(d$x))
  stopifnot(!is.nan(d$p))
  
  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func6(NA)

func6 <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#' MLE 
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)`
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' x1 = rgamma(100,3)
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#' 
func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

#' wrapper for acquiring marginals and other slices of data data2007
#'
#'select two speific rows and compute the product
#'@param x data.frame
#'
#' @return list
#' @export

selectmydata<-function(x){
  library(magrittr)
  library(tidyverse)
  xa<-data2007%>%
    select(gdpPercap, pop) %>% 
    mutate(gdp = pop * gdpPercap)
  return(xa)}
  
#' quiz 2 function 1 
#' 
#' Write an R function that, given a numeric matrix A and a numeric vector x, calculates xTA−1x
#' 
#' @param a matrix
#' @param x vector
#' 
#' @return object
#' @export
#' @example
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' fun1(a, x)

fun1 <- function(a, x) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(!is.na(x))
  stopifnot(!is.na(a))
  stopifnot(!is.nan(a))
  stopifnot(!is.nan(x))
  stopifnot(is.finite(x))
  stopifnot(is.finite(a))
  stopifnot(nrow(a) == ncol(a)) #the number of rows and colons should be the same if a is a square matrix
  stopifnot(nrow(a) == length(x))
  y <- solve(a, x)
  sum(x * y)}

#' quiz 2 function 2 
#' 
#' rewrite function for the preceding problem so it is a binary operator rather than an apparent function call, that is, if your function from the preceding problem was invoked
#' 
#' @param a matrix
#' @param x vector
#' 
#' @return object
#' @export

`%fun2%` <- function(a, x) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(!is.na(x))
  stopifnot(!is.na(a))
  stopifnot(!is.nan(a))
  stopifnot(!is.nan(x))
  stopifnot(is.finite(x))
  stopifnot(is.finite(a))
  stopifnot(nrow(a) == ncol(a)) #the number of rows and colons should be the same if a is a square matrix
  stopifnot(nrow(a) == length(x))
  y <- solve(a, x)
  sum(x * y)
}


#' quiz 2 function 3 
#' 
#' Write a function that takes a numeric matrix and standardizes its columns as (x−mean(x))/sd(x).
#' 
#' @param a matrix
#' 
#' @return matrix
#' @export

fun3 <- function(a) 
{
  stopifnot(!is.na(a))
  stopifnot(!is.nan(a))
  stopifnot(is.finite(a))
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  #since no deduction for loops, I m using it
  for (i in 1:ncol(a))#number of colons
  {
    tempA <- a[ , i]
    a[ , i] <- (tempA - mean(tempA)) / sd(tempA) #stardlize each value in each colon
  }    
  return(a)
}

#' Homework 2 q4 function
#' 
#' Try your function myapply on all of the example
#' 
#' @param a matrix
#' 
#' @return matrix
#' @export

fun <- function(a) 
{
  stopifnot(!is.na(a))
  stopifnot(!is.nan(a))
  stopifnot(is.finite(a))
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  tempfun <- function(x)
  {
    (x - mean(x))/sd(x)
  }
  apply(a,2,tempfun)
} 

#' Homework 2 q5 function
#' 
#' question 5 
#' @param x matrix
#' @param MARGIN margin of object you want to work on
#' @param FUN function
#' @export

myapply <- function(X, MARGIN, FUN, ...)
{
  
  stopifnot(length(dim(X))==2)
  
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  } 
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}
