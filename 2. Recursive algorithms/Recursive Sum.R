### Algorithm : Sum recursive function(합 재귀함수)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## Method 1 : 1 + 2 + ... + n
# (1) User-defined function
asum <- function(n) {
  s <- 0
  for(i in 1:n) { # input type : integer n
    s <- s + i
  }
  return(s)
}

# (2) Result Check
asum(55)

## 2. Code
sum.recursive <- function(n) {
  if(n == 1) return(n)
  return(n + sum.recursive(n-1)) # n + summation(n-1)
}

## 3. Result Check
sum.recursive(55)

##########################################################################

## Method 2 : x[1] + x[2] + ... x[n]
# (1) User-defined function
bsum <- function(x) {
  s <- 0
  for(i in 1:length(x)) {
    s <- s + x[i]
  }
  return(s)
}

# (2) Result Check
bsum(55)
bsum(1:100)
bsum(c(4, 6, 7, 8))

## 2. Code
sum.recursive2 <- function(x) {
  if(length(x) == 1) return(x)
  return(x[1] + sum.recursive2(x[-1]))
}

## 3. Result Check
sum.recursive2(1:100)
sum.recursive2(c(4, 6, 8, 9))
