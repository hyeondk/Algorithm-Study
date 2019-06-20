### Algorithm : 누적합 재귀함수 ###

## 1. Principle
# (1) R function
cumsum(1:10)
cumsum(c(4, 7, 8, 9))
cumsum(1.5:5.5)

# (2) User-defined function
csum <- function(x) {
  s <- 0
  cum <- c()
  for(i in 1:length(x)) {
    s <- s + x[i]
    cum <- c(cum, s) # i번째마다 누적합 출력
  }
  return(cum)
}

# (3) Result Check
csum(1:10)

## 2. Code
csum.recursive <- function(x) {
  if(length(x) == 1) return(x)
  return(c(x[1], x[1] + csum.recursive(x[-1]))) # i번째마다 누적합 출력
}

## 3. Result Check
csum.recursive(1:10)
csum.recursive(c(4, 6, 8, 9))
