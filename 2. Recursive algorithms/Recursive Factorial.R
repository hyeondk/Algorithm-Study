### Algorithm : Factorial recursive function(팩토리얼 재귀함수)
### Writer : Donghyeon Kim
### Date : 2022.04.19

## 1. Principle
# (1) User-defined function
fact <- function(n) {
  s <- 1
  for(i in 1:n) {
    s <- s * i # Continue to multiply i on sum(합에 i를 계속 곱한다)
  }
  return(s)
}

# (2) Result Check
fact(2)
fact(10)

## 2. Code
fact.recursive <- function(n) {
  if(n == 1) return(1) # In some cases, n = 1, so must set it up(n = 1인 경우도 있을 수 있으니 꼭 설정해주어야 함)
  return(n * fact.recursive(n-1)) # n x (n-1)!
}

## 3. Result Check
fact.recursive(10)

# Caution : You may fall into an infinite loop. Be careful!
# 주의 : 자칫하면 무한루프에 빠지게 된다. 조심할 것!
