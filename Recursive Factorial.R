### Algorithm : 팩토리얼 재귀함수 ###

## 1. Principle
# (1) User-defined function
fact <- function(n) {
  s <- 1
  for(i in 1:n) {
    s <- s * i #합에 i를 계속 곱한다.
  }
  return(s)
}

# (2) Result Check
fact(2)
fact(10)

## 2. Code
fact.recursive <- function(n) {
  if(n == 1) return(1) #n = 1인 경우를 꼭 세팅해주어야 함.
  return(n * fact.recursive(n-1)) #n x (n-1)!
}

## 3. Result Check
fact.recursive(10)

# 주의 : 자칫하면 무한루프에 빠지게 된다. 조심할 것!
