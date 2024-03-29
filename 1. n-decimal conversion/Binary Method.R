### Algorithm : Transition from decimal to binary(10진법에서 2진법으로 전환하기)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## 1. Code
find2num <- function(n) {
  k <- 1
  while(TRUE) {
    if(log(2^(k-1), base = 2) <= log(n, base = 2) & log(n, base = 2) < log(2^k, base = 2)) break
    k <- k + 1
  } # Binary digit output via k(k를 통해 이진법 자릿수 출력)
  
  x <- vector(mode = "numeric", length = k) # Create empty vector(빈 벡터 생성)
  x[k] <- n %% 2 # The rest at the kth(k번째에 나머지)
  q <- n %/% 2 # Enter a quotient for the variable q(q라는 변수에 몫 입력)
  while(k >= 2) {
    k <- k - 1 # Enter a value starting with x[4] by lowering the number of digits one by one(자릿수 하나씩 낮춰서 x[4]부터 값 입력)
    x[k] <- q %% 2
    q <- q %/% 2
  } # repeat until k = 2(k = 2까지 반복)
  x[1] <- 1 # The first value of a binary number is 1(이진수의 1번째 값은 1)
  
  return(as.integer(x)) # binary output(이진수 출력)
}

## 2. Result Check
find2num(17)
find2num(11)
find2num(9)
find2num(233)
