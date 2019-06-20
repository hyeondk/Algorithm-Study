### Algorithm : R에서 소수 구하기 ###

## 방법 1 : primes package를 사용한 소수 구하기
library(primes)

# Result Check
is_prime(5)

##############################################################################

## 방법 2 : User-defined function(소수인가 아닌가?)
is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

# Result Check
is.prime(9)
is.prime(13)

##############################################################################

## 방법 3 : 아리스토텔레스의 체(Sieve of Eratosthenes)를 이용한 소수 구하기

## 원리 ##
# (1) 2부터 n까지 숫자(2, 3, ..., n)를 모두 생성한다.
# (2) p = 2를 가장 작은 소수라고 정한다.
# (3) p(=2)의 배수가 되는 n보다 작거나 같은 숫자는 모두 소수에서 제외한다. 여기서 p는 소수로 제거하지 않는다. 
# (4) (단계 3)에서 제거되지 않은 숫자들 중에 가장 작은 숫자(p)를 탐색한다. 그리고 (3)을 반복한다. 만약 가장 작은 숫자가 없으면 이 과정을 끝낸다.
# (5) 소수들의 결과를 제시한다.

# Code 1
findprime <- function(n) {
  s <- rep(TRUE, n)
  s[1] <- FALSE
  p <- 2
  for(i in p:sqrt(n)) {
    s[seq.int(2*p, n, p)] <- FALSE
    p <- p + min(which(s[p+1:n]))
  }
  return(which(s))
}

# Result Check
findprime(100)

# Code 2
findprime2 <- function(n) {
  s <- 2:n
  i <- 1
  while(p < length(s)) {
    p <- s[i]
    vec <- s[s %% p == 0]
    vec <- vec[!vec %in% p]
    s <- s[!s %in% vec]
    i <- i + 1
  }
  return(s)
}

# Result Check
findprime2(100)
