### Algorithm : Finding prime numbers in R(R에서 소수 구하기)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## Method 1 : Use 'primes package' to find prime numbers(primes package를 사용한 소수 구하기)
if(!require(primes)) {
  install.packages("primes")
}
library(primes)

# Result Check
is_prime(5)

##############################################################################

## Method 2 : User-defined function - Is it a prime number or not?(소수인가 아닌가?)
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

## Method 3 : Find the prime number using Aristotle's Sieve of Eratosthenes(아리스토텔레스의 체를 이용한 소수 구하기)

## Principle ##
# (1) Generates all numbers(2, 3, ..., n) from 2 to n.
# (1) 2부터 n까지 숫자(2, 3, ..., n)를 모두 생성한다.

# (2) Let p = 2 be the smallest prime number.
# (2) p = 2를 가장 작은 소수라고 정한다.

# (3) Any number less than or equal to n that is a multiple of p(=2) is excluded from the prime number. Here, p is not removed as a prime number.
# (3) p(=2)의 배수가 되는 n보다 작거나 같은 숫자는 모두 소수에서 제외한다. 여기서 p는 소수로 제거하지 않는다.

# (4) In Step 3, search for the smallest number(p) among the numbers that have not been removed. Then repeat step 3. If there is no smallest number, end this process.
# (4) [단계 3]에서 제거되지 않은 숫자들 중에 가장 작은 숫자(p)를 탐색한다. 그리고 [단계 3]을 반복한다. 만약 가장 작은 숫자가 없으면 이 과정을 끝낸다.

# (5) Presents the results of prime numbers.
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
  p <- s[i]
  while(p < length(s)) {
    vec <- s[s %% p == 0]
    vec <- vec[!vec %in% p]
    s <- s[!s %in% vec]
    i <- i + 1
    p <- s[i]
  }
  return(s)
}

# Result Check
findprime2(100)
