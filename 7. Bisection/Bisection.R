### Algorithm : Bisection(이분법)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## Assumption
# f(x) = x^2 - sin(x) - 0.5

## Code 1
bisection_sol <- function(a, b, eps) {
  fx <<- function(x) {
    return(x^2 - sin(x) - 0.5)
  }
  m <- (a + b)/2
  if(fx(m) == 0) return(m)
  while(TRUE) {
    if(fx(a) * fx(m) < 0) b <- m else a <- m
    m2 <- (a + b)/2
    if(abs(m - m2) < eps) break else m <- m2
  }
  return(m2)
}

# Result Check
results <- matrix(rep(0, 15), 5, 3)
colnames(results) <- c("sol", "fx", "eps")
for(i in 1:5) {
  results[i, ] <- c(bisection_sol(-1, 0, 10^(-2*i)), fx(bisection_sol(-1, 0, 10^(-2*i))), 10^(-2*i))
}
results

###################################################################

## Code 2
bisection <- function(a, b, eps) {
  f <- function(x) {
    return(x^2 - sin(x) - 0.5)
  }
  
  m_new <- (a+b)/2
  m_old <- b
  
  while(abs(m_new - m_old) > eps) {
    if(f(m_new) == 0) {
      return(c(m_new, f(m_new)))
    } else if(f(m_new) * f(a) < 0) {
      b <- m_new
    } else {
      a <- m_new
    }
    m_old <- m_new
    m_new <- (a+b)/2
  }
  return(c(m_new, f(m_new)))
}

# Result Check
results <- matrix(rep(0, 15), 5, 3)
colnames(results) <- c("sol", "fx", "eps")
for(i in 1:5) {
  results[i, ] <- c(bisection(-1, 0, 10^(-2*i)), 10^(-2*i))
}
results
