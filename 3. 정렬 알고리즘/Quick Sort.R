### Algorithm : 빠른 정렬(Quick Sort) ###

## 1. Code
quicksort <- function(x) {
  if(length(x) <= 1) return(x)
  pivot <- x[1]
  rest <- x[-1]
  vec1 <- rest[rest < pivot]
  vec2 <- rest[rest >= pivot]
  vec1 <- quicksort(vec1)
  vec2 <- quicksort(vec2)
  result <- c(vec1, pivot, vec2)
  return(result)
}

## 2. Result Check
x <- round(rnorm(100, 50, 10))
quicksort(x)

x <- rnorm(100, 50, 10)
quicksort(x)
