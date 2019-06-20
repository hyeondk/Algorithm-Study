### Algorithm : 삽입 정렬(Insertion Sort) ###

## 1. Code
insertsort <- function(x) {
  if(length(x) == 1) return(x)
  for(i in 2:length(x)) {
    j <- i - 1
    comp <- x[i]
    while(j > 0 && x[j] > comp) {
      x[j + 1] <- x[j]
      j <- j - 1
      x[j + 1] <- comp
    }
  }
  return(x)
}

## 2. Result Check
d <- c(2, 4, 5, 1, 3)
insertsort(d)

insertsort(c(51, 31, 17, 5, 9))
insertsort(c(56, 89, 21, 99, 35, 1, 65, 7, 25))
insertsort(7)
