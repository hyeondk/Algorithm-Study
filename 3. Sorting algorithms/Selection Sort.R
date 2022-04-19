### Algorithm : Selection sort(선택 정렬)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## 1. Principle
# (1) Defines an algorithm that locates the minimum value.(최솟값의 위치를 찾아주는 알고리즘을 정의한다.)
findminidx <- function(x) {
  minidx <- 1
  if(length(x) == 1) { # When the length is 1, it should only be output to index = 1(길이가 1일때는 index = 1로만 출력되어야 함)
    return(minidx)
  }
  for(i in 2:length(x)) {
    if(x[i] < x[minidx]) {
      minidx <- i # Store minimum location in minidx(최솟값 위치를 minidx에 저장)
    }
  }
  return(minidx)
}

# (2) Selection sorting is performed using the minimum value function defined above.(위에서 정의한 최솟값 함수를 이용하여 선택 정렬을 시행한다.)
selsort <- function(x) {
  result <- NULL
  while(length(x) > 0) {
    index <- findminidx(x) # Store minimum position in index variable(최솟값 위치를 index 변수에 저장)
    result <- c(result, x[index]) # Store minimum value as x[index] in result(result에 x[index]로 최솟값 저장)
    x <- x[-index] # Remove the position found by index from x and repeat continuously(index로 찾은 위치는 x에서 제거하고 계속 반복)
  }
  return(result)
}

# (2-1) It is the same method as above, and only the while statement was changed to the for statement.(위와 동일한 방법이며, while문을 for문으로 변경만 하였다.)
selsort2 <- function(x) {
  result <- NULL
  for(i in 1:length(x)) {
    index <- findminidx(x)
    result <- c(result, x[index])
    x <- x[-index]
  }
  return(result)
}

# (3) Result Check
d <- c(2, 4, 5, 1, 3)
selsort(d)

selsort2(d)

###########################################################################################

## 2. Code
# Method 1 : Ascending order(오름차순)
selectsort <- function(x) {
  n <- length(x)
  for(i in 1:(n-1)) { # index repeats from 1 to (n-1)(index는 1부터 (n-1)까지 반복)
    minidx <- i # Repeat with minimum i as each(최솟값을 i로 각각 두고 반복 시행)
    for(j in (i+1):n) { # Repeat from (i+1) to n((i+1)부터 n까지 반복)
      if(x[j] < x[minidx]) {
      # ex) If i = 1, minidx = 1, and j repeats from 2 to 5. If there is a value less than x[1], store the location in minidx.
      # ex) i = 1이면 minidx = 1이고, j는 2~5에서 반복한다. 이 때, x[1]보다 작은 값이 있다면 그 위치를 minidx에 저장한다.
      
        minidx <- j
      }
    }
    temp <- x[i]
    # If the minimum index is found, change between x[i] and x[minidx]. As a result, x changes itself.
    # 최솟값 index를 찾았다면 x[i]와 x[minidx]끼리 변경한다. 결과적으로 x가 자체적으로 변경된다.
    
    x[i] <- x[minidx]
    x[minidx] <- temp
  }
  return(x)
}

# Result Check
d <- c(2, 4, 5, 1, 3)
selectsort(d)


# Method 2 : Descending order(내림차순)
selectsort2 <- function(x) {
  n <- length(x)
  for(i in 1:(n-1)) { # index repeats from 1 to (n-1)(index는 1부터 (n-1)까지 반복)
    minidx <- i # Repeat with minimum i as each(최솟값을 i로 각각 두고 반복 시행)
    for(j in (i+1):n) { # Repeat from (i+1) to n((i+1)부터 n까지 반복)
      if(x[j] > x[minidx]) {
      # ex) If i = 1, minidx = 1, and j repeats from 2 to 5. If there is a value less than x[1], store the location in minidx.
      # ex) i = 1이면 minidx = 1이고, j는 2~5에서 반복한다. 이 때, x[1]보다 작은 값이 있다면 그 위치를 minidx에 저장
      
        minidx <- j
      }
    }
    temp <- x[i]
    # If the minimum index is found, change between x[i] and x[minidx]. As a result, x changes itself.
    # 최솟값 index를 찾았다면 x[i]와 x[minidx]끼리 변경한다. 결과적으로 x가 자체적으로 변경된다.
    
    x[i] <- x[minidx]
    x[minidx] <- temp
  }
  return(x)
}

# Result Check
d <- c(2, 4, 5, 1, 3)
selectsort2(d)


# Method 3
selectsort3 <- function(x) {
  n <- length(x)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(x[i] > x[j]) {
        temp <- x[j]
        x[j] <- x[i]
        x[i] <- temp
      }
    }
  }
  return(x)
}

# Result Check
d <- c(2, 4, 5, 1, 3)
selectsort3(d)
