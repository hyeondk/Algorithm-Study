### Algorithm : 선택 정렬(Selection Sort) ###

## 1. Principle
# (1) 최솟값의 위치를 찾아주는 알고리즘을 정의한다.
findminidx <- function(x) {
  minidx <- 1
  if(length(x) == 1) { # 길이가 1일때는 index = 1로만 출력되어야 함. 아니면 오류 발생!
    return(minidx)
  }
  for(i in 2:length(x)) {
    if(x[i] < x[minidx]) {
      minidx <- i # 최솟값 위치를 minidx에 저장
    }
  }
  return(minidx)
}

# (2) 위에서 정의한 최솟값 함수를 이용하여 선택 정렬을 시행한다.
selsort <- function(x) {
  result <- NULL
  while(length(x) > 0) {
    index <- findminidx(x) # 최솟값 위치를 index 변수에 저장
    result <- c(result, x[index]) # result에 x[index]로 최솟값 저장
    x <- x[-index] # index로 찾은 위치는 x에서 제거하고 계속 반복
  }
  return(result)
}

# (2-1) 위와 동일한 방법이며, while문을 for문으로 변경만 하였다.
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
# 방법 1 : 오름차순
selectsort <- function(x) {
  n <- length(x)
  for(i in 1:(n-1)) { # index는 1부터 (n-1)까지 반복
    minidx <- i # 최솟값을 i로 각각 두고 반복 시행
    for(j in (i+1):n) { # (i+1)부터 n까지 반복
      if(x[j] < x[minidx]) { # ex) i = 1이면 minidx = 1이고, j는 2~5에서 반복한다. 이 때, x[1]보다 작은 값이 있다면 그 위치를 minidx에 저장
        minidx <- j
      }
    }
    temp <- x[i] # 최솟값 index를 찾았다면 x[i]와 x[minidx]끼리 변경한다! 결과적으로 x가 자체적으로 변경된다.
    x[i] <- x[minidx]
    x[minidx] <- temp
  }
  return(x)
}

# Result Check
d <- c(2, 4, 5, 1, 3)
selectsort(d)


# 방법 1 응용 : 내림차순
selectsort2 <- function(x) {
  n <- length(x)
  for(i in 1:(n-1)) { # index는 1부터 (n-1)까지 반복
    minidx <- i # 최솟값을 i로 각각 두고 반복 시행
    for(j in (i+1):n) { # (i+1)부터 n까지 반복
      if(x[j] > x[minidx]) { # ex) i = 1이면 minidx = 1이고, j는 2~5에서 반복한다. 이 때, x[1]보다 작은 값이 있다면 그 위치를 minidx에 저장
        minidx <- j
      }
    }
    temp <- x[i] # 최솟값 index를 찾았다면 x[i]와 x[minidx]끼리 변경한다! 결과적으로 x가 자체적으로 변경된다.
    x[i] <- x[minidx]
    x[minidx] <- temp
  }
  return(x)
}

# Result Check
d <- c(2, 4, 5, 1, 3)
selectsort2(d)


# 방법 2
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
