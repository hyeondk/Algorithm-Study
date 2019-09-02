### Algorithm : 병합 정렬(Merge Sort) ###

## 1. Code
mergesort <- function(x) {
  if(length(x) <= 1) {
    return(x)
  } # 데이터가 1개 이하이면 정렬 필요X
  
  mid <- length(x) %/% 2 # 중간을 기준으로 그룹 2개로 분리
  group1 <- mergesort(x[1:mid]) # 재귀 호출로 group1 정렬
  group2 <- mergesort(x[-(1:mid)]) # 재귀 호출로 group2 정렬
  
  # 두 그룹을 하나로 병합해준다.
  result <- NULL
  while(length(group1) >= 1 & length(group2) >= 1) { # 두 그룹에 자료가 남아있으면 반복
    if(group1[1] < group2[1]) { # 두 그룹의 1번째 원소끼리만 비교
      result <- c(result, group1[1])
      group1 <- group1[-1] # group1 값이 더 작을 경우, 그 값을 빼고 결과에 추가
    } else {
      result <- c(result, group2[1])
      group2 <- group2[-1]
    }
  }
  
  # 아직 남아있는 데이터를 결과에 추가
  # group1과 group2 중 이미 빈 것은 while을 바로 지나간다.
  while(length(group1) >= 1) {
    result <- c(result, group1[1])
    group1 <- group1[-1]
  }
  
  while(length(group2) >= 1) {
    result <- c(result, group2[1])
    group2 <- group2[-1]
  }
  
  return(result)
}

## 2. Result Check
d <- c(6, 8, 3, 9, 10, 1, 2, 4, 7, 5)
mergesort(d)

d2 <- sample(1:15, 15)
mergesort(d2)

d3 <- round(rnorm(30, 20, 8), 1)
mergesort(d3)
