### Algorithm : Sequential search(순차 탐색)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## Method 1 : When the same value is present, only the first position is output(같은 값이 있을 때 처음 나온 위치만 출력해주는 경우)
search_list <- function(L, x) {
  for(i in 1:length(L)) {
    if(x == L[i]) return(i)
  }
  return(-1)
}

# Result Check
L <- c(17, 92, 18, 33, 58, 7, 33, 42)
search_list(L, 18)
search_list(L, 33) # There are two 33s, but only the first index is output(33은 2개 있으나, 제일 처음에 있는 index만 출력)
search_list(L, 900)

## Method 2 : If you output all of the corresponding positions when the same values are present(같은 값이 있을 때 해당 위치를 모두 출력해주는 경우)
search_list2 <- function(L, x) {
  index <- NULL
  for(i in 1:length(L)) {
    if(x == L[i]) {
      index <- c(index, i)
    }
  }
  if(length(index) == 0) {
    return(-1)
  } else {
    return(index)
  }
}

# Result Check
L <- c(17, 92, 18, 33, 58, 7, 33, 42)
search_list2(L, 18)
search_list2(L, 33) # Output both positions since there are 2 33s(33이 2개 있으므로 위치 2개 모두 출력)
search_list2(L, 900)
