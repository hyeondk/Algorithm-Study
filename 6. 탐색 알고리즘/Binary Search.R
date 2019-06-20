### Algorithm : 이분 탐색(Binary Search) ###

## 1. Principle
# ex : 책을 적당히 펼쳐 쪽을 비교한 다음 찾고자 하는 쪽이 있을 방향으로만 다시 탐색하는 과정
# 몇 쪽 남지 않았을 때 한장씩 너기면서 찾는 과정 -> 순차 탐색
# 즉, 이분 탐색과 순차 탐색 알고리즘을 동시에 응용하면서 원하는 쪽을 찾고 있었던 것!

# 주의점!
# 책에서 특정한 쪽을 찾을 때 이분 탐색을 할 수 있었던 이유?
# 모든 책의 쪽 수가 1부터 빠짐없이 차례로 커지고 있었기 때문에 가능했다.
# 즉, 책의 쪽 번호가 이미 정렬되어 있으므로 특정 쪽의 앞쪽을 찾아봐야 할지 뒤쪽을 찾아봐야 할지 바로 알 수 있는 것이다.

## 2. Assumption
L <- c(16, 100, 9, 1, 25, 36, 4, 64, 49, 81)
L <- sort(L)
L # Check vector L

## 3. Code
binarysearch <- function(L, x) {
  idxstart <- 1 # 시작 index : 1
  idxend <- length(L) # 끝 index : length(L)
  while(idxstart <= idxend) { # 시작 index가 끝 index보다 작거나 같을 경우 게속 반복!
    mid <- (idxstart + idxend) %/% 2 # 중앙값은 두 index를 더해 나온 몫
    if(x == L[mid]) { # L[mid]와 x가 같다면 mid라는 index 출력
      return(mid)
    } else if(x > L[mid]) { # L[mid]보다 크게 있다면 시작 index를 1단계 올려준다.
      idxstart <- mid + 1
    } else { # L[mid]보다 작게 있다면 끝 index를 1단계 낮춰준다.
      idxend <- mid - 1
    } # 그렇게 찾을 때까지 반복하기 위해 while문을 사용한 것이다.
  }
  return(-1) # 값을 찾지 못하면 -1
}

## 4. Result Check
binarysearch(L, 81)
binarysearch(L, 90)
