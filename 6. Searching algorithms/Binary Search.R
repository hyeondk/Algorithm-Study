### Algorithm : Binary search(이분 탐색)
### Writer : Donghyeon Kim
### Update : 2022.04.19

## 1. Principle
# ex) The process is to open the book appropriately, compare the sides, and then search again only in the direction where the side you want to find is located.
# When there are only a few pages left, turn one page at a time to find it -> Sequential Search
# In other words, I was looking for the direction I wanted while applying the binary search and sequential search algorithm at the same time.
# ex) 책을 적당히 펼쳐 쪽을 비교한 다음 찾고자 하는 쪽이 있을 방향으로만 다시 탐색하는 과정이다.
# 몇 쪽 남지 않았을 때 한장씩 넘기면서 찾는 과정 -> 순차 탐색
# 즉, 이분 탐색과 순차 탐색 알고리즘을 동시에 응용하면서 원하는 쪽을 찾고 있었던 것.

# Points to Note!
# Why were you able to use 'binary search' when looking for a specific side in the book?
# It was possible because the number of pages in all books was growing one after another.
# In other words, since the book's side numbers are already aligned, you can immediately know whether to look for the front of a specific side or the back side.
# 주의점!
# 책에서 특정한 쪽을 찾을 때 '이분 탐색'을 사용할 수 있었던 이유는 무엇인가?
# 모든 책의 쪽 수가 1부터 빠짐없이 차례로 커지고 있었기 때문에 가능했다.
# 즉, 책의 쪽 번호가 이미 정렬되어 있으므로 특정 쪽의 앞쪽을 찾아봐야 할지 뒤쪽을 찾아봐야 할지 바로 알 수 있는 것이다.

## 2. Assumption
L <- c(16, 100, 9, 1, 25, 36, 4, 64, 49, 81)
L <- sort(L)
L # Check vector L

## 3. Code
binarysearch <- function(L, x) {
  idxstart <- 1 # Starting index : 1(시작 인덱스 : 1)
  idxend <- length(L) # Ending index : length(L)(끝 인덱스 : length(L))
  while(idxstart <= idxend) { # Repeat if the starting index is less than or equal to the ending index(시작 index가 끝 index보다 작거나 같을 경우 계속 반복함)
    mid <- (idxstart + idxend) %/% 2 # The median is the share of the two indexes(중앙값은 두 index를 더해 나온 몫임)
    if(x == L[mid]) { # If L[mid] and x are the same, the index output called mid(L[mid]와 x가 같다면 mid라는 index 출력)
      return(mid)
    } else if(x > L[mid]) { # If it is larger than L[mid], raise the starting index by one step(L[mid]보다 크게 있다면 시작 index를 1단계 올려준다)
      idxstart <- mid + 1
    } else { # If it is smaller than L[mid], lower the ending index by one step(L[mid]보다 작게 있다면 끝 index를 1단계 낮춰준다)
      idxend <- mid - 1
    } # Use the while statement to repeat until you find it(그렇게 찾을 때까지 반복하기 위해 while문을 사용한다)
  }
  return(-1) # If no value is found, -1(값을 찾지 못하면 -1)
}

## 4. Result Check
binarysearch(L, 81)
binarysearch(L, 90)
