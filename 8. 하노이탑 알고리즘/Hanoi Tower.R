### Algorithm : 하노이 탑 옮기기 ###

## 1. Principle
# (1) 크기가 다른 원반 n개를 출발점 기둥에서 도착점 기둥으로 전부 옮겨야 한다.
# (2) 원반은 한 번에 1개씩만 옮길 수 있다.
# (3) 원반을 옮길 때는 한 기둥의 맨 위 원반을 뽑아, 다른 기둥의 맨 위로만 옮길 수 있다.
# (4) 기둥의 중간에서 원반을 빼내거나 빼낸 원반을 다른 기둥의 중간으로 끼워 넣을 수 없다.
# (5) 원반을 옮기는 과정에서 큰 원반을 작은 원반 위로 올릴 수 없다.

## 2. Code

# Input
# 올미겨는 원반 수 : n
# 옮길 원반이 현재 있는 출발점 기둥 : pstart
# 원반을 옮길 도착점 기둥 : pend
# 옮기는 과정에서 사용할 보조 기둥 : paux

hanoi <- function(n, pstart, pend, paux) {
  if(n == 1) {
    print(paste(pstart, "->", pend))
  } else {
    hanoi(n-1, pstart, paux, pend) # 원반 n-1개를 paux로 이동(pend를 보조 기둥으로 사용)
    print(paste(pstart, "->", pend)) # 가장 큰 원반을 목적지로 이동
    hanoi(n-1, paux, pend, pstart) # paux에 있는 원반 n-1개를 목적지로 이동(pstart를 보조 기둥으로 사용)
  }
}

## 3. Result Check

# Input
# 출발점 기둥 : 1
# 도착점 : 3
# 보조 기둥 : 2

print("n = 1")
hanoi(1, 1, 3, 2)

print("n = 2")
hanoi(2, 1, 3, 2)

print("n = 3")
hanoi(3, 1, 3, 2)

print("n = 4")
hanoi(4, 1, 3, 2)
