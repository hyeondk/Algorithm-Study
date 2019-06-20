### Algorithm : 이차방정식 근의 공식 ###

## 1. Principle
# (1) Input a, b, c
a <- as.numeric(readline(prompt = "a:"))
b <- as.numeric(readline(prompt = "b:"))
c <- as.numeric(readline(prompt = "c:"))

# (2) User-defined function
D <- b^2 - 4*a*c
if(D == 0) {
  print((-b)/(2*a))
} else {
  if(D > 0) {
    print(c((-b - sqrt(D))/(2*a), (-b + sqrt(D))/(2*a)))
  }
  else print("근이 없음")
}

## 2. Code
equation2 <- function(a, b, c) {
  D <- b^2 - 4*a*c
  if(D == 0) {
    print((-b)/(2*a))
  } else {
    if(D > 0) {
      print(c((-b - sqrt(D))/(2*a), (-b + sqrt(D))/(2*a)))
    }
    else print("근이 없음")
  }
}

## 3. Result Check
equation2(1, 5, 6)
equation2(2, 7, 9)
