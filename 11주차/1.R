age <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
height <- c(100.1, 107.2, 114.1, 121.7, 126.8, 130.9, 137.5, 143.2, 149.4, 151.6, 154.0, 154.6, 155.0, 155.1, 155.3, 155.7)

plot(age, height)

regress <- function(x,y){  # y=ax+b 회귀식 
  
Sxx <- sum((x - mean(x))^2)
Syy <- sum((y - mean(y))^2)
Sxy <- sum((x - mean(x)) * (y - mean(y)))

a <- Sxy / Sxx
b <- mean(y) - mean(x)*a

ab <- list(a,b)
return(ab)
}

sq <- function(x, y){ # 결정계수 
  
  ab <- regress(x, y)
  
  a <- as.numeric(ab[1])
  b <- as.numeric(ab[2])
  
  r <- a*sum((x-mean(x)) * (y-mean(y))) / sum((y-mean(y))^2)
  
  return (r)
}

sq(age, height)
sq(1/age, height)
sq(sqrt(age), height)
sq(age^2, height)
sq(log(age), height)