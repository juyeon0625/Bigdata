cus1 <- c(5, NA, 4, NA, 1, 0, 3)
cus2 <- c(4, 4, 4, NA, NA, NA, 1)
cus3 <- c(5, 4, NA, 1, 2, NA, 3)
cus4 <- c(1, 2, 1, 4, 3, 5, 2)
cus5 <- c(0, 1, NA, 3, 5, 5, NA)
cus6 <- c(NA, 2, NA, NA, 4, 4, 2)
cus_a <- c(5, NA, NA, 1, NA, NA, 2)

data <- data.frame(rbind(cus1, cus2, cus3, cus4, cus5, cus6, cus_a))
colnames(data) <- c("pro1", "pro2", "pro3", "pro4", "pro5", "pro6", "pro7")
attach(data)

length(cus1)
##################고객의 평균 평점 
v_ <- function(x){

  x<- x[!is.na(x)]
  rate <- 1/length(x) * sum(x)
  return(rate)
}

v_(cus1)
v_(cus2)
v_(cus3)
v_(cus4)
v_(cus5)
v_(cus6)
v_(cus_a)

################ 고객과의 유사성을 나타내는 가중치
w <- function(x, y){
  x <- as.vector(x)
  y <- as.vector(y)
  
  rated <- is.finite(x) & is.finite(y)
  common <- which(rated)
  
  target <- x[common] - v_(x)
  customer <- y[common] - v_(y)
  
  up <- sum(target * customer, na.rm=TRUE)
  
  down <- sqrt(sum(target^2, na.rm=TRUE) * sum(customer^2, na.rm=TRUE))
  
  w_ <- as.vector(up/down)
  
  return(w_)
  
}

w(cus_a, cus1)
w(cus_a, cus2)
w(cus_a, cus3)
w(cus_a, cus4)
w(cus_a, cus5)
w(cus_a, cus6)

############## 가중치의 절대값들의 합 1 

k <- 1/sum(abs(c(w(cus_a, cus1), w(cus_a, cus2), w(cus_a, cus3),
                 w(cus_a, cus4), w(cus_a, cus5), w(cus_a, cus6))))

############# 목표고객에 대한 상품의 평점 추정치

p <- function(d, a, j){
  
  d <- data
  double <- 0
  length(double) <- ncol(d) - 1
  avgs <- NULL
  prod <- NULL
  
  double[i] <- if(!identical(a, d[,i])) {w(a, d[,i])} else{0}
    
  cat("w(cus_a, ", i, ") :", w(a, d[,i]), "\n")

  
  for(i in 1:length(d)){
    avgs <- c(avgs, v_(d[,i]))
    prod <- p(prod, d[j, i])
    cat("average for", i, "th colum is", v_(d[,i]), ",", j, "th product's expected rating is", d[j ,i], "\n")
    
  }
  r <- v_(a) + k * sum(double * (prod - avgs), na.rm=TRUE)
  return(r)
}

p(data, data[,7], 2)
p(data, data[,7], 3)
p(data, data[,7], 4)
p(data, data[,7], 5)
p(data, data[,7], 6)
