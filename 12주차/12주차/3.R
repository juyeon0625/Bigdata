cus1 <- c(1, 0, 1, 0, 1, 0, 1)
cus2 <- c(1, 1, 1, 0, 0, 0, 1)
cus3 <- c(1, 1, 0, 1, 1, 0, 1)
cus4 <- c(1, 1, 1, 1, 0, 1, 1)
cus5 <- c(0, 1, 0, 1, 1, 1, 0)
cus6 <- c(0, 1, 0, 0, 1, 1, 1)
cusa <- c(1, 0, 0, 1, 0, 0, 1)

data <- data.frame(rbind(cus1, cus2, cus3, cus4, cus5, cus6, cusa))
colnames(data) <- c("pro1", "pro2", "pro3", "pro4", "pro5", "pro6", "pro7")
attach(data)

#####################확률 p

p <- function(x){
  1/length(x) * sum(x[x==1])
}

p(cus1)
p(cus2)
p(cus3)
p(cus4)
p(cus5)
p(cus6)
p(cusa)

####################확률 p_ai

p_a1 <- 1/7 *2
p_a2 <- 1/7 *2
p_a3 <- 1/7 *3
p_a4 <- 1/7 *3
p_a5 <- 1/7 *1
p_a6 <- 1/7 *1

##################가중치 w

w <- function(pai, i){
  up <- pai - p(cusa) * p(i)
  down <- sqrt(p(cusa) * (1 - p(cusa))) * sqrt(p(i) * (1 - p(i)))
  up/down
}

w(p_a1, cus1)
w(p_a2, cus2)
w(p_a3, cus3)
w(p_a4, cus4)
w(p_a5, cus5)
w(p_a6, cus6)

############## k

k <- 1/sum(abs(c(w(p_a1, cus1), w(p_a2, cus2), w(p_a3, cus3),
                 w(p_a4, cus4), w(p_a5, cus5), w(cusa, cus6))))

#############평점 추정치 p

p <- function(data, j){
  i <- length(data[1,])-1
  b <- NULL
  1:i
  b <- NULL
  for(x in 1:i){
    b <- c(b, w(pai, x))
  }
  k * sum(b*data[1:(length(data[,1])-1),j])
}
