data1 <- read.csv("d:\\shops_training27.csv", header=TRUE)
data2 <- read.csv("d:\\shops_test27.csv", header=TRUE)


#################### 휴일 1
data1$anniversary <- ifelse(data1$anniversary == "평일", 0, 1)
data1$before_anniversary <- ifelse(data1$before_anniversary == "평일", 0, 1)
data1$after_anniversary <- ifelse(data1$after_anniversary == "평일", 0, 1)

data2$anniversary <- ifelse(data2$anniversary == "평일", 0, 1)
data2$before_anniversary <- ifelse(data2$before_anniversary == "평일", 0, 1)
data2$after_anniversary <- ifelse(data2$after_anniversary == "평일", 0, 1)

data1$isHoliday <- ifelse(data1$isHoliday == TRUE, 1, 0)
data1$before_isHoliday <- ifelse(data1$before_isHoliday == TRUE, 1, 0)
data1$after_isHoliday <- ifelse(data1$after_isHoliday == TRUE, 1, 0)

data2$isHoliday <- ifelse(data2$isHoliday == TRUE, 1, 0)
data2$before_isHoliday <- ifelse(data2$before_isHoliday == TRUE, 1, 0)
data2$after_isHoliday <- ifelse(data2$after_isHoliday == TRUE, 1, 0)

#################### 비 안개 1
data1$fog <- ifelse(data1$fog == TRUE, 1, 0)
data1$rain <- ifelse(data1$rain == TRUE, 1, 0)

data2$fog <- ifelse(data2$fog == TRUE, 1, 0)
data2$rain <- ifelse(data2$rain == TRUE, 1, 0)


################ 변수 표준화 
d <- data.frame(data1$prediction_orders, data1$anniversary, data1$ES_orders_25, data1$holiday_order)
s_d <- scale(d)

s_anni <- scale(data1$anniversary)
s_pre <- scale(data1$prediction_orders)
s_ES25 <- scale(data1$ES_orders_25)
s_holior <- scale(data1$holiday_order)
############## 상관행렬 구하기 
corr <- cor(d)
round(corr, 2)

############## 고유값 
eg <- eigen(cor(s_d))
sc <- s_d %*% eg$vectors[,1:2]

############## 주성분 
z1 <- (-0.6199058)*s_pre + (-0.3541140)*s_anni + (-0.2938699)*s_ES25 + (-0.6355790)*s_holior
z2 <- (-0.09004810)*s_pre + (-0.58925330)*s_anni + (0.80162510)*s_ES25 + (0.04548717)*s_holior


biplot(-sc, -eg$vectors[,1:2])
rownames(eg$vectors) = colnames(d)
s <- princomp(d, cor=TRUE)
summary(s)

#################### 범위 18등분
range(data1$class_orders)
data1_range <- (max(data1$class_orders) - min(data1$class_orders)) / 18

rv <- seq(from = 20, by = data1_range, to = 422)

data1$class_orders <- as.numeric(cut(data1$class_orders, rv, label = c(1:18)))
data2$class_orders <- as.numeric(cut(data2$class_orders, rv, label = c(1:18)))

table(data1$class_orders)
table(data2$class_orders)



######################### MAPE
MAPE <- function(y, exp_y){
  return( sum(abs(y - exp_y)/y)/length(y) * 100 )
}


######################### Confusion Matrix 출력 및 모델 성능 분석 함수
calculate <- function(actual, predicted){ # 패러미터는 모두 벡터
  
  predicted <- round(predicted)
  res <- actual - predicted
  res_table <- table(res)
  
  tp_range <- ifelse(-1 <= res & res <= 1, 1, 0)
  fp_range <- ifelse(res > 1, 1, 0)
  fn_range <- ifelse(res < -1, 1, 0)
  
  tp <- sum(tp_range) # 실제 값과 일치하며 양수
  fp <- sum(fp_range) # 불일치 양수 값
  fn <- sum(fn_range) # 불일치 음수 값
  
  P <- tp / (tp + fp)
  R <- tp / (tp + fn)
  F_measure <- 2 * R * P / (R+P)
  
  # Confusion Matrix
  conf_matrix <- matrix( c(tp, fp, fn, NA), nrow = 2, ncol = 2)
  colnames(conf_matrix) = c("Forecast = P", " N")
  rownames(conf_matrix) = c("TP FN", "FP TN")
  
  cat("True Positive  : ", tp, "\n",
      "False Positive : ", fp, "\n",
      "False Negative : ", fn, "\n\n",
      "-----------------------\n",
      "Total          : ", length(predicted), "/", length(actual), "\n\n",
      
      "MAPE      : ", MAPE(actual, predicted), "\n",
      "Precision : ", P, "\n",
      "Recall    : ", R, "\n",
      "F-Measure : ", F_measure, "\n\n")
  
  return (conf_matrix)
  
}

install.packages("tree")
library(tree)

attach(data1)

mytree <- tree(class_orders~ anniversary
               + prediction_orders
               + holiday_order
               ,data1)
summary(mytree)
plot(mytree)
text(mytree, all = T)

pred <- predict(mytree, newdata = data2)
pred_round <- round(pred)

table(data2$class_orders - pred_round)

calculate(data2$class_orders, pred)
