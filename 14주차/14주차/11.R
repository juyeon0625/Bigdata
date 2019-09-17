data1 <- read.csv("d:\\shops_training27.csv", header=TRUE)
data2 <- read.csv("d:\\shops_test27.csv", header=TRUE)

#################### 휴이  1
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
d <- data.frame(data1$isHoliday, data1$anniversary, data1$holiday_order, data1$rain, data1$fog, 
                data1$temp_highest, data1$ES_orders_75, data1$naverTrend_Shop_lastweek)
s_d <- scale(d)

############## 고유값  
eg <- eigen(cor(s_d))
sc <- s_d %*% eg$vectors[,1:2]

############## plot
rownames(eg$vectors) = colnames(d)
biplot(-sc, -eg$vectors[,1:2])
s <- princomp(d, cor=TRUE)
summary(s)

#################### 18등분으로 나누기 
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

######################### 
calculate <- function(actual, predicted){
  
  predicted <- round(predicted)
  res <- actual - predicted
  res_table <- table(res)
  
  tp_range <- ifelse(-1 <= res & res <= 1, 1, 0)
  fp_range <- ifelse(res > 1, 1, 0)
  fn_range <- ifelse(res < -1, 1, 0)
  
  tp <- sum(tp_range) 
  fp <- sum(fp_range) 
  fn <- sum(fn_range) 
  
  P <- tp / (tp + fp)
  R <- tp / (tp + fn)
  F_measure <- 2 * R * P / (R+P)
  
  cat("MAPE      : ", MAPE(actual, predicted), "\n",
      "Precision : ", P, "\n",
      "Recall    : ", R, "\n",
      "F-Measure : ", F_measure, "\n\n")
  
}


################### tree
install.packages("tree")
library(tree)

attach(data1)

mytree <- tree(class_orders~ isHoliday
               + anniversary
               #+ month
               #+ weekday
               #+ day
               + holiday_order
               #+ rain
               #+ fog
               #+ temp_highest
               #+ LYSM_similarDates_avg_orders
               #+ prediction_orders
               #+ ES_orders_25
               + ES_orders_75
               #+ similarDates_movingAvg_orders
               #+ currentFamilySavingCSI
               #+ underlyingInflationRate
               #+ importChangeRate
               #+ football_national_cnt
               + naverTrend_Shop_lastweek
               ,data1)

plot(mytree)
text(mytree, all = T)

pred <- predict(mytree, newdata = data2)
pred_round <- round(pred)

calculate(data2$class_orders, pred)