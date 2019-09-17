data1 <- read.csv("d:\\shops_training27.csv", header=TRUE)
data2 <- read.csv("d:\\shops_test27.csv", header=TRUE)

install.packages("party")
library(party)

######################################## TRUE, FALSE -> 1, 0

for(i in length(data1$isHoliday)){
  data1$isHoliday[i] = ifelse(data1$isHoliday[i] == "FALSE", 0, 1) #휴일x 0, 휴일 1
}

for(i in length(data2$isHoliday)){
  data2$isHoliday[i] = ifelse(data2$isHoliday[i] == "FALSE", 0, 1) #휴일x 0, 휴일 1
}

for(i in length(data1$before_isHoliday)){
  data1$before_isHoliday[i] = ifelse(data1$before_isHoliday[i] == "FALSE", 0, 1) #휴일x 0, 휴일 1
}

for(i in length(data2$before_isHoliday)){
  data2$before_isHoliday[i] = ifelse(data2$before_isHoliday[i] == "FALSE",  0, 1) #휴일x 0, 휴일 1
}

for(i in length(data1$after_isHoliday)){
  data1$after_isHoliday[i] = ifelse(data1$after_isHoliday[i] == "FALSE", 0 , 1) #휴일x 0, 휴일 1
}

for(i in length(data2$after_isHoliday)){
  data2$after_isHoliday[i] = ifelse(data2$after_isHoliday[i] == "FALSE", 0, 1)  #휴일x 0, 휴일 1
}

for(i in length(data1$rain)){
  data1$rain[i] = ifelse(data1$rain[i] == "TRUE", 1, 0)
}

for(i in length(data2$rain)){
  data2$rain[i] = ifelse(data2$rain[i] == "TRUE", 1, 0)
}

for(i in length(data1$fog)){
  data1$fog[i] = ifelse(data1$fog[i] == "TRUE", 1, 0)
}

for(i in length(data2$fog)){
  data2$fog[i] = ifelse(data2$fog[i] == "TRUE", 1, 0)
}

# 국경일인 경우 1

data1$anniversary <- ifelse(data1$anniversary == "평일", 0, 1) 
data1$before_anniversary <- ifelse(data1$before_anniversary == "평일", 0, 1) 
data1$after_anniversary <- ifelse(data1$after_anniversary == "평일", 0, 1)

data2$anniversary <- ifelse(data2$anniversary == "평일", 0, 1) 
data2$before_anniversary <- ifelse(data2$before_anniversary == "평일", 0, 1) 
data2$after_anniversary <- ifelse(data2$after_anniversary == "평일", 0, 1)


################################## 18개로 구간 나누기 
range(data1$class_orders)
data1_range <- ((max(data1$class_orders)) - (min(data1$class_orders)))/18

r1 <- seq(from=20, by=data1_range, to=422)

data1$class_orders <- cut(data1$class_orders, r1, label=c(1:18))
data2$class_orders <- cut(data2$class_orders, r1, label=c(1:18))

table_data1 <- table(data1$class_orders)
table_data2 <- table(data2$class_orders)


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

######################################## tree 
library(tree)
library(party)

attach(data1)
myformula <- tree(class_orders ~ anniversary
              + before_anniversary
              + after_anniversary
              + isHoliday
              + after_isHoliday
              + holiday_order
              + rain
              + fog
              + temp_highest
              + LYSM_similarDates_avg_orders
              + prediction_orders
              #+ ES_orders_25
              + ES_orders_75
              + similarDates_movingAvg_orders
              + underlyingInflationRate
              + importChangeRate
              + football_national_cnt
              + naverTrend_Shop_lastweek
              ,data1
)

predicted <- predict(myformula, newdata = data2)

calculate(data2$class_orders, predicted)