data1 <- read.csv("shops_training27.csv", header=TRUE, skip=1)
data2 <- read.csv("shops_test27.csv", header=TRUE, skip=1)

install.packages("tree")
library(tree)

install.packages("party")
library(party)

install.packages("forecast")

############################################# TRUE, FALSE -> 1, 0

ish <- NULL
for(i in data1$isHoliday){
  ifelse(i == "FALSE", ish <- c(ish, 0), ish <- c(ish, 1)) #휴일x 0, 휴일 1
}
data1$isHoliday <- ish

ish <- NULL
for(i in data2$isHoliday){
  ifelse(i == "FALSE", ish <- c(ish, 0), ish <- c(ish, 1)) #휴일x 0, 휴일 1
}
data2$isHoliday <- ish

before_ish <- NULL
for(i in data1$before_isHoliday){
  ifelse(i == "FALSE", before_ish <- c(before_ish, 0), before_ish <- c(before_ish, 1)) #휴일x 0, 휴일 1
}
data1$before_isHoliday <- before_ish

before_ish <- NULL
for(i in data2$before_isHoliday){
  ifelse(i == "FALSE", before_ish <- c(before_ish, 0), before_ish <- c(before_ish, 1)) #휴일x 0, 휴일 1
}
data2$before_isHoliday <- before_ish

after_ish <- NULL
for(i in data1$after_isHoliday){
  ifelse(i == "FALSE", after_ish <- c(after_ish, 0), after_ish <- c(after_ish, 1)) #휴일x 0, 휴일 1
}
data1$after_isHoliday <- after_ish

after_ish <- NULL
for(i in data2$after_isHoliday){
  ifelse(i == "FALSE", after_ish <- c(after_ish, 0), after_ish <- c(after_ish, 1)) #휴일x 0, 휴일 1
}
data2$after_isHoliday <- after_ish

ra <- NULL
for(i in data1$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1))
}
data1$rain <- ra

ra <- NULL
for(i in data2$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1))
}
data2$rain <- ra

fo <- NULL
for(i in data1$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1))
}
data1$fog <- fo

fo <- NULL
for(i in data2$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1))
}
data2$fog <- fo

# 국경일인 경우 1

data1$anniversary <- ifelse(data1$anniversary == "평일", 0, 1) 
data1$before_anniversary <- ifelse(data1$before_anniversary == "평일", 0, 1) 
data1$after_anniversary <- ifelse(data1$after_anniversary == "평일", 0, 1)

data2$anniversary <- ifelse(data2$anniversary == "평일", 0, 1) 
data2$before_anniversary <- ifelse(data2$before_anniversary == "평일", 0, 1) 
data2$after_anniversary <- ifelse(data2$after_anniversary == "평일", 0, 1)

################################## tree

attach(data1)

myformula <- (class_orders ~ data1$isHoliday
              + data1$anniversary
              #+ month
              #+ weekday
              #+ day
              + data1$holiday_order
              + data1$rain
              + data1$fog
              + data1$temp_highest
              #+ LYSM_similarDates_avg_orders
              + data1$prediction_orders
              + data1$ES_orders_25
              + data1$ES_orders_75
              #+ similarDates_movingAvg_orders
              + data1$currentFamilySavingCSI
              + data1$underlyingInflationRate
              + data1$importChangeRate
              + data1$football_national_cnt
              + data1$naverTrend_Shop_lastweek
)

ctree_1 <- ctree(myformula, data1)
plot(ctree_1)

pred <- predict(ctree_1, newdata = data2)

################################## 18개로 구간 나누기 
range(data1$class_orders)
data1_range <- ((max(data1$class_orders)) - (min(data1$class_orders)))/18

r1 <- seq(from=20, by=data1_range, to=422)
pr <- cut(pred, r1, labels=c(1:18))
tr <- cut(data2$class_orders, r1, labels=c(1:18))

table(pr)
table(tr)

table(pr) - table(tr)

##################################
TP <- data2$class_orders - pred

FP <- class_orders - pred

FN <- class_orders - pred

p <- (length(TP) - length(which(FP>=1)) - length(which(FN<=-1)))
     /((length(TP) - length(which(FP>=1)) - length(which(FN<=-1))) + (length(which(FP>=1))))

r <- (length(TP) - length(which(FP>=1)) - length(which(FN<=-1)))
      /((length(TP) - length(which(FP>=1)) - length(which(FN<=-1))) + (length(which(FN<=-1))))

f <- ((1+1) * p * r) / (r+p)













################################## tree 
attach(data1)
formula <- lm(class_orders ~ data1$isHoliday
              + data1$anniversary
              #+ month
              #+ weekday
              #+ day
              + data1$holiday_order
              + data1$rain
              + data1$fog
              + data1$temp_highest
              #+ LYSM_similarDates_avg_orders
              + data1$prediction_orders
              + data1$ES_orders_25
              + data1$ES_orders_75
              #+ similarDates_movingAvg_orders
              + data1$currentFamilySavingCSI
              + data1$underlyingInflationRate
              + data1$importChangeRate
              + data1$football_national_cnt
              + data1$naverTrend_Shop_lastweek
)
ctree_1 <- ctree(formula, data = data1)
plot(ctree_1)

predict(ctree_1, data = data2)



range <- cut(data1$class_orders, 18, right=FALSE)
table(range)




breaks <- round(seq(24,411,by=21.44445))
training_range <- cut(data1$class_orders,breaks,right=FALSE)
table(training_range)
