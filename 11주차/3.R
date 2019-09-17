shop <- read.csv("d:\\shops_training27.csv", header=TRUE)
data <- read.csv("d:\\shops_test27.csv", header=TRUE)


################################################ 0, 1로 바꾸기 
isH <- NULL
for(i in shop$isHoliday){
  ifelse(i == "FALSE", isH <- c(isH, 0), isH <- c(isH, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$isHoliday <- isH #0과 1로 설정한 isHoliday를 shop에 있는 isHoliday에 설정 

isH <- NULL
for(i in data$isHoliday){
  ifelse(i == "FALSE", isH <- c(isH, 0), isH <- c(isH, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$isHoliday <- isH  

before <- NULL
for(i in shop$before_isHoliday){
  ifelse(i == "FALSE", before <- c(before, 0), before <- c(before, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$before_isHoliday <- before 

before <- NULL
for(i in data$before_isHoliday){
  ifelse(i == "FALSE", before <- c(before, 0), before <- c(before, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$before_isHoliday <- before

after <- NULL
for(i in shop$after_isHoliday){
  ifelse(i == "FALSE", after <- c(after, 0), after <- c(after, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$after_isHoliday <- after 

after <- NULL
for(i in data$after_isHoliday){
  ifelse(i == "FALSE", after <- c(after, 0), after <- c(after, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$after_isHoliday <- after

an <- NULL
for(i in shop$anniversary){
  ifelse(i == "평일", an <- c(an, 0), an <- c(an, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$anniversary <- an

an <- NULL
for(i in data$anniversary){
  ifelse(i == "평일", an <- c(an, 0), an <- c(an, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$anniversary <- an

ba <- NULL
for(i in shop$before_anniversary){
  ifelse(i == "평일", ba <- c(ba, 0), ba <- c(ba, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$before_anniversary <- ba

ba <- NULL
for(i in data$before_anniversary){
  ifelse(i == "평일", ba <- c(ba, 0), ba <- c(ba, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$before_anniversary <- ba

aa <- NULL
for(i in shop$after_anniversary){
  ifelse(i == "평일", aa <- c(aa, 0), aa <- c(aa, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
shop$after_anniversary <- aa

aa <- NULL
for(i in data$after_anniversary){
  ifelse(i == "평일", aa <- c(aa, 0), aa <- c(aa, 1)) #휴일이 아닐 때는 0, 휴일일 때에는 1 
}
data$after_anniversary <- aa

ra <- NULL
for(i in shop$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1)) #비가 올 때는 0, 비가 오지 않을 때는 1 
}
shop$rain <- ra

ra <- NULL
for(i in data$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1)) #비가 올 때는 0, 비가 오지 않을 때는 1 
}
data$rain <- ra

fo <- NULL
for(i in shop$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1)) #안개가 낄 때는 0, 안개가 없을 때는 1 
}
shop$fog <- fo

fo <- NULL
for(i in data$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1)) #안개가 낄 때는 0, 안개가 없을 때는 1 
}
data$fog <- fo


#####################################################결정계수 확인 후 MAPE 

zb <- lm(shop$class_orders ~ shop$anniversary + shop$before_anniversary + shop$after_anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate + shop$football_national_cnt + shop$naverTrend_Shop_lastweek)
summary(zb)
zb$coefficients
y <- data$class_orders
ys <- 19.919509385*data$anniversary + (-4.933297588)*data$before_anniversary + (-2.674301833)*data$after_anniversary + 14.900178611*data$isHoliday + ( -27.547540388)*data$before_isHoliday + 1.441272846*data$after_isHoliday + 32.954609952*data$holiday_order + (-3.614206340)*data$rain + 1.962483934*data$fog + 0.356666816*data$prediction_orders + 0.245856122*data$ES_orders_25 + (-0.006439816)*data$ES_orders_75 + 0.059898811*data$importChangeRate + 1.537331970*data$football_national_cnt +  1.433045617*data$naverTrend_Shop_lastweek + 22.673998157 

mape <- sum((abs(y - ys)/y)) / length(data$after_isHoliday) *100
mape
