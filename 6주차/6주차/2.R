shop <- read.csv("shop_training27.csv", header = TRUE, skip = 1)

summary(shop)
head(shop)
attach(shop)


anni <- NULL

for (i in anniversary){
  ifelse(i == "평일", anni <- c(anni, 0), anni <- c(anni, 1))  #평일일 때는 0, 국경일일 때는 1
}
shop$anniversary <- anni  #0과 1로 설정한 anniverary를 shop에 있는 anniversary에 설정 


isH <- NULL

for (i in isHoliday){
  ifelse(i == "FALSE", isH <- c(isH, 0), isH <- c(isH, 1) )  #휴일이 아닐 때는 0, 휴일일 때에는 1
}
shop$isHoliday <- isH  #0과 1로 설정한 isHoliday를 shop에 있는 isHoliday에 설정


before <- NULL

for (i in before_isHoliday){
  ifelse(i == "FALSE", before <- c(before, 0), before <- c(before, 1))  #휴일 전 날이 아닐 때는 0, 휴일 전 날일 때는 1
}
shop$before_isHoliday <- before  #0과 1로 설정한 before_isHoliday를 shop에 있는 before_isHoliday에 설정


after <- NULL

for (i in after_isHoliday){
  ifelse(i == "FALSE", after <- c(after, 0), after <- c(after, 1))  #휴일 다음 날이 아닐 때는 0, 휴일 다음 날일 때는 1
}
shop$after_isHoliday <- after


rainy <- NULL

for (i in rain){
  ifelse(i == "TRUE", rainy <- c(rainy, 1), rainy<- c(rainy, 0))  #비가 오는 날 1, 비가 오지 않는날 0
}
shop$rain <- rainy


foggy <- NULL

for (i in fog){
  ifelse(i == "TRUE", foggy <- c(foggy, 0), foggy <- c(foggy, 1))  #안개가 낀 날 0, 안개가 끼지 않은 날 1
}
shop$fog <- foggy

  


cor(isH, class_orders)
cor(before, class_orders)
cor(after, class_orders)
cor(rainy, class_orders)
cor(foggy, class_orders)







