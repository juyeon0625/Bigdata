shop <- read.csv("shops_training27.csv", header=TRUE, skip = 1)
data <- read.csv("shops_test27.csv", header=TRUE, skip = 1)
attach(shop)
attach(data)

################################################TRUE, FALSE
isH <- NULL
for(i in shop$isHoliday){
  ifelse(i == "FALSE", isH <- c(isH, 0), isH <- c(isH, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$isHoliday <- isH #0?? 1?? ??��?? isHoliday?? shop?? ?ִ? isHoliday?? ??�� 

isH <- NULL
for(i in data$isHoliday){
  ifelse(i == "FALSE", isH <- c(isH, 0), isH <- c(isH, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$isHoliday <- isH  

before <- NULL
for(i in shop$before_isHoliday){
  ifelse(i == "FALSE", before <- c(before, 0), before <- c(before, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$before_isHoliday <- before 

before <- NULL
for(i in data$before_isHoliday){
  ifelse(i == "FALSE", before <- c(before, 0), before <- c(before, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$before_isHoliday <- before

after <- NULL
for(i in shop$after_isHoliday){
  ifelse(i == "FALSE", after <- c(after, 0), after <- c(after, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$after_isHoliday <- after 

after <- NULL
for(i in data$after_isHoliday){
  ifelse(i == "FALSE", after <- c(after, 0), after <- c(after, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$after_isHoliday <- after

an <- NULL
for(i in shop$anniversary){
  ifelse(i == "????", an <- c(an, 0), an <- c(an, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$anniversary <- an

an <- NULL
for(i in data$anniversary){
  ifelse(i == "????", an <- c(an, 0), an <- c(an, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$anniversary <- an

ba <- NULL
for(i in shop$before_anniversary){
  ifelse(i == "????", ba <- c(ba, 0), ba <- c(ba, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$before_anniversary <- ba

ba <- NULL
for(i in data$before_anniversary){
  ifelse(i == "????", ba <- c(ba, 0), ba <- c(ba, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$before_anniversary <- ba

aa <- NULL
for(i in shop$after_anniversary){
  ifelse(i == "????", aa <- c(aa, 0), aa <- c(aa, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
shop$after_anniversary <- aa

aa <- NULL
for(i in data$after_anniversary){
  ifelse(i == "????", aa <- c(aa, 0), aa <- c(aa, 1)) #?????? ?ƴ? ???? 0, ?????? ?????? 1 
}
data$after_anniversary <- aa

ra <- NULL
for(i in shop$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1)) #???? ?? ???? 0, ???? ???? ??�� ???? 1 
}
shop$rain <- ra

ra <- NULL
for(i in data$rain){
  ifelse(i == "TRUE", ra <- c(ra, 0), ra <- c(ra, 1)) #???? ?? ???? 0, ???? ???? ??�� ???? 1 
}
data$rain <- ra

fo <- NULL
for(i in shop$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1)) #?Ȱ??? ?? ???? 0, ?Ȱ??? ??�� ???? 1 
}
shop$fog <- fo

fo <- NULL
for(i in data$fog){
  ifelse(i == "TRUE", fo <- c(fo, 0), fo <- c(fo, 1)) #?Ȱ??? ?? ???? 0, ?Ȱ??? ??�� ???? 1 
}
data$fog <- fo


CSI <- NULL
for(i in shop$currentFamilySavingCSI){
  if(i == "?")
    CSI <- c(CSI, 80)
}
shop$currentFamilySavingCSI <- CSI

attach(shop)
shop$currentFamilySavingCSI[1:181] <- c(80)
mean(currentFamilySavingCSI)

class(currentFamilySavingCSI[1:181])


#####################################################??��???? Ȯ?? ?? MAPE  

a <- lm(shop$class_orders ~ shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday)
a$coefficients
summary(a)
y <- data$class_orders
ys <- 108.2336711*data$isHoliday + 0.5485016*data$before_isHoliday + -2.6929853*data$after_isHoliday + 92.1004463


b <- lm(shop$class_orders ~ shop$isHoliday)
b$coefficients
summary(b)
y <- data$class_orders
ys <- 107.60496*data$isHoliday + 91.63177 


c <- lm(shop$class_orders ~ shop$before_isHoliday)
c$coefficients
summary(c)

d <- lm(shop$class_orders ~ shop$after_isHoliday)
d$coefficients
summary(d)

e <- lm(shop$class_orders ~ shop$isHoliday + shop$before_isHoliday)
e$coefficients
summary(e)
y <- data$class_orders
ys <- 107.060279*data$isHoliday + 1.841793*data$before_isHoliday + 91.229249


f <- lm(shop$class_orders ~ shop$isHoliday + shop$after_isHoliday)
f$coefficients
summary(f)
y <- data$class_orders
ys <- 108.473240*data$isHoliday + (-2.956378)*data$after_isHoliday + 92.277885


g <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday)
summary(g)
g$coefficients
ys <- 28.961735*data$anniversary + 104.620963*data$isHoliday +  1.042206*data$before_isHoliday + (-2.461102)*data$isHoliday + 91.675922 


h <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday)
summary(h)
h$coefficients
ys <- 28.89338*data$anniversary + 104.21439*data$isHoliday + 91.36645 


i <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order)
summary(i)
i$coefficients
ys <-  26.127591*data$anniversary + 65.525543*data$isHoliday + (-7.543070)*data$before_isHoliday +  3.233283*data$after_isHoliday + 25.675340*data$holiday_order + 92.333749


j <- lm(shop$class_orders ~ shop$anniversary + shop$before_anniversary + shop$after_anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday)
summary(j)
j$coefficients
ys <-  26.9317060*data$anniversary +  10.0278191*data$before_anniversary + (-1.3422496)*data$after_anniversary +  105.1239988*data$isHoliday + (-0.4749236)*data$before_isHoliday + (-2.9882840)*data$after_isHoliday +  91.8431980


k <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain)
summary(k)
k$coefficients
ys <- 26.446241*data$anniversary + 65.428789*data$isHoliday + (-7.576641)*data$before_isHoliday + 3.223255*data$after_isHoliday + 25.704864*data$holiday_order + (-2.636933)*data$rain + 94.308970


l <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog)
summary(l)
l$coefficients
ys <- 26.300503*data$anniversary + 65.270289*data$isHoliday + (-7.613684)*data$before_isHoliday + 3.268856*data$after_isHoliday + 25.730693*data$holiday_order + (-2.833888)*data$rain + 2.913745*data$fog + 92.772514


m <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$temp_highest)
summary(m)
m$coefficients
ys <- 25.4426694*data$anniversary + 66.3032658*data$isHoliday + (-7.5912353)*data$before_isHoliday +  2.9333900*data$after_isHoliday + 25.1115714*data$holiday_order + (-4.7708310)*data$rain + 1.8574843*data$fog + ( -0.4048513)*data$temp_highest + 101.5412932


n <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$LYSM_similarDates_avg_orders)
summary(n)
n$coefficients
ys <- 25.9554477*data$anniversary + 41.7855024*data$isHoliday + (-5.5273082)*data$before_isHoliday + 1.1179661*data$after_isHoliday + 22.5888326*data$holiday_order + (-2.7891570)*data$rain + 2.9322105*data$fog + 0.2452453*data$LYSM_similarDates_avg_orders + 69.7355275


o <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders)
summary(o)
o$coefficients
ys <- 18.2514408*data$anniversary +  13.8802935*data$isHoliday + (-3.4624578)*data$before_isHoliday + -0.7449316*data$after_isHoliday + 26.0072891*data$holiday_order + (-3.5268940)*data$rain + 2.3812831*data$fog + 0.4603794*data$prediction_orders + 51.2027124


p <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25)
summary(p)
p$coefficients
ys <- 17.2103564*data$anniversary +  16.7405728*data$isHoliday + (-30.7602367)*data$before_isHoliday + 0.7395809*data$after_isHoliday + 32.8489519*data$holiday_order + (-3.7137778)*data$rain + 2.0149099*data$fog + 0.3750867*data$prediction_orders +  0.2715814*data$ES_orders_25 + 32.2885571


q <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75)
summary(q)
q$coefficients
ys <- 16.9621135*data$anniversary + 19.9305104*data$isHoliday + (-26.5508045)*data$before_isHoliday + 1.4942947*data$after_isHoliday + 31.7112726*data$holiday_order + (-3.9398858)*data$rain + 2.0697596*data$fog + 0.3651464*data$prediction_orders + 0.1961108*data$ES_orders_25 + 0.1174306*data$ES_orders_75 + 26.4701654


r <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$similarDates_movingAvg_orders)
summary(r)
r$coefficients
ys <- 5.2726086*data$anniversary + 4.9724690*data$isHoliday + (-22.8941879)*data$before_isHoliday + 0.5912379*data$after_isHoliday + 28.5176677*data$holiday_order + (-3.8779958)*data$rain + 2.3515798*data$fog + 0.2976079*data$prediction_orders + 0.1590234*data$ES_orders_25 + 0.1787182*data$ES_orders_75 + 0.2543212*data$similarDates_movingAvg_orders + 4.3392055


s <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$underlyingInflationRate)
summary(s)
s$coefficients
ys <- 16.9769312*data$anniversary + 19.7972840*data$isHoliday + (-26.4036562)*data$before_isHoliday + 1.4690305*data$after_isHoliday + 31.6804651*data$holiday_order + (-3.9350663)*data$rain +  1.9066905*data$fog + 0.3664634*data$prediction_orders + 0.1957550*data$ES_orders_25 + 0.1141351*data$ES_orders_75 + (-0.9593312)*data$underlyingInflationRate + 29.8580421


t <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate)
summary(t)
t$coefficients  
ys <- 17.13812199*data$anniversary + 20.25437983*data$isHoliday + (-26.34428215)*data$before_isHoliday + 1.29474252*data$after_isHoliday + 31.49200330*data$holiday_order + (-3.74704372)*data$rain + 1.90610574*data$fog + 0.36339986*data$prediction_orders + 0.20040653*data$ES_orders_25 + 0.09424125*data$ES_orders_75 + 0.06011241*data$importChangeRate + 28.20617541


u <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate + shop$football_national_cnt)
summary(u)
u$coefficients
ys <- 18.06059616*data$anniversary + 16.83116453*data$isHoliday + (-26.91202073)*data$before_isHoliday + 1.46722799*data$after_isHoliday + 32.30422840*data$holiday_order + (-3.36256401)*data$rain + 1.92472654*data$fog + 0.36026108*data$prediction_orders + 0.20492945*data$ES_orders_25 + 0.09937599*data$ES_orders_75 +  0.05956079*data$importChangeRate + 1.39352644*data$football_national_cnt + 27.01522714


v <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate + shop$football_national_cnt + shop$naverTrend_Shop_lastweek)
summary(v)
v$coefficients
ys <- 18.342715020*data$anniversary +  15.964618120*data$isHoliday + (-27.708177138)*data$before_isHoliday +  0.590411652*data$after_isHoliday + 32.404692176*data$holiday_order + (-3.656400958)*data$rain + 1.890066514*data$fog + 0.356823531*data$prediction_orders + 0.240961507*data$ES_orders_25 + (-0.009706336)*data$ES_orders_75 +  0.061709594*data$importChangeRate + 1.581672611*data$football_national_cnt + 1.450163527*data$naverTrend_Shop_lastweek + 23.508274481


z <- lm(shop$class_orders ~ shop$anniversary + shop$before_anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate + shop$football_national_cnt + shop$naverTrend_Shop_lastweek)
summary(z)
z$coefficients
ys <- 19.27190494*data$anniversary + (-4.57104528)*data$before_anniversary + 15.03283819*data$isHoliday + (-27.75793275)*data$before_isHoliday +  0.99292100*data$after_isHoliday + 32.89388227*data$holiday_order + (-3.64009384)*data$rain + 1.97860539*data$fog + 0.35754019*data$prediction_orders + 0.24808175*data$ES_orders_25 + ( -0.01114583)*data$ES_orders_75 +  0.06030160*data$importChangeRate + 1.54106717*data$football_national_cnt + 1.43336038*data$naverTrend_Shop_lastweek + 22.95752073


zb <- lm(shop$class_orders ~ shop$anniversary + shop$before_anniversary + shop$after_anniversary + shop$isHoliday + shop$before_isHoliday + shop$after_isHoliday + shop$holiday_order + shop$rain + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$ES_orders_75 + shop$importChangeRate + shop$football_national_cnt + shop$naverTrend_Shop_lastweek)
summary(zb)
zb$coefficients
y <- data$class_orders
ys <- 19.919509385*data$anniversary + (-4.933297588)*data$before_anniversary + (-2.674301833)*data$after_anniversary + 14.900178611*data$isHoliday + ( -27.547540388)*data$before_isHoliday + 1.441272846*data$after_isHoliday + 32.954609952*data$holiday_order + (-3.614206340)*data$rain + 1.962483934*data$fog + 0.356666816*data$prediction_orders + 0.245856122*data$ES_orders_25 + (-0.006439816)*data$ES_orders_75 + 0.059898811*data$importChangeRate + 1.537331970*data$football_national_cnt +  1.433045617*data$naverTrend_Shop_lastweek + 22.673998157 


za <- lm(shop$class_orders ~ shop$anniversary + shop$isHoliday + shop$after_isHoliday + shop$holiday_order + shop$fog + shop$prediction_orders + shop$ES_orders_25 + shop$importChangeRate + shop$football_national_cnt + shop$naverTrend_Shop_lastweek)
summary(za)
za$coefficients
ys <- 19.33544390*data$anniversary + 20.03910406*data$isHoliday + 1.78961250*data$after_isHoliday + 22.36602903*data$holiday_order + 1.79813796*data$fog +  0.41035730*data$prediction_orders + 0.04774901*data$ES_orders_25 + 0.09003512*data$importChangeRate + 1.43505062*data$football_national_cnt + 1.94748697*data$naverTrend_Shop_lastweek +  26.53140078

mape <- sum((abs(y - ys)/y)) / length(data$after_isHoliday) *100
mape
zb 13.49034