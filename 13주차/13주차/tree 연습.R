install.packages("tree")
library(tree) # 주어진 패키지 설치 후 시작

data(iris)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
plot(trainData[, 3],trainData[, 4], type="n", xlab="petal length",
     ylab="petal width")
text(trainData[, 3], trainData[, 4], c("s", "c", "v")[trainData[, 5]])
plot(testData[, 3],testData[, 4], type="n", xlab="petal length",
     ylab="petal width")
text(testData[, 3], testData[, 4], c("s", "c", "v")[testData[, 5]])


myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
ir.tr = tree(myFormula, trainData)
summary(ir.tr)
ir.tr
plot(ir.tr)
text(ir.tr, all = T)
partition.tree(ir.tr, add = TRUE, cex = 1.5)
plot(prune.misclass(ir.tr))
fin.tr = prune.misclass(ir.tr, best=4) #오분류율에 의한 가지치기
plot(fin.tr)
text(fin.tr, all=T)
testPred <- predict(fin.tr, newdata=testData)
pred = c("init") #선언 및 초기화
for(i in 1:(length(t(testPred))/3)) {
  m = max(testPred[i,])
  w = which(testPred[i,]==m)
  if(w==1) pred[i] = c("setosa")
  else if(w==2) pred[i] = c("versicolor")
  else if(w==3) pred[i] = c("virginica")
}
table(pred, testData$Species)


data(iris)
summary(iris)
install.packages("party")
library(party)
data(iris)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7 ,0.3))
trainData <- iris[ind==1 ,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
table(predict(iris_ctree,trainData), trainData$Species)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")
testPred <- predict(iris_ctree, newdata=testData)
table(testPred, testData$Species)



############################################# 불순도 -> 엔트로피 지수
############################################# I(S)

IS <- function(x){
  i <- 1:18
  length(x)
  
  p <- table(range) / length(x)
  
  s <- NULL
  s <- p[i] * (-log2(p[i]))
  
  return(sum(s))
  
}
IS(data1$class_orders)


############################################# E(A)

EA <- function(x){
  
}





data(iris)
summary(iris)
install.packages("party")
library(party)
data(iris)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7 ,0.3))
trainData <- iris[ind==1 ,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
table(predict(iris_ctree,trainData), trainData$Species)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")
testPred <- predict(iris_ctree, newdata=testData)
table(testPred, testData$Species)
