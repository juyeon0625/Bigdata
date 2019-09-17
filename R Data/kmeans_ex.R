plot(x, col=cl$cluster)
points(cl$clenters, col=1:5, pch=8)
require(graphics)
x=rbind(matrix(rnorm(100, mean=0, sd=0.3), ncol=2),
        matrix(rnorm(100, mean=1, sd=0.3), ncol=2))
colnames(x) = c("x", "y")

###########################################
cl1 = kmeans(x, 2)

plot(x, col=cl1$cluster)
points(cl1$clenters, col=1:2, pch=18, cex=5)

###########################################
cl2 = kmeans(x, 5, nstart=25)

plot(x, col=cl2$cluster)
points(cl2$clenters, col=1:5, pch=8)