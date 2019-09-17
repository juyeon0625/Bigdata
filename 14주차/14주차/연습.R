a <- c(2, 1, 5, 2, 3, 4, 4, 1, 3, 5)
b <- c(4, 5, 3, 2, 5, 3, 4, 2, 3, 5)
c <- c(5, 1, 4, 3, 5, 2, 3, 1, 2, 3)
noo <- data.frame(a, b, c)

sl=scale(noo) # standardize noo


eg=eigen(cor(sl)) # eigen value, vector
sc=sl%*%eg$vectors[,1:2]
rownames(eg$vectors)=c("면", "그릇", "국물")
biplot(-sc, -eg$vectors[,1:2])
############################
n = princomp(noo, cor=TRUE) #표준화 내장

screeplot(n, npcs=4, type="lines")
summary(n)
loadings(n)
n$scores
biplot(n)


