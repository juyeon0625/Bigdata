?cor
gov <- c(1, 2, 3, 4, 5)
x <- c(1, 2, 3, 4, 5)
y <- c(5, 3, 1, 4, 2)

cor.test(gov, x, method="spearman")
cor.test(gov, y, method="spearman")
cor.test(x, y,method = "spearman")

