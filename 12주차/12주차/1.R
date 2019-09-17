word1 <- c(0.124, 0.0194, 0.0082, 0.0087, 0.0093, 0.0185, 0.0028)
word2 <- c(0.275, 0.0043, 0.0032, 0.0174, 0.0061, 0.0249, 0.0003)
word3 <- c(0.019, 0.0054, 0.0007, 0.0091, 0.0172, 0.0084, 0.0202)
word4 <- c(0.182, 0.0155, 0.0104, 0.0086, 0.0028, 0.0167, 0.0083)
word5 <- c(0.223, 0.0028, 0.0073, 0.0268, 0.0009, 0.0193, 0.0054)

data <- data.frame(rbind(word1, word2, word3, word4, word5))
colnames(data) = c("a", "doc1", "doc2", "doc3", "doc4", "doc5", "doc6")

attach(data)

###############코사인 유사성 척도 

up1 <- sum(a * doc1)
up2 <- sum(a * doc2)
up3 <- sum(a * doc3)
up4 <- sum(a * doc4)
up5 <- sum(a * doc5)
up6 <- sum(a * doc6)

down1 <- sqrt(sum(a^2)) * sqrt(sum(doc1^2))
down2 <- sqrt(sum(a^2)) * sqrt(sum(doc2^2))
down3 <- sqrt(sum(a^2)) * sqrt(sum(doc3^2))
down4 <- sqrt(sum(a^2)) * sqrt(sum(doc4^2))
down5 <- sqrt(sum(a^2)) * sqrt(sum(doc5^2))
down6 <- sqrt(sum(a^2)) * sqrt(sum(doc6^2))

u1 <- up1 / down1
u2 <- up2 / down2
u3 <- up3 / down3
u4 <- up4 / down4
u5 <- up5 / down5
u6 <- up6 / down6

u1
u2
u3
u4
u5
u6