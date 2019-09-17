dice1 <- sample(1:6, 1000, replace=TRUE)
dice2 <- sample(1:6, 1000, replace=TRUE)
dice3 <- sample(1:6, 1000, replace=TRUE)
dice4 <- sample(1:6, 1000, replace=TRUE)
dice5 <- sample(1:6, 1000, replace=TRUE)
dice6 <- sample(1:6, 1000, replace=TRUE)

chisq.test(dice1, dice2)
chisq.test(dice1, dice3)
chisq.test(dice1, dice4)
chisq.test(dice1, dice5)
chisq.test(dice1, dice6)


