head(gssdata)
attach(gssdata)

r <- rincome
s <- satjob1
j <- jobsecok
a <- age

rs <- sd(r)^2
ss <- sd(s)^2
js <- sd(j)^2
as <- sd(a)^2

in_r_s <- rs + ss
in_r_j <- rs + js
in_a_s <- as + ss
in_a_j <- as + js

rm <- mean(r)
sm <- mean(s)
jm <- mean(j)
am <- mean(a)
all_sum <- rm + sm + jm + am
mm <- all_sum/4

out_r_s <- length(r) * (rm - mm)^2 
          +length(s) * (sm - mm)^2
out_r_j <- length(r) * (rm - mm)^2
          +length(j) * (jm - mm)^2
out_a_s <- length(a) * (am - mm)^2
          +length(s) * (sm - mm)^2
out_a_j <- length(a) * (am - mm)^2
          +length(j) * (jm - mm)^2

cor_ratio_r_s <- out_r_s / (in_r_s + out_r_s)
cor_ratio_r_j <- out_r_j / (in_r_j + out_r_j)
cor_ratio_a_s <- out_a_s / (in_a_s + out_a_s)
cor_ratio_a_j <- out_a_j / (in_a_j + out_a_j)
