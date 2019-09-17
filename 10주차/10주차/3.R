jc_ex <- (76*123)/250
wc_ex <- (104*123)/250
cc_ex <- (70*123)/250
jh_ex <- (76*127)/250
wh_ex <- (104*127)/250
ch_ex <- (70*127)/250

jc <- (43-jc_ex)^2 / jc_ex
wc <- (51-wc_ex)^2 / wc_ex
cc <- (29-cc_ex)^2 / cc_ex
jh <- (33-jh_ex)^2 / jh_ex
wh <- (53-wh_ex)^2 / wh_ex
ch <- (41-ch_ex)^2 / ch_ex

x2 <- jc + wc + cc + jh + wh + ch

v <- sqrt(x2 / (250 * (min(3, 2)-1)))




