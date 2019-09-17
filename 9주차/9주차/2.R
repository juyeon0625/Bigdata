options(digits = 5)
height <- rnorm(500000, 171, 15)
weight <- rnorm(500000, 70, 20)

plot(height, weight, main = "키와 몸무게 산포도")
cor(weight, height)

height <- sample(height, 2000)
weight <- sample(weight, 2000)
plot(height, weight, main = "키와 몸무게 산포도(2000 samples)")
cor(height, weight)

heights <- sort(height)
weights <- sort(weight)
plot(heights, weights, main = "키와 몸무게 산포도(sorted 2000 samples)")
cor(heights, weights)

plot(jitter(weights, 500), jitter(heights, 500), main = "키와 몸무게 산포도(sample + jitter)")
cor(heights, weights)


