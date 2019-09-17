confint <- function(x, alpha = .05) { # alpha : 제 1종 오류가 일어날 확률
	conflevel = (1 - alpha)*100

	stderr <- sd(x)/sqrt(length(x))
	tcrit <- qt(1-alpha/2, length(x)-1)
	margin <- stderr * tcrit       # 오차 한계

	lower <- mean(x) - margin  # 신뢰 하한값(lower confidence limit)
	upper <- mean(x) + margin # 신뢰 상한값(upper confidence limit)

	cat(conflevel,"Percent Confidence Interval","\n")
	cat("Mean:", mean(x), "Std. Error:", stderr,"\n")
	cat("Lower Limit:", lower, "\n")
	cat("Upper Limit:", upper, "\n")
}