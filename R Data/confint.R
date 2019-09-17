confint <- function(x, alpha = .05) { # alpha : �� 1�� ������ �Ͼ Ȯ��
	conflevel = (1 - alpha)*100

	stderr <- sd(x)/sqrt(length(x))
	tcrit <- qt(1-alpha/2, length(x)-1)
	margin <- stderr * tcrit       # ���� �Ѱ�

	lower <- mean(x) - margin  # �ŷ� ���Ѱ�(lower confidence limit)
	upper <- mean(x) + margin # �ŷ� ���Ѱ�(upper confidence limit)

	cat(conflevel,"Percent Confidence Interval","\n")
	cat("Mean:", mean(x), "Std. Error:", stderr,"\n")
	cat("Lower Limit:", lower, "\n")
	cat("Upper Limit:", upper, "\n")
}