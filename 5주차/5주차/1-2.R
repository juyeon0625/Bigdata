head(gssdata)
summary(gssdata)
cor(gssdata,(,1:5))

scale(gssdata)

돈은 많이 받는 사람이 적음
나이는 19세부터 88세까지 있으며 대부분 30에서 50정도 사이에 있음
직업만족도는 1부터 4까지 있으며 중간값이 2이며 불만족스러운것이 더 많다
일한기간은 
안전도 1이 제일 안전하며 거의 안전하다고 나옴

attach(gssdata)
min(rincome)
mean(rincome)
median(rincome)
mode(rincome)
max(rincome)
sort(rincome)

나이는 20대중반부터 60대까지 분포해있는데 40대초반이 제일 많ㅇ
직업만족도는 대체적으로 만족하고 있다.
대부분 2년3년내에 퇴직한다

pairs(gssdata)
plot(satjob1,jobsecok)
plot(jobsecok, satjob1)
plot(satjob1, jobsecok, abline(lm(jobsecok~satjob1)))
jitter(gssdata)
jitter(satjob1, jobsecok)
jitter(gssdata)

attach(gssdata)
gssdata_ji = data.frame(jitter(satjob1),jitter(jobsecok))
plot(gssdata_ji)
