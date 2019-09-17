head(gssdata)
summary(gssdata)

attach(gssdata)
gssdata_ji = data.frame(jitter(satjob1),jitter(jobsecok))
plot(gssdata_ji)