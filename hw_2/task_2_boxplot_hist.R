p = 1/sqrt(1-runif(50))
boxplot(p)
b=quantile(p, 0.75) + 1.5*IQR(p)

q=ifelse(p>b, NA, p)
boxplot(q)
sum(is.na(q))

hist(d[,'LogFrequency'], col=8)
hist(d[,'LogFrequency'], col=8, breaks = 20)

s=subset(d, LogFreq !=0 & SeC == "op")
mean(s[, 2])
