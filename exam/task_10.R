# Task 1
p = Poverty
for (i in 1:ncol(p))
{
  boxplot(p[,i], xlab=colnames(p)[i])
}

#p[,'N_EMPLD'] = ifelse(p[,'N_EMPLD'] < 6000, x, NA)
p[,2] = ifelse(p[,2] > 10000, NA, p[,2])
p[,1] = ifelse(p[,1] > 30, NA, p[,1])

boxplot(p)

plot(p)
for (i in 1:ncol(p))
{
  plot(p[,i], p[, 'PT_POOR'],  xlab=colnames(p)[i])
}
['POP_CHNG', 'PT_PHONE', 'PT_RURAL']

round(cor(p, use="pairwise.complete.obs"), 2)

alpha = 0.05
k = 3
n = 7
m = matrix(rep(0, n**2), n)
s = names(p)
rownames(m) = s
colnames(m) = s

for (i in 2:n)
{
  for(j in 1:(i-1))
  {
    if(i != k & j != k)
    {
      r = cor.test(p[, i], p[, j])
      m[i, j] = r$p.value < alpha
    }
  }
}

g = graph_from_adjacency_matrix(m, mode="undir")
plot(g)

summary(lm(p[,3]~., data=p[,-3]))
# Task 2
round(cor(p, use="pairwise.complete.obs"), 2)

for (j in c(1, 2, 5, 6))
{
  print(j)
  print(summary(lm(p[,j]~., data=p[,-c(j, 3, 4, 7)])))
}


# Task 3
m = lm(p[,3]~., data=p[,-3])
plot(m)

plot(m$residuals[1:length(m$residuals) - 1], m$residuals[2:length(m$residuals)])

dwtest(m, alternative = "greater")
dwtest(m, alternative = "less")

# Task 4
d = Job_prof

x = d[, 4]
y = d[, 5]

plot(x ,y)
lines(loess.smooth(x, y), col="magenta", lwd=3)

x2 = x**2

m = lm(y~x + x2)
m = lm(y~x2)
m = lm(y~x)
summary(m)

# Task 5
p = Poverty
x = p[, -3]
y = p[, 3]

x = x[-25,]
y = y[-25]
m = lm(y~., data=x)

stepAIC(m)

n = 29
stepAIC(m, k=log(n))


# Task 1
d = Dog_Wolf

plot(d, col=d$TYPE)
scatter3D(d$X5, d$X6, d$X4, colvar = d$TYPE, type="h", phi=30, theta = 20, d=5, bty="g", pch=20)


# classification
train = d[1:40, -7]
test = d[41:43, -7]

clf = factor(c(rep("dog", 30), rep("wolf", 10)))
clf

r = knn(train, test, clf, k=10, prob = TRUE)
r

# 2d densety
n = 100
x = rnorm(n)
y = x + rnorm(n)

plot(y~x)

z = kde2d(x, y, h=2)
image(z)
image(z, zlim = c(0, 0.05))

contour(z, col = "red", drawlabels = FALSE)

persp(z, phi=30, theta = 20, d=5)

z = kde2d(x, y, n=50, h=1)
persp(z, phi=30, theta = 20, d=5, col="yellow", shade=.5)

# diskriminantniy analisys
s=t(spanish)
s=s[order(rownames(s)),]

s[1:5,1:4]
p=prcomp(s,scale=TRUE)
summary(p)
d=data.frame(p$x)
m=lda(d[,1:8], spanishMeta$Author)
?lda

plot(m)
round(predict(m)$posterior, 4)


# Task 2
b = BankLoan
b = b[-445,]

m = glm(b[,9]~., b[,-9], family = 'binomial')
m
plot(m)


# Lesson 10
# Task 1
d = Kruskal
boxplot(PERFRMNC~CONDITN, data=d)

## Exam
# Task 2
n = 100
u = Uniform[, 1]

x = ifelse(u < 0.5, sqrt(2 * u) - 1, u - 0.5)
sum(ifelse(x > 1/4, 1, 0)) / n


# Task 3
x = 1 / (1 - u) - 1
boxplot(x)
b=quantile(x, 0.75) + 3*IQR(x)

q=ifelse(x>b, NA, x)
boxplot(q)
sum(is.na(q))

log_q = log(q)
qqnorm(log_q)
qqline(log_q)
sf.test(log_q)

# Task 4
c = CarSales
nrow(c)

boxplot(c)
c = c[-54, ]
nrow(c)

boxplot(c[c[, "Type"] == 0, "Miles_gal"])
boxplot(c[c[, "Type"] == 1, "Miles_gal"])
plot(ecdf(c[c[, "Type"] == 0, "Miles_gal"]))
plot(ecdf(c[c[, "Type"] == 1, "Miles_gal"]), add=T, col=3)

wilcox.test(c[c[, "Type"] == 1, "Miles_gal"], c[c[, "Type"] == 0, "Miles_gal"], alternative = "less")
ks.test(c[c[, "Type"] == 0, "Miles_gal"], c[c[, "Type"] == 1, "Miles_gal"], alternative = "less")

# Task 5
c = Credit
c[c[, "numcred"] == 1, "credhist"]


nu1 = 1:4
x = c[c[, "numcred"] == 1, "credhist"]
nu1[1] = sum(ifelse(x <= 2, 1, 0))
for (i in 2:4) {
  nu1[i] = sum(ifelse(x == i+1, 1, 0))
}
nu1

nu2 = 1:4
x = c[c[, "numcred"] == 2, "credhist"]
nu2[1] = sum(ifelse(x <= 2, 1, 0))
for (i in 2:4) {
  nu2[i] = sum(ifelse(x == i+1, 1, 0))
}
nu2
chisq.test(nu1, nu2)
m = matrix(c(nu1, nu2), nrow=2, ncol=4, byrow=T)
chisq.test(m)
m
