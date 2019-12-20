# Task 1
x = CarSales[, "Miles_gal"]
y = CarSales[, "Sales"]

hist(y)

y_log = log(y)
hist(y_log)

plot(x, y_log)

x = ifelse(x<40, x, NA)
y_log = ifelse(y_log>-2, y_log, NA)
plot(x, y_log)

cor(x, y_log, method = "pearson", use = "pairwise.complete.obs")
cor.test(x, y_log)


z = CarSales[, 'Type']
z = ifelse(z == 0, 4, z)
z = ifelse(z == 1, 2, z)

m = cbind(x, y)

plot(m, col=z)

cor.test(x[z==4], y_log[z==4])
cor.test(x[z==2], y_log[z==2])

cor.test(x[z==4], y_log[z==4], method = "spearman")


d = Dog_Wolf
p = prcomp(d)
summary(p)


m = as.matrix(Dog_Wolf[,-7])
c = prcomp(m)$rotation
y = m %*% c
plot(y[, 1:2], type="n")
text(y[, 1:2], cex=0.7)

# Task 2
a = AffixProd
m = ncol(a)
d = a[,-c(1, m-2, m-1, m)]

p = prcomp(d)
summary(p)

round(p$rotation[,1:3], 3)

# Task 3
d = Dative
t = table(d[,1], d[,3])
t

chisq.test(t)


# Task 1
p = Poverty
for (i in 1:ncol(p))
{
  boxplot(p[,i], xlab=colnames(p)[i])
}

p[,'N_EMPLD'] = ifelse(p[,'N_EMPLD'] < 6000, x, NA)
boxplot(p)

plot(p)
for (i in 1:ncol(p))
{
  plot(p[,i], p[, 'PT_POOR'],  xlab=colnames(p)[i])
}
['POP_CHNG', 'PT_PHONE', 'PT_RURAL']

round(cor(p, use="pairwise.complete.obs"), 2)
