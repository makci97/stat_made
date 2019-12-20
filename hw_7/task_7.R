# Task 1
n = 1000000
k = 16
count = 0
for (i in 1:n){
  point = runif(k, min=-1, max=1)
  count = count + ifelse(sum(point**2) < 1, 1, 0)
}
count

p = pi^(k / 2)/(2^(k)* factorial(k/2))
# binom p(S > 8)
1 - pbinom(8, size=n, prob=p)

# pois p(S > 8)
lambda = n * p
1 - ppois(8, lambda)


# Task 2
d = CarSales[, c("Price", "Engine_s")]
n = nrow(d)
n

# N3 standartization
max(d[,"Price"])
max(d[,"Engine_s"])
for (i in 1:ncol(d)){
  d[i] = (d[,i] - median(d[,i]))/mad(d[,i])
}
max(d[,"Price"])
max(d[,"Engine_s"])

# sum of dist to k nearest points
k = 10
sum_k_dists = 1:n
for (i in 1:n)
{
  t = rep(Inf, k)
  for(j in 1:n)
  {
    if (i != j)
    {
      dist = sqrt(sum((d[i,] - d[j,])**2))
      if (dist < t[k])
      {
        t[k] = dist
        t = sort(t)
      }
    }
  }
  sum_k_dists[i] = sum(t)
}
sum_k_dists

# threshold for far points
alpha = 0.05
b = quantile(sum_k_dists, 1 - alpha)

# find far points
is_far = ifelse(sum_k_dists < b, F, T)
!is_far

# plot
plot(d[!is_far,], xlim=c(-2, 8), ylim=c(-3, 6))
par(new=T)
plot(d[is_far,], col=2, xlim=c(-2, 8), ylim=c(-3, 6))
sum(is_far)



# Task 3
d = CarsChar
z = CarsChar
ncol(d)

for (i in 2:ncol(d)){
  z[i] = (d[,i] - median(d[,i]))/mad(d[,i])
}
max(d[,"PRICE"])
max(z[,"PRICE"])

boxplot(z[, 2])
boxplot(z[, 3])
boxplot(z[, 4])
boxplot(z[, 5])
boxplot(z[, 6])

z[,2] = ifelse(z[,2] > 10, NA, z[,2])
z[,4] = ifelse(z[,4] < -15, NA, z[,4])
z[,5] = ifelse(z[,5] < -10, NA, z[,5])
z[,6] = ifelse(z[,6] > 2, NA, z[,6])

plot(z[2:ncol(z)])
plot(z[, c("ACCELER", "BRAKING")])
text(z[, c("ACCELER", "BRAKING")], labels=z[,1])

b = CarsChar
for (i in 2:ncol(d)){
  b[i] = (d[,i] - min(na.omit(d[,i])))/(max(na.omit(d[,i])) - min(na.omit(d[,i])))
}
max(b[,"PRICE"])

barplot(t(b[4, 2:ncol(b)])[,1])


# Task 4
s = Simpl
plot(s)

k = 5
r = kmeans(s, k, nstart=10)
plot(s, col=r$cluster)

r$betweenss/r$totss

plot(1:ncol(s), r$centers[1,], type="l", col=1, ylim=c(min(r$centers), max(r$centers)))
for (i in 2:nrow(r$centers)){
  lines(1:ncol(s), r$centers[i,], type="l", col=i)
}
legend("topleft", legend=1:k, lty=rep(1, k), col=1:k)

dist(r$centers)

# Task 5
dist(s)
r = hclust(dist(s), method = "complete")
plot(r)

clust = cutree(r, k=5)
plot(s, col=clust)

plot(r$height, type='l')



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
