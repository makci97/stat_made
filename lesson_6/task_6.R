# xi^2
# Task 1
x = c(69971, 382, 1, 1, 0)
y = c(1093547, 2469, 153, 16, 0)
x[5] = 1000000 - sum(x)
y[5] = 18580121 - sum(y)

p = y / sum(y)
z = p * 1000000

chisq.test(x=x, p=p)


x = c(69971, 382, 1, 0)
y = c(1093547, 2469, 153, 0)
x[4] = 1000000 - sum(x)
y[4] = 18580121 - sum(y)

p = y / sum(y)
z = p * 1000000

chisq.test(x=x, p=p)

# Task 2
x = c(229, 211, 93, 35, 7, 0, 0, 1)
y = 0:7
lambda = sum(x/ sum(x) * y)

p = dpois(y, lambda)
p[8] = 1 - ppois(6, lambda)
sum(p)

z = p * sum(x)
z

barplot(x, col=4)
barplot(z, add=T, col=3, width=0.95)
?barplot

chisq.test(x=x, p=p)

# Task 3
x = c(229, 211, 93, 35, 8)
y = 0:4

p = dpois(y, lambda)
p[5] = 1 - ppois(3, lambda)
sum(p)

z = p * sum(x)
z

barplot(x, col=4)
barplot(z, add=T, col=3, width=0.95)

chisq.test(x, p=p)

1 - pchisq(1.0301, 3)

# max likelihood
lambda = seq(0.9, 1, 0.001)
n = length(lambda)
likelihood = 1:n
for (i in 1:n)
{
  p = dpois(y, lambda[i])
  p[5] = 1 - ppois(3, lambda[i])
  likelihood[i] = sum(x * log(p))
}
lambda = lambda[which.max(likelihood)]

p = dpois(y, lambda)
p[5] = 1 - ppois(3, lambda)
sum(p)

z = p * sum(x)
z

barplot(x, col=4)
barplot(z, add=T, col=3, width=0.95)

chisq.test(x, p=p)
1 - pchisq(1.0189, 3)

# Task 4
x = Uniform[, 1]

hist(x, breaks="FD")

nu = 1:10
s = 0
for (i in 1:10) {
  nu[i] = sum(ifelse(x <= i/10, 1, 0)) - s
  s = s + nu[i]
}

p = punif(1:10/10)
for (i in 10:2)
{
  p[i] = p[i] - p[i - 1]
}

chisq.test(nu, p=p)

nortest::ad.test(x)
nortest::cvm.test(x)
nortest::lillie.test(x)
nortest::pearson.test(x)
nortest::pearson.test(x, adjust = F)
nortest::sf.test(x)

?nortest::pearson.test(x)

# Task 5
x = c(69972, 382, 1)
y = c(1093547 + 16, 2469, 153)
m = matrix(c(x, y), nrow=2, ncol=3, byrow=T)

chisq.test(m)

# HW
matplot(UnitedVal[, 2:length(UnitedVal)], type="l", col=2:length(UnitedVal), lty=2:length(UnitedVal))
legend("topleft", legend=colnames(UnitedVal[, 2:length(UnitedVal)]), col=2:length(UnitedVal), lty=2:length(UnitedVal))
?legend("topleft")

colnames(UnitedVal[, 2:length(UnitedVal)])
first_group = c("BP", "Lukoil", "TNK")
second_group = c("MTK", "Sibneft", "Tatneft", "Ukos")

first_m = t(UnitedVal[, first_group])
second_m = t(UnitedVal[, second_group])

chisq.test(first_m)
chisq.test(second_m)

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

k = 10
alpha = 0.1

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
