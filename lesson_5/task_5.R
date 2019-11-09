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

