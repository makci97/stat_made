lambda = 5
u = runif(100)
t = -log(1 - u) / lambda

threshold = 3 / lambda
n_more_than = sum(ifelse(t > threshold, 1,0))
p_more_than = 1 - (1  - exp(-lambda*threshold))

print(n_more_than)
print(p_more_than)


v = 1:100
v
v[ifelse(u < 0.2, T, F)]


a = 40
b = 60
n = 100
p = 1/2
m = n * p
d = sqrt(n * p * (1 - p))
pnorm((b - m) / d) - pnorm((a - 1 - m) / d)

pbinom(b, n, p) - pbinom(a - 1, n, p)

