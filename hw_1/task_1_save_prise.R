n = 500
p = 0.0123
threshold = 12
lambda = n * p

1 - ppois(threshold - 1, lambda = lambda)

1 - pbinom(threshold - 1, size = n, prob = p)
