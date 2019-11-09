# Равномерное
# 1
n = 100
x = runif(n)
y = sqrt(runif(n))

plot(ecdf(x))
abline(0, 1)
plot(ecdf(y))
abline(0, 1)

?ks.test

ks.test(x, 'punif')
ks.test(y, 'punif')

# Показательное
# 2
x = Condit[,1]
boxplot(x)
hist(x, breaks="FD")

n = length(x)
y = 1:n
z = 1:n
s = 0
for (k in 1:length(x)){
  s = s + x[k]
  y[k] = s
}
z = y / y[n]
z[n] = NA
plot(ecdf(z))
ks.test(z, 'punif')

# 3
right_threshold = quantile(x, 0.75) + 1.5*IQR(x)
left_threshold = quantile(x, 0.25) - 1.5*IQR(x)
x_cleaned = ifelse(x>right_threshold | x < left_threshold, NA, x)

x_cleaned = na.omit(x_cleaned)

boxplot(x_cleaned)
hist(x_cleaned, breaks="FD")

n = length(x_cleaned)
y = cumsum(x_cleaned)
z = y / y[n]
z[n] = NA
plot(ecdf(z))
ks.test(z, 'punif')

# Нормальное
# 4
x = Employees[, "SALARY"]

boxplot(x)
hist(x, breaks="FD")

?qqnorm
qqnorm(x)
qqline(x)

# 5
# критерий Колмогорова с поправкой Лильефорса
?lillie.test
lillie.test(x)

# критерий Крамера ― фон Мизеса
?cvm.test
cvm.test(x)

# критерий Андерсона — Дарлинга
?ad.test
ad.test(x)

# критерий Шапиро — Франчиа
?sf.test
sf.test(x)

log_freq = Prefix.ver[, "LogFrequency"]
#log_freq = ifelse(log_freq == 0, NA, x)
#log_freq = na.omit(log_freq)

plot(ecdf(log_freq))
boxplot(log_freq)
hist(log_freq, breaks="FD")

qqnorm(log_freq)
qqline(log_freq)

lillie.test(log_freq)
cvm.test(log_freq)
ad.test(log_freq)
sf.test(log_freq)


# HW
x = Uniform[, 1]
#x = runif(10000)
z = log(x / (1 - x))

plot(ecdf(z))
boxplot(z)
hist(z, breaks="FD")

lillie.test(z)
cvm.test(z)
ad.test(z)
sf.test(z)

qqnorm(z)
qqline(z)

