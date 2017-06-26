#introduction to statistics 3
#week 1
#task 1.2
test_data <- as.data.frame(list(X1 = c(-1.4, -1, 0.3, 0.3, -1.4, -1, 0.4, -0.1, 0.8, -0.5), X2 = c(0.2, -1.7, 0.1, -0.5, -0.5, 0.6, -0.2, -0.7, -0.7, -0.2), X3 = c(-1.3, 0.4, -1.3, 1, 1.1, 0.1, -1.8, -0.3, -1.6, 0), X4 = c(0.1, 0.2, -0.6, -0.6, -0.5, 0.1, -0.8, 0.7, 0.2, 1.3), X5 = c(-1.7, -0.3, 1.4, -1.3, -0.7, 0.9, 0.5, 1, 1.2, 1.3)))
fit <- lm (test_data[,1] ~ ., test_data[-1])

qplot (x = test_data[,1], y = fit$residuals, data = test_data) + geom_smooth(method = lm)
summary(fit)
fit2 <- lm (I (fit$residuals^2) ~ ., test_data[-1])
summary(fit2)$r.squared


#task 1.3

test_data <- as.data.frame(list(X1 = c(-0.4, 0.7, -0.7, 0.5, -1, -0.7, -0.3, 1.2, 0.4, 1.7), X2 = c(-1, 0.7, -1.2, -0.7, 0.4, -1.9, -0.5, -0.5, 0.3, 0.1), X3 = c(-0.3, -0.8, -1.1, -1.6, 0.8, 0, -0.2, 0, -1.8, 0.5), X4 = c(-0.8, 1.5, 1.8, 0.6, -1.2, -0.1, -0.8, 1.1, -1.1, -0.4)))
n <- names(test_data[-1])
z <- sapply (n, function(x) 1/(1-summary(lm(reformulate(n[n!= x], x), data = test_data))$r.squared) )

#the best solution with as.formula!
z <- sapply (n, function(x) 1/(1-summary(lm(as.formula(paste(x, "~.")), data = test_data))$r.squared) )
z <- sapply (n, function(x) print(as.formula(paste(x, "~."))))
z

#task 1.4
test_data <- as.data.frame(list(X1 = c(-0.4, 0.7, -0.7, 0.5, -1, -0.7, -0.3, 1.2, 0.4, 1.7), X2 = c(-1, 0.7, -1.2, -0.7, 0.4, -1.9, -0.5, -0.5, 0.3, 0.1), X3 = c(-0.3, -0.8, -1.1, -1.6, 0.8, 0, -0.2, 0, -1.8, 0.5), X4 = c(-0.8, 1.5, 1.8, 0.6, -1.2, -0.1, -0.8, 1.1, -1.1, -0.4)))
test_data <- mtcars
n <- names(test_data[-1])
z <- sapply (n, function(x) 1/(1-summary(lm(as.formula(paste(x, "~.")), data = test_data))$r.squared) )
z
while (length(which (z > 10)) != 0 )
{
  z <- z[-which.max(z)]
  n <- names(z)
  test_data1 <- test_data[n]
  z <- sapply (n, function(x) 1/(1-summary(lm(as.formula(paste(x, "~.")), data = test_data1))$r.squared) )
  z
}
n <- names(z)
test_data1 <- test_data[n]
fit <- lm (test_data[,1] ~ ., test_data[n])
return (fit$coefficients)

#task 1.5
set.seed(42)
test_data <- data.frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))  
s <- seq (-2, 2, by = 0.1)
s1 <- sapply (s, function (x) if (x > 0) test_data[,2]^x else if (x < 0) -test_data[,2]^x else log(test_data[,2]))
s2 <- as.data.frame(s1)
s2
n <- names(s2)
r <- sapply (n, function (z) summary(lm (test_data[,1] ~ s2[,z]))$r.squared )
as.vector(s1[,which.max(r)])
