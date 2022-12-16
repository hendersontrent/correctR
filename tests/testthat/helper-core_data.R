# Generate data and models to test package

x <- stats::rnorm(30, mean = 0.6, sd = 0.1)
y <- stats::rnorm(30, mean = 0.4, sd = 0.1)

# Data for repeated k-fold cross-validation correction

tmp <- data.frame(model = rep(c(1, 2), each = 60),
                  values = c(stats::rnorm(60, mean = 0.6, sd = 0.1),
                             stats::rnorm(60, mean = 0.4, sd = 0.1)),
                  k = rep(c(1, 1, 2, 2), times = 15),
                  r = rep(c(1, 2), times = 30))
