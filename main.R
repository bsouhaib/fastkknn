rm(list = ls())
source("fastkknn.R")
data <- data.frame(embed(sunspot.year, 10))
colnames(data)[1] <- "Y"
N <- nrow(data)

Dtrain <- head(data, round(0.7*N))
Dval <- tail(data, N - round(0.7*N))

# Predict Y in Dval with k = 4 neighbors
res <- fastkknn(as.formula("Y~."), train = Dtrain, test = Dval, k = 4)

# Give the results for all k from 1 to 100 by inc = 2 (k = 1, 3, 5, ...)
res <- fastkknn(as.formula("Y~."), train = Dtrain, test = Dval, k = 100, allk = T)

# Give the results for all k from 1 to 100 by inc = 1 (k = 1, 2, 3, 4, 5, ...)
res <- fastkknn(as.formula("Y~."), train = Dtrain, test = Dval, k = 100, allk = T, inc = 1)

