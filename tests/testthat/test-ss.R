library(balance)
library(vegan)

data(iris)
x <- iris[1:100,1:4]
y <- iris[1:100,5]

ss.v <- sapply(1:ncol(x), function(i){
  rda1 <- rda(x[,i], y)
  rda1$CCA$eig/ rda1$CA$eig
})
ss.b <- ssBetween(x, y) / ssWithin(x, y)

test_that("ss matches the decomposition by vegan for variables", {

  expect_equal(
    as.numeric(round(ss.v, 5)),
    as.numeric(round(ss.b, 5))
  )
})

lr12.v <- rda(log(x[,1] / x[,2]), y)
lr12.v <- lr12.v$CCA$eig / lr12.v$CA$eig
lr12.b <- ssBetween(x, y, pairwise = TRUE) / ssWithin(x, y, pairwise = TRUE)
lr12.b <- lr12.b[1,2]

test_that("ss matches the decomposition by vegan for log-ratios", {

  expect_equal(
    as.numeric(round(lr12.v, 5)),
    as.numeric(round(lr12.b, 5))
  )
})
