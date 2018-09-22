library(balance)

data(iris)
x <- iris[,1:4]
pb <- pba(x)

test_that("deploying pba on self returns its balances", {

  expect_equal(
    pb@pba,
    predict(pb, x)
  )
})
