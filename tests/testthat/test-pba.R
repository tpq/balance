library(balance)

data(iris)
x <- iris[,1:4]
pb1 <- pba(x, how = "sbp.fromPBA")
pb2 <- pba(x, how = "sbp.fromABA")
pb3 <- pba(x, how = "sbp.fromRandom")

test_that("deploying pba on self returns its balances", {

  expect_equal(
    pb1@pba,
    predict(pb1, x)
  )

  expect_equal(
    pb2@pba,
    predict(pb2, x)
  )

  expect_equal(
    pb3@pba,
    predict(pb3, x)
  )
})
