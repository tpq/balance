library(balance)

data(iris)
x <- iris[,1:4]

test_that("this vlr matches the one used in propr", {

  expect_equal(
    as.numeric(propr:::lr2vlr(as.matrix(log(x)))),
    as.numeric(vlr(x))
  )
})
