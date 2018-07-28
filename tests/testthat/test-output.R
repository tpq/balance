library(balance)

data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
                 c(0,+1,-1,0,0),c(0,0,0,+1,-1))
colnames(y1) <- paste0("z", 1:4)

res1 <- balance(expenditures, y1)

test_that("output contains ggplot objects", {

  expect_true(
    "ggplot" %in% class(res1[[1]])
  )

  expect_true(
    "ggplot" %in% class(res1[[2]])
  )

  expect_true(
    "matrix" %in% class(res1[[3]])
  )
})
