library(balance)

data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
                 c(0,+1,-1,0,0),c(0,0,0,+1,-1))
colnames(y1) <- paste0("z", 1:4)

a <- robCompositions::balances(expenditures, y1)[[1]]
colnames(a) <- paste0("z", 1:4)
rownames(a) <- as.character(1:nrow(a))
b <- apply(y1, 2, function(z) balance.compute(expenditures, z))
rownames(b) <- as.character(1:nrow(b))
c <- balance::balances(expenditures, y1)

test_that("balance::balances() matches robCompositions::balances()", {

  expect_equal(
    a,
    b
  )

  expect_equal(
    a,
    c
  )
})