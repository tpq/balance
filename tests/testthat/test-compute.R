library(balance)

data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
                 c(0,+1,-1,0,0),c(0,0,0,+1,-1))
rownames(y1) <- colnames(expenditures)
colnames(y1) <- paste0("z", 1:4)

a <- robCompositions::balances(expenditures, y1)[[1]]
colnames(a) <- paste0("z", 1:4)
rownames(a) <- as.character(1:nrow(a))
b <- apply(y1, 2, function(z) balance:::balance.fromContrast(expenditures, z))
rownames(b) <- as.character(1:nrow(b))
c <- balance::balance.fromSBP(expenditures, y1)

test_that("balance::balance.fromSBP() matches robCompositions::balances()", {

  expect_equal(
    a,
    b
  )

  expect_equal(
    a,
    c
  )
})
