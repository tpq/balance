library(balance)

data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
                 c(0,+1,-1,0,0),c(0,0,0,+1,-1))
colnames(y1) <- paste0("z", 1:4)

res1 <- balance(expenditures, y1)
unsorted <- res1[[1]]$data
A <- unsorted[order(paste0(unsorted$Component, unsorted$BalanceID)),]
rownames(A) <- NULL

y2 <- y1[, c("z2", "z3", "z4", "z1")]
res2 <- balance(expenditures, y2)
sorted <- res2[[1]]$data
B <- sorted[order(paste0(sorted$Component, sorted$BalanceID)),]
rownames(B) <- NULL

set.seed(1)
sample <- sample(1:5)
res3 <- balance(expenditures[,sample], y2[sample,])
sorted2 <- res3[[1]]$data
C <- sorted2[order(paste0(sorted2$Component, sorted2$BalanceID)),]
rownames(C) <- NULL

test_that("changing order of y does not alter results", {

  expect_equal(
    A,
    B
  )

  expect_equal(
    B,
    C
  )
})
