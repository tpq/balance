library(balance)

data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
                 c(0,+1,-1,0,0),c(0,0,0,+1,-1))
colnames(y1) <- paste0("z", 1:4)

a <- reshape2::melt(expenditures)
b <- wide2long(expenditures)

test_that("wide2long melts data like reshape2::melt", {

  expect_equal(
    a$value,
    b$value
  )

  expect_equal(
    as.character(a$variable),
    b$variable
  )

  expect_equal(
    expenditures[,"housing"],
    b[b$variable == "housing","value"]
  )

  expect_equal(
    as.character(1:20),
    b[b$variable == "housing","id"]
  )
})
