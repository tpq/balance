library(balance)

h <- hclust(dist(cars[1:35,]))
phylo <- ape::as.phylo(h)
sbp1 <- philr::phylo2sbp(phylo)
sbp2 <- sbp.fromHclust(h)
colnames(sbp2) <- NULL

test_that("sbp.fromHclust matches the ape and philr method", {

  expect_equal(
    sbp.sort(sbp1),
    sbp.sort(sbp2)
  )
})
