context("i/o")
library(allen.archaeology)

test_that("relation set strengths are equal", {
    res <- allen.relations.relationships(c("o", "O"), c("o", "O"))
    expect_match(res, "\\(oO\\) is equal to \\(oO\\)")
})
