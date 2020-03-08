context("relations")
library(allen.archaeology)

test_that("interval 1 precedes interval 2", {
    expect_match(allen.relation(0, 1, 2, 3), "p")
})

test_that("interval 1 meets interval 2", {
    expect_match(allen.relation(0, 1, 1, 2), "m")
})

test_that("interval 1 overlaps interval 2", {
    expect_match(allen.relation(0, 2, 1, 3), "o")
})

test_that("interval 1 finished by interval 2", {
    expect_match(allen.relation(0, 3, 1, 3), "F")
})

test_that("interval 1 contains interval 2", {
    expect_match(allen.relation(0, 3, 1, 2), "D")
})

test_that("interval 1 starts interval 2", {
    expect_match(allen.relation(0, 1, 0, 2), "s")
})

test_that("interval 1 equals interval 2", {
    expect_match(allen.relation(0, 1, 0, 1), "e")
})

test_that("interval 1 started by interval 2", {
    expect_match(allen.relation(0, 2, 0, 1), "S")
})

test_that("interval 1 preceded by interval 2", {
    expect_match(allen.relation(2, 3, 0, 1), "P")
})

test_that("interval 1 met by interval2", {
    expect_match(allen.relation(1, 2, 0, 1), "M")
})

test_that("interval 1 overlapped by interval 2", {
    expect_match(allen.relation(1, 3, 0, 2), "O")
})

test_that("interval 1 finishes interval 2", {
    expect_match(allen.relation(1, 2, 0, 2), "f")
})

test_that("interval 1 during interval 2", {
    expect_match(allen.relation(1, 2, 0, 3), "d")
})
