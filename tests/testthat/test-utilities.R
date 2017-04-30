context("Shift")
test_that("Shift works for positive N", {
  x <- 1:10
  
  for(n in 1:(length(x) - 1)){
    expect_equal(Shift(x, n), c(rep(NA, n), head(x, length(x) - n)))
  }
})

test_that("Shift works for negative N", {
  x <- 1:10
  
  for(n in 1:(length(x) - 1)){
    expect_equal(Shift(x, -1*n), c(tail(x, length(x) - n), rep(NA, n)))
  }
})

test_that("Shift works for N == 0.", {
  x <- 1:10
  expect_equal(Shift(x, 0), x)
})

test_that("Shift works for abs(N) > length(x).", {
  x <- 1:10
  expect_equal(Shift(x, length(x) + 1), rep(NA, length(x)))
  expect_equal(Shift(x, -1*(length(x) + 1)), rep(NA, length(x)))
})

test_that("Shift of x when length(x) == 0 as length 0", {
  x <- numeric(0)
  expect_equal(length(Shift(x, 1)), 0)
})