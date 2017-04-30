#testthat
library(metrics)

context("RMSE")

test_that("RMSE works for numeric input", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(1.0, 3.0, 5.0, 7.0, 9.0)
  
  expect_equal(rmse(x, y), 2.4494897427831780981972840747059)
  expect_equal(rmse(x), 3.3166247903553998491149327366707)
})

test_that("RMSE works for integer input",{
  x <- as.integer(1:5)
  y <- as.integer(seq(1, 9, by = 2))
  
  expect_equal(rmse(x, y), 2.4494897427831780981972840747059)
  expect_equal(rmse(x), 3.3166247903553998491149327366707)
})

test_that("RMSE works for logical input.", {
  x <- c(T, T, T, T)
  y <- c(T, F, T, F)
  
  expect_equal(rmse(x, y), 0.70710678118654752440084436210485)
  expect_equal(rmse(x), 1)
})

test_that("RMSE breaks on character input.", {
  expect_error(rmse(letters[1:5]))
  expect_error(rmse(LETTERS[1:5]))
  expect_error(rmse(as.character(1:5)))
  expect_error(rmse(letters[1:5], 1:5))
})

context("MSE")
test_that("MSE works for numeric input", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(1.0, 3.0, 5.0, 7.0, 9.0)
  
  expect_equal(mse(x, y), 6)
  expect_equal(mse(x), 11)
})

test_that("MSE works for integer input",{
  x <- as.integer(1:5)
  y <- as.integer(seq(1, 9, by = 2))
  
  expect_equal(mse(x, y), 6)
  expect_equal(mse(x), 11)
})

test_that("MSE works for logical input.", {
  x <- c(T, T, T, T)
  y <- c(T, F, T, F)
  
  expect_equal(mse(x, y), 0.5)
  expect_equal(mse(x), 1)
})

test_that("MSE breaks on character input.", {
  expect_error(mse(letters[1:5]))
  expect_error(mse(LETTERS[1:5]))
  expect_error(mse(as.character(1:5)))
  expect_error(mse(letters[1:5], 1:5))
})