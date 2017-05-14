library(metrics)

context("RMSE")
test_that("RMSE works for numeric input", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(1.0, 3.0, 5.0, 7.0, 9.0)
  
  expect_equal(rmse(x, y), 2.4494897427831780981972840747059)
  expect_equal(rmse(x, 2*x), 3.3166247903553998491149327366707)
})

test_that("RMSE works for integer input",{
  x <- as.integer(1:5)
  y <- as.integer(seq(1, 9, by = 2))
  
  expect_equal(rmse(x, y), 2.4494897427831780981972840747059)
  expect_equal(rmse(x, 2*x), 3.3166247903553998491149327366707)
})

test_that("RMSE works for logical input.", {
  x <- c(T, T, T, T)
  y <- c(T, F, T, F)
  
  expect_equal(rmse(x, y), 0.70710678118654752440084436210485)
  expect_equal(rmse(x, 2*x), 1)
})

test_that("RMSE breaks on character input.", {
  expect_error(rmse(letters[1:5]))
  expect_error(rmse(LETTERS[1:5]))
  expect_error(rmse(as.character(1:5)))
  expect_error(rmse(letters[1:5], 1:5))
})

test_that("RMSE breaks when inputs have different lengths", {
  expect_error(rmse(c(1, 2, 3), c(1, 3, 5, 7, 9)))
})

test_that("RMSE correctly removes NAs", {
  x <- c(NA, 1:5)
  y <- c(NA, seq(1, 9, 2))
  
  expect_equal(rmse(x, y, na.rm = TRUE), 2.4494897427831780981972840747059)
  expect_equal(rmse(x, 2*x, na.rm = TRUE), 3.3166247903553998491149327366707)
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

test_that("MSE breaks when inputs have different lengths", {
  expect_error(mse(c(1, 2, 3), c(1, 3, 5, 7, 9)))
})

test_that("MSE correctly removes NAs", {
  x <- c(NA, 1:5)
  y <- c(NA, seq(1, 9, 2))
  
  expect_equal(mse(x, y, na.rm= TRUE), 6)
  expect_equal(mse(x, na.rm = TRUE), 11)
})

context("MAPE")
test_that("MAPE works for numeric input", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(1.0, 3.0, 5.0, 7.0, 9.0)
  
  expect_equal(mape(x, y), 32.126984126984126984126984126984)
})

test_that("MAPE works for integer input",{
  x <- as.integer(1:5)
  y <- as.integer(seq(1, 9, by = 2))
  
  expect_equal(mape(x, y), 32.126984126984126984126984126984)
})

test_that("MAPE breaks on character input.", {
  expect_error(mape(letters[1:5]))
  expect_error(mape(LETTERS[1:5]))
  expect_error(mape(as.character(1:5)))
  expect_error(mape(letters[1:5], 1:5))
})

test_that("MAPE breaks when x and target lengths differ.", {
  expect_error(mape(1:10, 1:5))
  expect_error(mape(1:10))
})

test_that("MAPE correctly removes NAs.", {
  x <- c(1:5, NA)
  y <- c(seq(1, 9, by = 2), NA)
  
  expect_equal(mape(x, y, na.rm = TRUE), 32.126984126984126984126984126984)
})

context("MASE")
test_that("MASE breaks when target isn't specified.", {
  expect_error(mase(1:10))
})

test_that("MASE breaks when x and target lengths differ.", {
  expect_error(mase(1:10, 1:5))
  expect_error(mase(1:5, 1:10))
})

test_that("MAPE breaks on character input.", {
  expect_error(mase(letters[1:5], letters[1:5]))
  expect_error(mase(LETTERS[1:5]))
  expect_error(mase(as.character(1:5)))
  expect_error(mase(letters[1:5], 1:5))
})

test_that("MASE works for numeric input.", {
  expect_equal(mase(c(1, 2, 3), c(1, 4, 4)), 0.6666666666666666666666667)
  expect_equal(mase(c(1, 2, NA, 4), c(2, 2, 5, NA)), NA_real_)
})

test_that("MASE works for integer input.", {
  expect_equal(mase(c(1L, 2L, 3L), c(1L, 4L, 4L)), 0.6666666666666666666666667)
  expect_equal(mase(c(1L, 2L, NA, 4L), c(2L, 2L, 5L, NA)), NA_real_)
})

test_that("MASE correctly removes NAs.", {
  expect_equal(mase(c(1, 2, 3, NA, 4),
                    c(2, 2, 5, NA, NA),
                    na.rm = TRUE),
               0.66666666666666666666667)
})

test_that("MASE returns Inf when the target is constant.", {
  expect_equal(mase(1:10, rep(10, 10), m = 2), Inf)
})

test_that("MASE breaks when m is negative (or zero).", {
  expect_error(mase(1:10, 1:10, -5))
  expect_error(mase(1:10, 1:10, 0))
})

test_that("MASE breaks when m > length(target).", {
  expect_error(mase(1:10, 1:10, 100))
})

context("AE")
test_that("AE works for numeric input.", {
  x <- c(1, 2, 3)
  expect_equal(ae(x, c(1, 2, 4)), 1)
  expect_equal(ae(x, c(-1, -2, -3)), 12)
  expect_equal(ae(x, c(2, 3, 4)), 3)
})

test_that("AE works for integer input.", {
  x <- as.integer(c(1, 2, 3))

  expect_equal(ae(x, c(1L, 2L, 4L)), 1)
  expect_equal(ae(x, c(-1L, -2L, -3L)), 12)
  expect_equal(ae(x, c(2L, 3L, 4L)), 3)
})

test_that("AE breaks when x and target have different lengths.", {
  expect_error(ae(c(1, 2, 3), c(1)))
})

test_that("AE correctly handles NAs.", {
  x <- c(NA, 1:5)
  y <- c(1:5, NA)
  
  expect_equal(ae(x, y, na.rm = TRUE), 4)
  expect_equal(ae(x, y), NA_real_)
})

test_that("AE breaks on character input.", {
  expect_error(ae(letters[1:5], letters[1:5]))
  expect_error(ae(LETTERS[1:5]))
  expect_error(ae(as.character(1:5)))
  expect_error(ae(letters[1:5], 1:5))
})

context("SSE")
test_that("SSE works for numeric input.", {
  x <- c(1, 2, 3)
  expect_equal(sse(x, c(1, 2, 4)), 1)
  expect_equal(sse(x, c(-1, -2, -3)), 56)
  expect_equal(sse(x, c(2, 3, 4)), 3)
})

test_that("SSE breaks when x and target have different lengths.", {
  expect_error(sse(c(1, 2, 3), c(1)))
})

test_that("SSE correctly handles NAs.", {
  x <- c(NA, 1:5)
  y <- c(1:5, NA)
  
  expect_equal(sse(x, y, na.rm = TRUE), 4)
  expect_equal(sse(x, y), NA_real_)
})

test_that("SSE breaks on character input.", {
  expect_error(sse(letters[1:5], letters[1:5]))
  expect_error(sse(LETTERS[1:5]))
  expect_error(sse(as.character(1:5)))
  expect_error(sse(letters[1:5], 1:5))
})

context("MPE")
test_that("MPE works for numeric input", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(1.0, 3.0, 5.0, 7.0, 9.0)
  
  expect_equal(mpe(x, y), 32.126984126984126984126984126984)
})

test_that("MPE works for integer input",{
  x <- as.integer(1:5)
  y <- as.integer(seq(1, 9, by = 2))
  
  expect_equal(mpe(x, y), 32.126984126984126984126984126984)
})

test_that("MPE breaks on character input.", {
  expect_error(mpe(letters[1:5]))
  expect_error(mpe(LETTERS[1:5]))
  expect_error(mpe(as.character(1:5)))
  expect_error(mpe(letters[1:5], 1:5))
})

test_that("MPE breaks when x and target lengths differ.", {
  expect_error(mpe(1:10, 1:5))
  expect_error(mpe(1:10))
})

test_that("MPE correctly removes NAs.", {
  x <- c(1:5, NA)
  y <- c(seq(1, 9, by = 2), NA)
  
  expect_equal(mpe(x, y, na.rm = TRUE), 32.126984126984126984126984126984)
})



