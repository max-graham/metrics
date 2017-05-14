# File: error.R
# Author: Max Graham
# Date: 2017-04-28
#
# A collection of useful error metrics

ae <- function(x, target, na.rm = FALSE){
  #' Absolute Error
  #' 
  #' Calculates the absolute difference between \code{x} and \code{target}.
  #' 
  #' @param x numeric vector; the values to check
  #' @param target numeric vector; the values to compare against
  #' @param na.rm logical; should we remove NA cases?
  #' @return atomic numeric value; the absolute error
  #' @examples 
  #' ae(c(1, 2, 3), c(1, 2, 4))     #> 1
  #' ae(c(1, 2, 3), c(2, 3, 4))     #> 3
  #' ae(c(1, 2, 3), c(-1, -2, -3))  #> 12
  #' @export
  
  # Error handling
  n <- length(x)
  if (n < 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
  }
  
  sum(abs(target - x), na.rm = na.rm)
}

mse <- function(x, target, na.rm = FALSE){
  #' MSE
  #' 
  #' Calculates the Mean Squared Error(MSE), or Mean Squared Deviation (MSD),
  #' between x and the target. Calculation uses (target - x).
  #'
  #' @param x numeric vector
  #' @param target numeric vector
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated MSE
  #' @examples 
  #' mse(c(1, 2, 3), c(1, 4, 4))      #> 1.666667
  #' mse(c(1, 2, NA), c(NA, 10, 20))  #> NA
  #' @export
  
  # Error handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
  }
  
  mean((target- x)^2, na.rm = na.rm)  # return
}

rmse <- function(x, target, na.rm = FALSE){
  #' RMSE
  #' 
  #' Calculates the Root Mean Squared Error(RMSE) between x and the target.
  #' Calculation uses (target - x).
  #' 
  #' @param x numeric vector
  #' @param target numeric vector
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated RMSE
  #' @examples 
  #' rmse(c(1, 2, 3), c(1, 4, 4))      #> 1.290994
  #' rmse(c(1, 2, NA), c(NA, 10, 20))  #> NA
  #' @export
  
  # Error handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
  }
  
  sqrt(mse(x, target, na.rm = na.rm))  # return
}

mape <- function(x, target, na.rm = FALSE){
  #' MAPE
  #' 
  #' Calculates the Mean Absolute Percentage Error(MAPE) between x and the
  #' target.
  #' 
  #' @param x numeric vector; the forecast to evaluate
  #' @param target numeric vector; the target values
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated MAPE
  #' @examples 
  #' \dontrun{mape(c(1, 2, 3))}            #> Error: different lengths (3, 0)
  #' mape(c(1, 2, 3), c(1, 4, 4))          #> 25.0
  #' mape(c(1, 2, NA, 4), c(2, 2, 5, NA))  #> NA
  #' mape(c(1, 2, NA, 4),
  #'      c(2, 2, 5, NA),
  #'      na.rm = TRUE)                    #> 12.5
  #' @export
  
  # Error handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
    n <- length(x)
  }
  
  (100 / n) * sum(abs((target - x)) / abs(target), na.rm = na.rm)
}

mase <- function(x, target, m = 1, na.rm = FALSE){
  #' MASE
  #' 
  #' Calculates the Mean Absolute Scaled Error(MASE) between x and the
  #' target. This is intended to be used for measuring the accuracy of a
  #' time series forecast, since it reports the error in the context of the
  #' naive forecast (random walk for non-seasonal, or a seasonal naive otherwise).
  #'
  #' @note This will return \code{Inf} when the last \code{length(x) - m}
  #'   values in target are all the same.
  #' @param x numeric vector; the forecast to evaluate.
  #' @param target numeric vector; the target values.
  #' @param m atomic integer; the seasonal period. Defaults to 1 if non-seasonal.
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated MASE.
  #' @examples 
  #' \dontrun{mase(c(1, 2, 3))}            #> Error: different lengths (3, 0)
  #' mase(c(1, 2, 3), c(1, 4, 4))          #> 0.66667
  #' mase(c(1, 2, NA, 4), c(2, 2, 5, NA))  #> NA
  #' mase(c(1, 2, 3, NA, 4),
  #'      c(2, 2, 5, NA, NA),
  #'      na.rm = TRUE)                    #> 0.66667
  #' @export

  # Error handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
    n <- length(x)  # update after masking NAs
  }
  if(m <= 0){
    stop("m must be positive.")
  }
  if(m >= length(target)){
    stop("m must be less than the number of periods in target.")
  }
  
  y <- Shift(target, m)
  sum(abs(target - x)) / ((n / (n - m)) * sum(abs(target[(m + 1):n] - y[(m + 1):n])))
}

sse <- function(x, target, na.rm = FALSE){
  #' SSE
  #' 
  #' Calculates the sum of squared errors (SSE) between x and the target.
  #' 
  #' @param x numeric vector
  #' @param target numeric vector
  #' @param na.rm logical; should NAs be removed before the calculation?
  #' @return An atomic numeric vector containing the calculated SSE
  #' @examples 
  #' sse(c(1, 2, 3), c(2, 3, 4))                 #> 1 + 1 + 1 =  3.0
  #' sse(c(1, 2, 3), c(3, 4, 5))                 #> 4 + 4 + 4 = 12.0
  #' sse(c(1, 2, NA), c(1, 2, 3))                #> NA
  #' sse(c(1, 2, NA), c(1, 2, 3), na.rm = TRUE)  #> 0 + 0 = 0.0
  #' @export
  
  # Error Handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if(na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
  }
  sum((target - x)^2, na.rm = na.rm)
}

mpe <- function(x, target, na.rm = FALSE){
  #' MPE
  #' 
  #' Calculates the Mean Percentage Error(MPE) between x and the
  #' target.
  #' 
  #' @param x numeric vector
  #' @param target numeric vector
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated MPE
  #' @examples 
  #' \dontrun{mpe(c(1, 2, 3))}            #> Error: different lengths (3, 0)
  #' mpe(c(1, 2, 3), c(1, 4, 4))          #> 25.0
  #' mpe(c(1, 2, NA, 4), c(2, 2, 5, NA))  #> NA
  #' mpe(c(1, 2, NA, 4),
  #'     c(2, 2, 5, NA),
  #'     na.rm = TRUE)                    #> 12.5
  #' @export
  
  # Error handling
  n <- length(x)
  if (n <= 1 || n != length(target)) {
    stop("Arguments x and target have different lengths: ",
         length(x), " and ", length(target), ".")
  }
  if (na.rm){
    mask <- (is.na(x) | is.na(target))
    x <- x[!mask]
    target <- target[!mask]
    n <- length(x)
  }
  
  (100 / n) * sum((target - x) / target, na.rm = na.rm)
}




