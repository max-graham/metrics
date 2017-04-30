# File: error.R
# Author: Max Graham
# Date: 2017-04-28
#
# A collection of useful error metrics


rmse <- function(x, target = NULL, na.rm = FALSE){
  #' RMSE
  #' 
  #' Calculates the Root Mean Squared Error(RMSE) between x and the target. If
  #' target is NULL, calculates the RMSE on the input vector x.
  #' 
  #' @param x numeric vector
  #' @param target numeric vector; If not NULL, calculates rmse(x - target)
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated RMSE
  #' @examples 
  #' rmse(c(1, 2, 3))             #> 2.160247
  #' rmse(c(1, 2, 3), c(1, 4, 4)) #> 1.290994
  #' rmse(c(1, 2, NA))            #> NA
  #' @export
  
  # Error handling
  n <- length(x)
  if (!is.null(target)){
    if (n <= 1 || n != length(target)) {
      stop("Arguments x and target have different lengths: ",
           length(x), " and ", length(target), ".")
    }
    return(rmse(x - target, na.rm = na.rm))
  }
  if (na.rm){
    x <- x[!is.na(x)]
  }
  
  sqrt(mse(x))  # return
}

mse <- function(x, target = NULL, na.rm = FALSE){
  #' MSE
  #' 
  #' Calculates the Mean Squared Error(MSE), or Mean Squared Deviation (MSD),
  #' between x and the target. If target is NULL, calculates the MSE on the
  #' input vector x.
  #'
  #' @param x numeric vector
  #' @param target numeric vector; If not NULL, calculates mse(x - target)
  #' @param na.rm logical; should NAs be removed before calculation?
  #' @return An atomic numeric vector containing the calculated MSE
  #' @examples 
  #' mse(c(1, 2, 3))             #> 4.666667
  #' mse(c(1, 2, 3), c(1, 4, 4)) #> 1.666667
  #' mse(c(1, 2, NA))            #> NA
  #' @export
  
  # Error handling
  n <- length(x)
  if (!is.null(target)){
    if (n <= 1 || n != length(target)) {
      stop("Arguments x and target have different lengths: ",
           length(x), " and ", length(target), ".")
    }
    return(mse(x - target, na.rm = na.rm))
  }
  if (na.rm){
    x <- x[!is.na(x)]
  }
  
  mean(x^2)  # return
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
  }
  
  (100 / n) * sum(abs((target - x) / target))
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
  #' mase(c(1, 2, 3), c(1, 4, 4))          #> 0.3333333
  #' mase(c(1, 2, NA, 4), c(2, 2, 5, NA))  #> NA
  #' mase(c(1, 2, 3, NA, 4),
  #'      c(2, 2, 5, NA, NA),
  #'      na.rm = TRUE)                    #> 0.6666667
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
  
  y <- Shift(target, m)
  
  sum(abs(target - x)) / ((n / (n - m)) * sum(abs(target[(m + 1):n] - y[(m + 1):n])))
}

# TODO(Max): Move this to a utilities package
Shift <- function(x, n){
  #' Shift (Internal)
  #' 
  #' Shifts the contents of a vector \code{n} places to the right (positive
  #' \code{n}) or left (negative \code{n}).
  #' 
  #' @param x vector; the values to be shifted
  #' @param n atomic integer; the number of places to Shift
  #' @return the shifted vector \code{x} (same length as the input vector)

  # If n > length(x), then you're shifting everything out of the scope of x
  if (abs(n) > length(x)){
    return(rep(NA, length(x)))
  }
  
  if (n < 0){
    # Shift left
    return(c(utils::tail(x, length(x) - abs(n)), rep(NA, abs(n))))
  } else {
    # Shift right
    return(c(rep(NA, n), utils::head(x, length(x) - n)))
  }
}

