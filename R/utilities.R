# File: utilities.R
# Author: Max Graham
#
# A collection of handy functions for internal use.


Shift <- function(x, n){
  #' Shift (Internal)
  #' 
  #' Shifts the contents of a vector \code{n} places to the right (positive
  #' \code{n}) or left (negative \code{n}).
  #' 
  #' @param x vector; the values to be shifted
  #' @param n atomic integer; the number of places to Shift
  #' @return The shifted vector \code{x} (same length as the input vector)
  
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

