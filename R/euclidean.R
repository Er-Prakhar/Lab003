#' Find the greatest common divisor of two numbers.
#'
#' Pseudocode from: https://en.wikipedia.org/wiki/Euclidean_algorithm. 
#'
#' @param a A number
#' @param b A number
#' @return The greatest common divisor
#' @examples
#' euclidean(100, 1000)
#' @export


euclidean <- function(a, b) {
  
  stopifnot("arguments must be numeric scalars" = 
              all(is.numeric(c(a,b)), length(a) == 1, length(b) == 1), a >= 0, b >= 0)
  
  stopifnot("arguments must be integers" = all(floor(a) == a), floor(b) == b)
  
  if(b == 0)
    return(a)
  
  return(euclidean(b, a %% b))
  
}