#' Find the greatest common divisor of two numbers
#'
#' \code{euclidean} returns the greatest common divisor of two values provided
#' as arguments.
#' Pseudocode taken from the Wikipedia page for the Euclidean algorithm.
#'
#' @param a,b Non negative integers.
#'
#' @return Greatest common divisor of \code{a} and \code{b}.
#' @examples
#' euclidean(100, 1000)
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export


euclidean <- function(a, b) {

  stopifnot("arguments must be numeric scalars" =
              all(is.numeric(c(a,b)), length(a) == 1, length(b) == 1), a >= 0, b >= 0)

  stopifnot("arguments must be integers" = all(floor(a) == a), floor(b) == b)

  if(b == 0)
    return(a)

  return(euclidean(b, a %% b))

}
