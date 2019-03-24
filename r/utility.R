#' The utility function used by XYW, adding in linear leisure.
#' 
#' @param x Units of the good consumed.
#' @param r Risk aversion parameter, higher is more risk averse. Greater than 0. Time invariant.
#'
#' @return Utility.
utility_xyw <- function(x, l, r) -exp(-(x+l)*r) + 1

#' A linear utility function.
#' 
#' @param x Units of the good consumed.
#' @param l Leisure.
#'
#' @return Utility.
utility_linear <- function(x, l) x + l

