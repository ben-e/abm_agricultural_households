# These are functions used to generate endowments, fitted to CFP and TGCC data

#' Labor endowment drawn from the gamma distribution.
#' 
#' @param n Number of draws.
#' @param shape The shape parameter for the rgamma function. Default 4.5 per TGCC and CFP data.
#' @param scale The scale parameter for the rgamma function. Default 0.75 per TCGC and CFP data.
#' 
#' @return A labor endowment vector of size n.
labor_endowment_gamma <- function(n, shape = 4.5, rate = 0.75) {
  rgamma(n, shape, rate)
}

#' Capital endowment drawn from the gamma distribution.
#' 
#' Alternative defaults: shape = 2.75, scale = 0.75.
#' 
#' @param n Number of draws.
#' @param shape The shape parameter for the rgamma function. Default 2.5 per TGCC and CFP data.
#' @param scale The scale parameter for the rgamma function. Default 0.65 per TCGC and CFP data.
#' 
#' @return A labor endowment vector of size n.
capital_endowment_gamma <- function(n, shape = 2.5, rate = 0.65) {
  rgamma(n, shape, rate)
}