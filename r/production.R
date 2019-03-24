#' A Cobb-Douglas Production Function.
#' 
#' @param a Technology
#' @param l Labor
#' @param beta Output elasticity of labor.
#' @param k Capital.
#' @param alpha Output elasticity capital.
production_cd <- function(a, l, beta, k, alpha) {
  a*(l^beta)*(k^alpha)
}