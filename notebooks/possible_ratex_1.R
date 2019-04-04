#' Determine future output from a number of possible paths.
#' 
#' @param utility_fun A utility function that takes x.
#' @param prod_fun A production function that takes a.
#' @param fwd The number of recrusive levels the agent considers.
#' @param npaths The number of paths that should be considerd at each level.
#' @param x The current amount of item x that can be invested or consumed.
#' 
#' @return A scalar representing the optimal amount of x to invest.
.future_paths <- function(utility_fun, prod_fun, fwd, npaths, x, a, util = 0, path = "") {
  # Return utility and path at base of recursion
  if (fwd == 0)
    return(data_frame(util = util, path = path))
  else {
    # Calculate possible paths
    # 0 investment is always possible, and the agent must always consume at least 1
    possible_paths <- seq(0, x-1, length.out = npaths)
    
    # Get utility for each possible path
    util <- util + utility_fun(x -  possible_paths)
    # Get output in the next period for investment along each possible path
    out <- prod_fun(a + possible_paths)*runif(npaths, 0.8, 1)
    # Not sure if I should have the runif here...
    
    # Return realized paths
    map_dfr(1:npaths, ~ .future_paths(utility_fun = utility_fun, prod_fun = prod_fun, 
                                      fwd = fwd - 1, 
                                      npaths = npaths, 
                                      x = out[.x], 
                                      a = a + possible_paths[.x], 
                                      util = util[.x],
                                      path = paste0(path, .x)))
  }
}
# .future_paths <- loop_transform(.future_paths)

future_output <- function(utility_fun, prod_fun, fwd, npaths, x, a) {
  # The possible base paths that .future_paths will use
  possible_paths <- seq(0, x-1, length.out = npaths)
  
  .future_paths(utility_fun = utility_fun, prod_fun = prod_fun, 
                fwd = fwd, npaths = npaths,  x = x, a = a) %>% 
    separate(path, letters[1:npaths], 1:npaths) %>% 
    filter(util == max(util)) %>% 
    pull(a) %>% 
    as.numeric() %>% 
    possible_paths[.]
}