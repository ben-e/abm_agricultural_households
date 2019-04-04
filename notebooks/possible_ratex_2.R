utility <- function(x, r = 0.05) -exp(-(x)*r) + 1
production <- function(a, l, beta = 0.85, k, alpha = 0.15) a*(l^beta)*(k^alpha)
production_ <- function(a, l = 5, beta = 0.85, k = 5, alpha = 0.15) a*(l^beta)*(k^alpha)

# --------------------------------------------------------------------------------------------------
explore_future <- function(ai, a, x, util_f, prod_f, period, start_period = period, util_so_far = 0) {
  # Base case
  if (period == 0) {
    print(paste0("util_so_far: ", util_so_far))
    return(util_so_far)
  } else {
    # Utility in this period with investment
    util <- util_f(x - ai)
    # Production in next period
    prod <- prod_f(ai + a)
    
    # Now optimize next period
    opt <- optim(
      par = c(0), 
      fn = function(ai) explore_future(ai, a = a, x = prod, util_f = util_f, prod_f = prod_f,
                                       period = period - 1, start_period = start_period, 
                                       util_so_far = util + util_so_far),
      lower = 0, upper = prod,
      method = "L-BFGS-B"
    )
    
    if (period == start_period) {
      print(opt)
      opt$par
    }
    else
      opt$value
  }
}

explore_future(ai = 0, a = 1, x = 5,util_f = utility, prod_f = production_, period = 3)

# --------------------------------------------------------------------------------------------------

be_explore_future <- explore_future <- function(ai, a, x, util_f, prod_f, period, util_so_far = 0) {
  if (period == 0){
    return(util_so_far)
  } else {
    # Utility in this period with investment
    util <- util_f(x - ai)
    # Production in next period
    prod <- prod_f(ai + a)
    a_new <- a + ai
    
    # Now explore all futures
    purrr::map(
      prod*((0:100)/100), 
      function(ai) {
        be_explore_future(ai = ai, a = a_new, x = prod,util_f = utility, prod_f = production_, 
                          period = period - 1, util_so_far = util_so_far + util)
      }) %>% max()
  }
}

be_explore_future(ai = 0, a = 1, x = 5, util_f = utility, prod_f = production_, period = 3)

# --------------------------------------------------------------------------------------------------
# Not recursive
# This seems like the best option so far, clean up and rewrite in C++
new_explore_future <- function(a, x, util_f, prod_f, look_ahead) {
  # Get permutations of investment in every time period considered
  future_tree <- do.call(expand.grid, rep(list(seq(0, 100, 1)), look_ahead))
  names(future_tree) <- paste0("invest_", 1:look_ahead)
  future_tree$output_1 <- x
  future_tree$tech_0 <- a
  
  for (branch in 1:nrow(future_tree)) {
    for (time in 1:look_ahead) {
      # Investment which will be realized in the next period
      x_inv <- future_tree[ , paste0("output_", time)]*(future_tree[ , paste0("invest_", time)]/100)
      # Utility in this period
      future_tree[ , paste0("util_", time)] <- util_f(future_tree[ , paste0("output_", time)] - x_inv)
      
      # Technology for next period
      future_tree[ , paste0("tech_", time)] <- future_tree[ , paste0("tech_", time - 1)] + future_tree[ , paste0("output_", time)]*(future_tree[ , paste0("invest_", time)]/100)
      # output for next period
      if (time + 1 <= look_ahead)
        future_tree[ , paste0("output_", time + 1)] <- prod_f(future_tree[ , paste0("tech_", time)])
    }
  }
  # future_tree$total_utility <- rowSums(test[, paste0("util_", 1:look_ahead)])
  future_tree
}

new_explore_future(a = 1, x = 5, util_f = utility, prod_f = production_, look_ahead = 2)
