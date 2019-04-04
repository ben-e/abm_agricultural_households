# Description --------------------------------------------------------------------------------------
# This is a bare minimum simulation. Agents only choose between labor and leisure every period. 
# Agents do not interact.
# Created, BE, 2019-03-23

# Libraries ----------------------------------------------------------------------------------------
library(dplyr)

# Utility and production functions
source("agent_based_agricultural_household_modeling/r/production.R")
source("agent_based_agricultural_household_modeling/r/utility.R")
source("agent_based_agricultural_household_modeling/r/endowment.R")

# Simulation Settings ------------------------------------------------------------------------------
# Number of agents
n_agents <- 40

# Number of simulation rounds
n_periods <- 1

# Production and utility functions
production <- function(k, l) production_cd(a = 1, l = l, beta = 0.9, k = k, alpha = 0.1)
utility <- utility_xyw

# Function to generate labor endowment
labor_endowment <- labor_endowment_gamma

# Function to generate capital endowment
capital_endowment <- capital_endowment_gamma

# Function to generate risk aversion
risk_aversion_dist <- function(n) rgamma(n, 1, 2)

# Optimization function
farmer_opt <- function(capital, labor, risk_aversion, leisure) {
  # Get production
  prod <- production(capital, labor - leisure)
  # Every household member must consume one unit of x, a leisure amount that results in a household
  # member dying will return 0
  if (prod < labor) 0
  else utility(prod, leisure, risk_aversion)
}

# Farmers ------------------------------------------------------------------------------------------
# Farmers will be represented by rows in a data frame.

farmers <- tibble(
  # endowment of capital, using
  capital = capital_endowment(n_agents),
  # endowment of labor (household size)
  labor = ceiling(labor_endowment(n_agents)),
  # endowment of risk aversion
  risk_aversion = risk_aversion_dist(n_agents),
  # leisure decision, must be less than labor
  leisure = 0,
  # production output from labor, leisure, and capital
  production = 0,
  # utility from production and leisure
  utility = 0
)

# Simulation ---------------------------------------------------------------------------------------
for (period in 1:n_periods) {
  for (farmer in 1:nrow(farmers)) {
    # Farmers begin the period by choosing the optimal leisure amount (note: optim minimizes)
    farmers$leisure[farmer] <- optim(
      c(0), 
      function(leisure) -farmer_opt(capital = farmers$capital[farmer], 
                                   labor = farmers$labor[farmer],
                                   risk_aversion = farmers$risk_aversion[farmer],
                                   leisure = leisure), 
      lower = 0, upper = farmers$labor[farmer],
      method = "L-BFGS-B")$par
  }
  
  # These are vectorized :)
  # Now actual production
  farmers$production <- production(farmers$capital, farmers$labor - farmers$leisure)
  # And utility
  farmers$utility <- utility(farmers$production, farmers$leisure, farmers$risk_aversion)
  print(farmers)
}
