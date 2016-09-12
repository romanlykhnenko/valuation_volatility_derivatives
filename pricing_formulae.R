#
# This file contains functions needed for calculation of values of options and
# futures on volatility index based on GL96 model
#


#
# Model parameters(just for testing of the functions below)
#
V0 = 17.5  # initial level of volatility index
kappa_V = 0.1  # speed of mean reversion
theta_V = 20.0  # long-term index level
sigma_V = 2.0  # volatility of volatility
zeta_V = 0.0  # factor of the expected volatility risk premium
r = 0.01  # risk-free interest rate

#
# Option parameters
#
K = 20.0  # strike
T = 1.0  # time horizon

#
# Formula for futures valuation
#
futuresPrice <-function(V0, kappa_V, theta_V, zeta_V, T){
  
  # Futures pricing formula in GL96 model
  # 
  # Arguments:
  #
  #     V0:       current volatility level
  #     
  #     kappa_V:  mean-reversion factor
  #     
  #     theta_V:  long-run mean of volatility
  #     
  #     zeta_V:   volatility risk premium
  #     
  #     T:        time-to-maturity
  #
  #
  # Output:
  #
  #   price of the future based on GL96 model for a given set of parameters
  
  
  alpha = kappa_V * theta_V
  beta = kappa_V + zeta_V
  price = (alpha / beta * (1 - exp(-beta * T)) + exp(-beta * T) * V0)
  return(price)
  
}

# performance of futuresPrice function
# futuresPrice(V0, kappa_V, theta_V, zeta_V, T)

#
# non-central chi-squared 
#
cx <- function(K, gamma, nu, lambda_V, exact=True){
  
  #  Complementary distribution function of non-central chi-squared density.
  #
  # Args:
  #
  #       K: strike price
  # 
  #       gamma: as defined in the GL96 model
  # 
  #       nu: degrees of freedom
  # 
  #       lambda_V: non-centrality parameter
  # 
  
  out <- (1 - pchisq(gamma * K, nu, lambda_V))
  return(out) 
  
}

#
# GL96 exact call price
#
callPriceGL96 <- function(V0, kappa_V, theta_V, sigma_V, zeta_V, T, r, K){
  
  # Call option pricing formula in GL96 Model
  #
  # Args:
  # 
  #     V0: current volatility level
  # 
  #     kappa_V: mean-reversion factor
  # 
  #     theta_V: long-run mean of volatility
  # 
  #     sigma_V: volatility of volatility
  # 
  #     zeta_V: volatility risk premium
  # 
  #     T: time-to-maturity
  # 
  #     r: risk-free short rate
  # 
  #     K: strike price of the option
  # 
  #
  # Output:
  # 
  #     price of the call option based on GL96 model for a given set of parameters
  
  D = exp(-r * T)  # discount factor
  
  alpha = kappa_V * theta_V 
  beta = kappa_V + zeta_V
  gamma = 4 * beta / (sigma_V ** 2 * (1 - exp(-beta * T)))
  nu = 4 * alpha / sigma_V ** 2
  lambda_V = gamma * exp(-beta * T) * V0
  
  # the pricing formula
  call = (D * exp(-beta * T) * V0 * cx(K, gamma, nu + 4, lambda_V)
          + D * (alpha / beta) * (1 - exp(-beta * T))
          * cx(K, gamma, nu + 2, lambda_V)
          - D * K * cx(K, gamma, nu, lambda_V))
  return (call)
  
}



# price of the call on volatility index
# callPriceGL96(V0, kappa_V, theta_V, sigma_V, zeta_V, T, r, K)





