#
# This script contains a set of functions needed for analysis
#


bsCallPrice <- function(S0, K, T, r, sigma) {
  
  
  # function: bsCallPrice
  # 
  # Valuation of European call option in BSM model.
  # Analytical formula.
  #
  # Args:
  #
  #       S0:     initial stock price
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  # 
  # returns:
  #
  #       value : present value of the European call option
  
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  d2 = (log(S0 / K) + (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  value = (S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  
  return(value)
}


bsVega <- function(S0, K, T, r, sigma) {
  
  # function: bsVega
  # 
  # Valuation Vega of the option
  #
  # Args:
  #
  #       S0:     initial stock price
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #       sigma : volatility 
  # 
  # returns:
  #
  #       vega : value of Vega of call option
  
  d1 = (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  vega = S0 * dnorm(d1) * sqrt(T)
  
  return(vega)
  
}

bsCallImpVol <- function(S0, K, T, r, C0, sigmaEst, it=100) {
  
  # function: bsCallImpVol
  # 
  # Calculation of implied volatility of call option
  # using Newton-Raphson method 
  #
  # Args:
  #
  #       S0:     initial stock price
  # 
  #       K :     strike price
  # 
  #       T :     maturity date (in year fractions)
  # 
  #       r :     constant risk-free short rate
  # 
  #      C0 :    call price observed from the market
  #  
  # sigmaEst :    starting value used for approximation of implied volatility
  #
  #        it:    number of iteration used (by default equals 100)
  # 
  # returns:
  #
  #        estimation of implied volatility
  
  for( i in 1:it){
    sigmaEst <- sigmaEst - ((bsCallPrice(S0, K, T, r, sigmaEst) - C0)
                            / bsVega(S0, K, T, r, sigmaEst))
  }
  
  return(sigmaEst)
}

# calculates mean value of call options for all  strikes
StrikeValueCall <- function(vstoxxOptions){
  
  # input:
  #
  #       vstoxxOptions: options on VSTOXX with all characterisitcs
  #
  # output:
  #
  #       StrikeValueCall: data frame with mean value of call options for all  strikes
  
  # add time to maturity column
  vstoxxOptions <- mutate(vstoxxOptions, 
                          TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                            - as.Date(DATE, format = "%Y-%m-%d ") ) /360))
  # calculate mean value of call options for all strikes
  StrikeValueCall <- vstoxxOptions %>%
    dplyr::filter(TTM >= 0) %>%
    dplyr::filter(TYPE == 'C' ) %>%
    group_by(STRIKE) %>%
    summarise(meanCallValueStrike = mean(PRICE))
  
  return(StrikeValueCall)
}

# calculates mean value of call options for all times to maturity
ttmValueCall <- function(vstoxxOptions){
  
  # input:
  #
  #       vstoxxOptions: options on VSTOXX with all characterisitcs
  #
  # output:
  #
  #       ttmValueCall: data frame with mean value of call options for all 
  #                       times to maturity
  
  # add time to maturity column
  vstoxxOptions <- mutate(vstoxxOptions, 
                          TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                            - as.Date(DATE, format = "%Y-%m-%d ") ) /360))
  
  # calculate mean value of call options for all strikes
  ttmValueCall <- vstoxxOptions %>%
    dplyr::filter(TTM >= 0) %>%
    dplyr::filter(TYPE == 'C' ) %>%
    group_by(TTM) %>%
    summarise(meanCallValueTTM = mean(PRICE))
  
  return(ttmValueCall)
}

# calculates mean value of put options for all strikes
StrikeValuePut <- function(vstoxxOptions){
  
  # input:
  #
  #       vstoxxOptions: options on VSTOXX with all characterisitcs
  #
  # output:
  #
  #       StrikeValuePut: data frame with mean value of put options for all strikes
  
  
  # add time to maturity column
  vstoxxOptions <- mutate(vstoxxOptions, 
                          TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                            - as.Date(DATE, format = "%Y-%m-%d ") ) /360))
  
  # calculate mean value of put options for all strikes
  StrikeValuePut <- vstoxxOptions %>%
    dplyr::filter(TTM >= 0) %>%
    dplyr::filter(TYPE == 'P' ) %>%
    group_by(STRIKE) %>%
    summarise(meanPutValueStrike = mean(PRICE))
  
  return(StrikeValuePut)
}

# calculates mean value of put options for all times to maturity
ttmValuePut <- function(vstoxxOptions){
  
  # input:
  #
  #       vstoxxOptions: options on VSTOXX with all characterisitcs
  #
  # output:
  #
  #       ttmValuePut: data frame with mean value of put options for all 
  #                       times to maturity
  
  # add time to maturity column
  vstoxxOptions <- mutate(vstoxxOptions, 
                          TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                            - as.Date(DATE, format = "%Y-%m-%d ") ) /360))
  
  # calculate mean value of put options for all times to maturity
  ttmValuePut <- vstoxxOptions %>%
    dplyr::filter(TTM >= 0) %>%
    dplyr::filter(TYPE == 'P' ) %>%
    group_by(TTM) %>%
    summarise(meanPutValueTTM = mean(PRICE))
  
  return(ttmValuePut)
}







