
impVola <- function(dateOption, vstoxxOptions, vstoxxIndex){
  
  # input:
  #
  #       dateOption: date for which volatility smiles must be ploted
  #
  #       vstoxxOptions: options on VSTOXX with all characterisitcs
  #
  #       vstoxxIndex:  values of VSTOXX index
  #
  # output: 
  #
  #       plotData: data frame containing all required information to plot
  #                 volatility smiles and observed option prices
  
  
  # coerce to Date 
  vstoxxOptions$DATE <- as.Date(vstoxxOptions$DATE, "%Y-%m-%d ")
  
  # coerce to Date
  vstoxxOptions$MATURITY <- as.Date(vstoxxOptions$MATURITY, "%Y-%m-%d ")
  
  # take Options traded on dateOption
  vstoxxOptions3103 <- dplyr::filter(vstoxxOptions, 
                                     as.Date(DATE, format = "%Y-%m-%d ") == 
                                       dateOption)  
  
  # add time to maturity (in years) as a column
  vstoxxOptions3103 = mutate(vstoxxOptions3103, 
                             TTM = as.numeric((as.Date(MATURITY, format = "%Y-%m-%d ")
                                               - as.Date(DATE, format = "%Y-%m-%d ") ) /360))
  
  
  
  # select only call options
  vstoxxOptions3103call = dplyr::filter(vstoxxOptions3103, TYPE == 'C')
  
  # VSTOXX value for a give date
  V0 <- vstoxxIndex %>%
    dplyr::filter(as.Date(Date) == dateOption ) %>%
    select(V2TX)
  
  # coerce dataframe to a number
  V0 <- as.numeric(as.vector(V0))
  
  
  # short rate
  r  <- 0.01
  
  # add column with implied volatilities
  vstoxxOptions3103call = mutate(vstoxxOptions3103call,
                                 ImpVol = bsCallImpVol(V0, STRIKE, TTM, r, 
                                                       PRICE, 2, 100))
  
  # remove rows with NA
  vstoxxOptions3103call <- na.omit(vstoxxOptions3103call)
  
  # select options with non-zero maturity
  plotData <- dplyr::filter(vstoxxOptions3103call, ImpVol > 0)
  
  # coerce to factor
  plotData$MATURITY <- factor(plotData$MATURITY)
  
  return(plotData)
  
}


