#
# This script performs calibration of the GL 1996 model to the real data #######
#


# load required files and libraries
source("pricing_formulae.R")
source("packages.R")


# load the data
vstoxxOptions31032014 <- read.csv("Data/vstoxx_options_31032014.csv")

# coerce to data format
vstoxxOptions31032014$MATURITY <- as.Date(vstoxxOptions31032014$MATURITY, 
                                          format = "%Y-%m-%d ")

# global variables #############################################################

# tolerance level to define moneyness
tol = 0.25

# VSTOXX index at 31.03.2014
V0 = 17.6639  

# volatility risk premium factor
zeta_V = 0

################################################################################

# select options close enough to the ATM level
optionData <- vstoxxOptions31032014[(vstoxxOptions31032014$STRIKE > (1-tol)*V0)&
                        (vstoxxOptions31032014$STRIKE < (1+tol)*V0),]

# print unique maturities
unique(optionData$MATURITY)

# select one maturity date from unique maturities
maturitySelected <- "2014-05-16"

optionData <- optionData[optionData$MATURITY == maturitySelected,]

ttm <- optionData$TTM # global variable to be used in functions

strikes <- optionData$STRIKE # global variable to be used in functions

valuationFunction <- function(kappa_V, theta_V, sigma_V){
  
  # Args:
  #
  #   kappa_V: parameter in the GL96 model
  #
  #   theta_V: parameter in the GL96 model
  # 
  #   sigma_V: parameter in the GL96 model
  #
  # Output:
  #
  #   vector of the call prices for different ttm&strike, calculated based
  #   on GL96 model
  
  callPrice <- rep(0,length(ttm))
  
  for (i in 1:length(ttm)){
    callPrice[i] <- callPriceGL96(V0, kappa_V, theta_V, sigma_V, zeta_V, 
                                  ttm[i], r, strikes[i])
  }
  
  return(callPrice)
}

# performance of the valuationFunction
valuationFunction(kappa_V, theta_V, sigma_V)

mseFunction <- function(kappaV_thetaV_sigmaV){  
  
  #
  # calculation of the MSE
  #
  # Args:
  #
  #       kappaV_thetaV_sigmaV: vector, where 
  #              kappaV_thetaV_sigmaV[1] contains kappa_V
  #              kappaV_thetaV_sigmaV[2] contains theta_V
  #              kappaV_thetaV_sigmaV[3] contains sigma_V
  #
  # Output:
  #
  #              mean squared error between real and model based option prices
  
  kappa_V <- kappaV_thetaV_sigmaV[1] 
  theta_V <- kappaV_thetaV_sigmaV[2] 
  sigma_V <- kappaV_thetaV_sigmaV[3] 
  
  
  # call prices calculated based on  the model
  callPrices <- valuationFunction(kappa_V, theta_V, sigma_V)
  
  # call prices observed from the market
  callQuotes <- optionData$PRICE
  
  # mean squared error
  MSE <- sum((callPrices - callQuotes)^2/length(callQuotes))
  
  return(MSE)
} 

kappa_V_theta_V_sigma_V <- c(5,20,4)

mseFunction(kappa_V_theta_V_sigma_V)

# solve optimization problem, and obtain optimal parameters
optimalParameters <- optim(c(5.0, 20.1, 1.0), mseFunction)$par

# calculate option prices based on the model, and optinal parameters
model_prices = valuationFunction(optimalParameters[1], optimalParameters[2],
                                 optimalParameters[3])

# combine real and model values in one data frame
real_predictedOption <- data.frame(real=optionData$PRICE,
                                   predicted=model_prices,
                                   strike=strikes)
# MSE
mse <- sum((real_predictedOption$real-
          real_predictedOption$predicted)^2)/length(real_predictedOption)

# plotting procedure
ggplot(data=real_predictedOption, aes(x=strike))+
  geom_line(aes(y=predicted, colour ="model values")) +
  geom_point(aes(y=real, colour ="market values")) +
  scale_colour_manual("", 
                      breaks = c("model values", "market values"),
                      values = c("model values"="green", "market values"="red"))+
  ylab("option values")+
  ggtitle("market and model option price")+
  ggsave(paste0(maturitySelected,".pdf"), width = 7, height = 4)



