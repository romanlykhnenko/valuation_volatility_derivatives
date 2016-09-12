#
# This script performs some descriptive analysis, including assessment of the
# implied volatility structure
#

# load files to be used 
source("packages.R")
source("functions.R")
source("funImpVola.R")

# load data 

# VSTOXX index 
vstoxxIndex <- read.csv("Data/vstoxx_index.csv")

# Options on VSTOXX 
vstoxxOptions <- read.csv("Data/vstoxx_options.csv")

# ESTOXX and VSTOXX
bothIndexes <- read.csv("Data/both_indexes.csv")

# remove NAs
bothIndexes <- na.omit(bothIndexes)

# correlation between returns of VSTOXX and ESTOXX 50
correlationVSTOXXestoxx <- cor(diff(log(bothIndexes$EUROSTOXX)), 
                               diff(log(bothIndexes$VSTOXX)) )

# plot both indexes in one graph -----------------------------------------------
bothIndexes$Date <- as.Date(bothIndexes$Date, "%Y-%m-%d ")
bothIndexes$ScaledVstoxx <- 100*bothIndexes$VSTOXX

ggplot(data=bothIndexes, aes(x=Date))+
  geom_line(aes(y=EUROSTOXX, colour ="EUROSTOXX")) +
  geom_line(aes(y=ScaledVstoxx, colour ="ScaledVstoxx")) +
  scale_colour_manual("", 
                      breaks = c("EUROSTOXX", "ScaledVstoxx"),
                      values = c("EUROSTOXX"="blue", "ScaledVstoxx"="red"))+
  ylab("index value")+
  ggtitle("Underlying and volatility ")+
  ggsave("bothIndex.pdf",  width = 7, height = 4)

# plotting of implied volatility -----------------------------------------------

# select date
date <- "2014-03-31"

# subset data wrt to selected date
plotData <- impVola(date, vstoxxOptions, vstoxxIndex)

# plot implied volatilities
ggplot(plotData, aes(STRIKE, ImpVol, group = MATURITY,
                       colour = MATURITY)) + 
  geom_line(size = 1) +
  labs(x="Strike", y="Implied volatility") +
  ggtitle("Implied volatility as a function of strike") +
  ggsave("2014-03-31vola.pdf",  width = 7, height = 4)

# plot price of the option as observed from the market -------------------------
ggplot(plotData, aes(STRIKE, PRICE, group = MATURITY, colour = MATURITY)) + 
  geom_line(size = 1) + 
  labs(x="Strike", y="Price ") +
  ggtitle("Price observed form the market as a function of strike")
  ggsave("2014-03-31price.pdf", width = 7, height = 4)

# call
ggplot(StrikeValueCall(vstoxxOptions), aes(STRIKE, meanCallValueStrike)) + 
  geom_line() +
  labs(x="strike", y="average value of option")
  ggsave("2014-03-31a.pdf", width = 7, height = 4)

# call
ggplot(ttmValueCall(vstoxxOptions), aes(TTM, meanCallValueTTM)) + 
  geom_line() + 
  labs(x="time to maturity", y="average value of option")
  ggsave("2014-03-31b.pdf", width = 7, height = 4)

# put
ggplot(StrikeValuePut(vstoxxOptions), aes(STRIKE, meanPutValueStrike)) + 
  geom_line() +
  labs(x="strike", y="average value of option")
  ggsave("2014-03-31c.pdf")

# put
ggplot(ttmValuePut(vstoxxOptions), aes(TTM, meanPutValueTTM)) + 
  geom_line() +
  labs(x="time to maturity", y="average value of option")
  ggsave("2014-03-31d.pdf")

