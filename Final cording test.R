################################
## Created by Kazuhiko Fukunaga
################################
### Hult Business School
## Wealth and Investment Management Risk in R
### Professor Thomas Kurnicki
## Fisher Asset Portfolio
##############################################

# Need to instal the quantmod package
#install.packages("quantmod")
library(quantmod) #loading the quantmod package
##############################################
#step 1:
#downloading pricing data from the yahoo finance portal
stock1 <- getSymbols("AAPL", auto.assign=FALSE)
stock2 <- getSymbols("MSFT", auto.assign=FALSE)
stock3 <- getSymbols("AMZN", auto.assign=FALSE)
stock4 <- getSymbols("V", auto.assign=FALSE)
etf5 <- getSymbols("VCIT", auto.assign=FALSE)
stock6 <- getSymbols("GOOGL", auto.assign=FALSE)
stock7 <- getSymbols("BABA", auto.assign=FALSE)
stock8 <- getSymbols("TSM", auto.assign=FALSE)
stock9 <- getSymbols("ADBE", auto.assign=FALSE)
stock10 <- getSymbols("TCEHY", auto.assign=FALSE)


#Step2
#now we want to join all the 3 different securities into one data frame: 
joined_prices <- merge.xts(stock1,
                           stock2,
                           stock3,
                           stock4,
                           etf5,
                           stock6,
                           stock7,
                           stock8,
                           stock9,
                           stock10)
#Step3:
#but we just need the closing prices - which are in the 4th, 10th and 16th variables:
joined_prices_only <- joined_prices[,c(6,12,18,24,30,36,42,48,54,60)]
#can we code the above differently? 

#Step4:
#we want to calculate daily ROR by using the log() function that calculates natural logarithm:
library(dplyr)

joined_returns <- as.data.frame(joined_prices_only)%>%
  mutate(AAPL_ROR = log(AAPL.Adjusted/lag(AAPL.Adjusted)))%>%
  mutate(MSFT_ROR = log(MSFT.Adjusted/lag(MSFT.Adjusted)))%>%
  mutate(AMZN_ROR = log(AMZN.Adjusted/lag(AMZN.Adjusted)))%>%
  mutate(V_ROR = log(V.Adjusted/lag(V.Adjusted)))%>%
  mutate(VCIT_ROR = log(VCIT.Adjusted/lag(VCIT.Adjusted)))%>%
  mutate(GOOGL_ROR = log(GOOGL.Adjusted/lag(GOOGL.Adjusted)))%>%
  mutate(BABA_ROR = log(BABA.Adjusted/lag(BABA.Adjusted)))%>%
  mutate(TSM_ROR = log(TSM.Adjusted/lag(TSM.Adjusted)))%>%
  mutate(ADBE_ROR = log(ADBE.Adjusted/lag(ADBE.Adjusted)))%>%
  mutate(TCEHY_ROR = log(TCEHY.Adjusted/lag(TCEHY.Adjusted)))

#Step5:
#Do we need to Compound the returns? Do them monthly? quarterly? annually? (if we go beyond 12 months then we need to annualize) 
n <- 25 #how many days do we need to combine

joined_returns <- as.data.frame(joined_prices_only)%>%
  mutate(AAPL_ROR = log(AAPL.Adjusted/lag(AAPL.Adjusted, n )))%>%
  mutate(MSFT_ROR = log(MSFT.Adjusted/lag(MSFT.Adjusted, n )))%>%
  mutate(AMZN_ROR = log(AMZN.Adjusted/lag(AMZN.Adjusted, n )))%>%
  mutate(V_ROR = log(V.Adjusted/lag(V.Adjusted, n )))%>%
  mutate(VCIT_ROR = log(VCIT.Adjusted/lag(VCIT.Adjusted, n )))%>%
  mutate(GOOGL_ROR = log(GOOGL.Adjusted/lag(GOOGL.Adjusted, n )))%>%
  mutate(BABA_ROR = log(BABA.Adjusted/lag(BABA.Adjusted, n )))%>%
  mutate(TSM_ROR = log(TSM.Adjusted/lag(TSM.Adjusted, n )))%>%
  mutate(ADBE_ROR = log(ADBE.Adjusted/lag(ADBE.Adjusted, n )))%>%
  mutate(TCEHY_ROR = log(TCEHY.Adjusted/lag(TCEHY.Adjusted, n )))

#In wealth management - we usually have monthly data and then we aggregate to 1Y, 3Y, 5Y performance.

#monthly 
stock1_returns <- monthlyReturn(getSymbols("AAPL", auto.assign=FALSE))
stock2_returns <- monthlyReturn(getSymbols("MSFT", auto.assign=FALSE))
stock3_returns <- monthlyReturn(getSymbols("AMZN", auto.assign=FALSE))
stock4_returns <- monthlyReturn(getSymbols("V", auto.assign=FALSE))
etf5_returns <- monthlyReturn(getSymbols("VCIT", auto.assign=FALSE))
stock6_returns <- monthlyReturn(getSymbols("GOOGL", auto.assign=FALSE))
stock7_returns <- monthlyReturn(getSymbols("BABA", auto.assign=FALSE))
stock8_returns <- monthlyReturn(getSymbols("TSM", auto.assign=FALSE))
stock9_returns <- monthlyReturn(getSymbols("ADBE", auto.assign=FALSE))
stock10_returns <- monthlyReturn(getSymbols("TCEHY", auto.assign=FALSE))

#merging returns(Step2)
joined_monthlyreturns <- merge.xts(stock1_returns, 
                                   stock2_returns, 
                                   stock3_returns,
                                   stock4_returns,
                                   etf5_returns,
                                   stock6_returns,
                                   stock7_returns,
                                   stock8_returns,
                                   stock9_returns,
                                   stock10_returns)

colnames(joined_monthlyreturns) <- c("AAPL_returns", 
                                     "MSFT_returns",
                                     "AMZN_returns", 
                                     "V_returns", 
                                     "VCIT_returns",
                                     "GOOGL_returns", 
                                     "BABA_returns", 
                                     "TSM_returns", 
                                     "ADBE_returns", 
                                     "TCEHY_returns")



##############################################################
##############################################################
##############################################################
### Calculating different risk metrics #######################
##############################################################
##############################################################
##############################################################

#before we go into risk metrics, lets calculate portfolio returns
# for all the stocks together, so we need to know asset allocation

AAPL_alloc <-0.1766
MSFT_alloc <-0.1299
AMZN_alloc <-0.1292
V_alloc <-0.1113
VCIT_alloc <-0.0963
GOOGL_alloc <-0.09436
TSM_alloc <-0.0725
BABA_alloc <-0.0725
ADBE_alloc <-0.0656
TCEHY_alloc <-0.0646

#these have to sum to 1

joined_portfolio_ret <- as.data.frame(joined_monthlyreturns)%>%
  mutate(portfolio = AAPL_alloc * AAPL_returns +
           MSFT_alloc * MSFT_returns +
           AMZN_alloc * AMZN_returns +
           V_alloc * V_returns +
           VCIT_alloc * VCIT_returns +
           GOOGL_alloc * GOOGL_returns +
           BABA_alloc * BABA_returns +
           TSM_alloc * TSM_returns +
           ADBE_alloc * ADBE_returns +
           TCEHY_alloc * TCEHY_returns)
#let's take a look at the portfolio returns timeseries:
plot(joined_portfolio_ret$portfolio, type="l")
#now this is semi-stationary. It has the same mean accross all the series, and different sigma here and there. 

#############################################################
##### Adding a benchmark before we do risk analysis##########
##### We'll use Vanguard Russell 1000 ETF (VONE) ############
#############################################################


##### Adding a benchmark before we do risk analysis########## 
benchmark_returns1 <- monthlyReturn(getSymbols("FNGS", auto.assign = FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, 
                                   stock2_returns, 
                                   stock3_returns,
                                   stock4_returns,
                                   etf5_returns,
                                   stock6_returns,
                                   stock7_returns,
                                   stock8_returns,
                                   stock9_returns,
                                   stock10_returns, 
                                   benchmark_returns1)

colnames(joined_monthlyreturns) <- c("AAPL_returns", 
                                     "MSFT_returns",
                                     "AMZN_returns", 
                                     "V_returns", 
                                     "VCIT_returns",
                                     "GOOGL_returns", 
                                     "BABA_returns", 
                                     "TSM_returns", 
                                     "ADBE_returns", 
                                     "TCEHY_returns",
                                     "Benchmark_FNGS_returns")


################################
#let's go back to calculating risk for each individual security
#################################
##############
## sigma for last 12 months (need to annualize by sqrt(12))
##############

time_index <- nrow(joined_monthlyreturns)
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)


AAPL_sigma <- sd(joined_monthlyreturns$AAPL_returns[(time_index-11) : time_index]) * sqrt(12) 
MSFT_sigma <- sd(joined_monthlyreturns$MSFT_returns[(time_index-11) : time_index]) * sqrt(12)
AMZN_sigma <- sd(joined_monthlyreturns$AMZN_returns[(time_index-11) : time_index]) * sqrt(12)
V_sigma <- sd(joined_monthlyreturns$V_returns[(time_index-11) : time_index]) * sqrt(12)
VCIT_sigma <- sd(joined_monthlyreturns$VCIT_returns[(time_index-11) : time_index]) * sqrt(12)
GOOGL_sigma <- sd(joined_monthlyreturns$GOOGL_returns[(time_index-11) : time_index]) * sqrt(12)
BABA_sigma <- sd(joined_monthlyreturns$BABA_returns[(time_index-11) : time_index]) * sqrt(12)
TSM_sigma <- sd(joined_monthlyreturns$TSM_returns[(time_index-11) : time_index]) * sqrt(12)
ADBE_sigma <- sd(joined_monthlyreturns$ADBE_returns[(time_index-11) : time_index]) * sqrt(12)
TCEHY_sigma <- sd(joined_monthlyreturns$TCEHY_returns[(time_index-11) : time_index]) * sqrt(12)
benchmake_FNGS_sigma <- sd(joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

print(c(AAPL_sigma,
        MSFT_sigma,
        AMZN_sigma, 
        V_sigma,
        VCIT_sigma,
        GOOGL_sigma,
        BABA_sigma, 
        TSM_sigma,
        ADBE_sigma,
        TCEHY_sigma,
        benchmake_FNGS_sigma))

################################
### beta for the last 12 months
################################
time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame
last_12_months <- joined_monthlyreturns[(time_index-11) : time_index, ]

##we will use the RUS1000 as the benchmark to regress against
# linear model
AAPL_reg <- lm(AAPL_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(AAPL_reg)

MSFT_reg <- lm(MSFT_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(MSFT_reg)

AMZN_reg <- lm(AMZN_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(AMZN_reg)

V_reg <- lm(V_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(V_reg)

VCIT_reg <- lm(VCIT_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(VCIT_reg)

GOOGL_reg <- lm(GOOGL_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(GOOGL_reg)

BABA_reg <- lm(BABA_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(BABA_reg)

TSM_reg <- lm(TSM_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(TSM_reg)

ADBE_reg <- lm(ADBE_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(ADBE_reg)

TCEHY_reg <- lm(TCEHY_returns ~ Benchmark_FNGS_returns ,data=last_12_months)  
summary(TCEHY_reg)

##############
## TrackingError for last 12 months (need to annualize by sqrt(12))
##############

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)#need to convert to data frame to be able to subset based on indexing

AAPL_te <- sd(joined_monthlyreturns$AAPL_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

MSFT_te <- sd(joined_monthlyreturns$MSFT_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

AMZN_te <- sd(joined_monthlyreturns$AMZN_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

V_te <- sd(joined_monthlyreturns$V_returns[(time_index-11) : time_index]-
             joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

VCIT_te <- sd(joined_monthlyreturns$VCIT_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

GOOGL_te <- sd(joined_monthlyreturns$GOOGL_returns[(time_index-11) : time_index]-
                 joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

BABA_te <- sd(joined_monthlyreturns$BABA_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

TSM_te <- sd(joined_monthlyreturns$TSM_returns[(time_index-11) : time_index]-
               joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

ADBE_te <- sd(joined_monthlyreturns$ADBE_returns[(time_index-11) : time_index]-
                joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)

TCEHY_te <- sd(joined_monthlyreturns$TCEHY_returns[(time_index-11) : time_index]-
                 joined_monthlyreturns$Benchmark_FNGS_returns[(time_index-11) : time_index]) * sqrt(12)



print(c(AAPL_te,
        MSFT_te,
        AMZN_te,
        V_te,
        VCIT_te,
        GOOGL_te,
        BABA_te,
        TSM_te,
        ADBE_te,
        TCEHY_te))

#########################################################################
#########################################################################
######## Calculating sigma and beta and TE for entire portfolio##########
#########################################################################

#we've seen this code before:
AAPL_alloc <-0.1766
MSFT_alloc <-0.1299
AMZN_alloc <-0.1292
V_alloc <-0.1113
VCIT_alloc <-0.0963
GOOGL_alloc <-0.09436
TSM_alloc <-0.0725
BABA_alloc <-0.0725
ADBE_alloc <-0.0656
TCEHY_alloc <-0.0646
#these have to sum to 1

#creating our portfolio returns viable: 
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = AAPL_alloc * AAPL_returns +
           MSFT_alloc * MSFT_returns +
           AMZN_alloc * AMZN_returns +
           V_alloc * V_returns +
           VCIT_alloc * VCIT_returns +
           GOOGL_alloc * GOOGL_returns +
           BABA_alloc * BABA_returns +
           TSM_alloc * TSM_returns +
           ADBE_alloc * ADBE_returns +
           TCEHY_alloc * TCEHY_returns)

#now calculating sigma for the portfolio variable:

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame need this to pick up that last month in df
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) #need to convert to data frame to be able to subset based on indexing
portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11) : time_index]) * sqrt(12) 

#running regression for last 12 months between portfolio and RUS1000 benchmark 
portfolio_vs_FNGS_reg <- lm(portfolio ~  Benchmark_FNGS_returns,data=joined_monthlyreturns[((time_index-11) : time_index),])  #we run a linear regression with the bencharmk returns (RUS1000) as X and asset returns as Y
summary(portfolio_vs_FNGS_reg)


#tracking error between portfolio and benchmark:
portfolio_te <- sd(joined_monthlyreturns$portfolio[(time_index-11) : time_index]-joined_monthlyreturns$portfolio[(time_index-11) : time_index]) * sqrt(12)

print(portfolio_te)
##############################################################
##############################################################
##############################################################
### Sharpe and Treynor risk metrics    #######################
##############################################################
##############################################################
##############################################################

#Calculating Sharpe for single securities and for the entire portfolio:
# Share ratio will have a 12M expected return, and a 12M sigma
# the risk free rate is 0.001
riskfree <- 0.001

#we've calculated the expectation using the mean() function

AAPL_sharpe <- (mean(joined_monthlyreturns$AAPL_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$AAPL_returns[(time_index-11) : time_index])

MSFT_sharpe <- (mean(joined_monthlyreturns$MSFT_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$MSFT_returns[(time_index-11) : time_index])

AMZN_sharpe <- (mean(joined_monthlyreturns$AMZN_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$AMZN_returns[(time_index-11) : time_index])

VCIT_sharpe <- (mean(joined_monthlyreturns$VCIT_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$VCIT_returns[(time_index-11) : time_index])

GOOGL_sharpe <- (mean(joined_monthlyreturns$GOOGL_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$GOOGL_returns[(time_index-11) : time_index])

BABA_sharpe <- (mean(joined_monthlyreturns$BABA_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$BABA_returns[(time_index-11) : time_index])

TSM_sharpe <- (mean(joined_monthlyreturns$TSM_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$TSM_returns[(time_index-11) : time_index])

ADBE_sharpe <- (mean(joined_monthlyreturns$ADBE_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$ADBE_returns[(time_index-11) : time_index])

TCEHY_sharpe <- (mean(joined_monthlyreturns$TCEHY_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$TCEHY_returns[(time_index-11) : time_index])

V_sharpe <- (mean(joined_monthlyreturns$V_returns[(time_index-11) : time_index])-riskfree ) / 
  sd(joined_monthlyreturns$V_returns[(time_index-11) : time_index])

portfolio_sharpe <- (mean(joined_monthlyreturns$portfolio[(time_index-11) : time_index])-riskfree ) / portfolio_sigma

print(c(AAPL_sharpe,
        MSFT_sharpe,
        AMZN_sharpe,
        V_sharpe,
        VCIT_sharpe,
        GOOGL_sharpe,
        BABA_sharpe,
        TSM_sharpe,
        ADBE_sharpe,
        TCEHY_sharpe,
        portfolio_sharpe))

#we'l calculate treynor's ratio for the same: 
  AAPL_treynor <- (mean(joined_monthlyreturns$AAPL_returns[(time_index-11) : time_index])-riskfree ) / AAPL_reg$coefficients[2] 
  
  MSFT_treynor <- (mean(joined_monthlyreturns$MSFT_returns[(time_index-11) : time_index])-riskfree ) / MSFT_reg$coefficients[2] 
  
  AMZN_treynor <- (mean(joined_monthlyreturns$AMZN_returns[(time_index-11) : time_index])-riskfree ) / AMZN_reg$coefficients[2] 
  
  V_treynor <- (mean(joined_monthlyreturns$V_returns[(time_index-11) : time_index])-riskfree ) / V_reg$coefficients[2] 
  
  VCIT_treynor <- (mean(joined_monthlyreturns$VCIT_returns[(time_index-11) : time_index])-riskfree ) / VCIT_reg$coefficients[2] 
  
  GOOGL_treynor <- (mean(joined_monthlyreturns$GOOGL_returns[(time_index-11) : time_index])-riskfree ) / GOOGL_reg$coefficients[2] 
  
  BABA_treynor <- (mean(joined_monthlyreturns$BABA_returns[(time_index-11) : time_index])-riskfree ) / BABA_reg$coefficients[2] 
  
  TSM_treynor <- (mean(joined_monthlyreturns$TSM_returns[(time_index-11) : time_index])-riskfree ) / TSM_reg$coefficients[2] 
  
  ADBE_treynor <- (mean(joined_monthlyreturns$ADBE_returns[(time_index-11) : time_index])-riskfree ) / ADBE_reg$coefficients[2] 
  
  TCEHY_treynor <- (mean(joined_monthlyreturns$TCEHY_returns[(time_index-11) : time_index])-riskfree ) / TCEHY_reg$coefficients[2] 
  
  portfolio_treynor <- (mean(joined_monthlyreturns$portfolio[(time_index-11) : time_index])-riskfree ) / portfolio_vs_FNGS_reg$coefficients[2]
  
  print(c(AAPL_treynor,
          MSFT_treynor,
          AMZN_treynor,
          V_treynor,
          VCIT_treynor,
          GOOGL_treynor,
          BABA_treynor,
          TSM_treynor,
          ADBE_treynor,
          TCEHY_treynor,
          portfolio_treynor))
  
  
  #Portfolio returns recommendation
  AAPL_alloc <-0.1766
  MSFT_alloc <-0.1299+0.02
  AMZN_alloc <-0.1292
  V_alloc <-0.1113
  VCIT_alloc <-0.0963
  GOOGL_alloc <-0.09436+0.04
  TSM_alloc <-0.0725
  BABA_alloc <-0.0725
  ADBE_alloc <-0.0656
  TCEHY_alloc <-0.0646-0.6
  
  joined_portfolio_ret_rec <- as.data.frame(joined_monthlyreturns)%>%
    mutate(portfolio = AAPL_alloc * AAPL_returns +
             MSFT_alloc * MSFT_returns +
             AMZN_alloc * AMZN_returns +
             V_alloc * V_returns +
             VCIT_alloc * VCIT_returns +
             GOOGL_alloc * GOOGL_returns +
             BABA_alloc * BABA_returns +
             TSM_alloc * TSM_returns +
             ADBE_alloc * ADBE_returns +
             TCEHY_alloc * TCEHY_returns)
  
