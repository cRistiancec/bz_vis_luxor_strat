# Part 1 - Data Visualization
# In the first part of this project, my objective was to get familiar with my dataset of Brent crude futures (historical prices) using baseR and the Quandl and ggplot packages.
library(Quandl)
library(tidyverse)
library(ggthemes)
library(quantmod)
library(quantstrat)
library(knitr)
library(blotter)

Quandl.api_key("httWUrzTVAL6RdqaHqSf")

brent <- Quandl('CHRIS/CME_BZ1') # calls first month contract for Brent crude futures on CME exchange
head(brent, n=10)

# WSJ-style plot of historical futures closing prices
ggplot(brent, aes(x = Date, y = Last)) +
  geom_line() +
  theme_wsj(color = "gray") +
  theme(axis.title=element_text(size=12)) +
  ggtitle("Brent Crude Futures Closing Prices") +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Cleaning the data
# Arrange data in reverse chronological order (oldest first) and sort by consecutive dates
brent <- brent %>% arrange(rev(rownames(.))) %>% mutate(date = as.Date(Date, "%d-%m-%Y")) %>% arrange(date)
brent$returns <- as.numeric(c('NA',diff(log(brent$Last)))) # Adding a new column with the differences between logs of Last prices
BZ_DATA <- select_if(brent, is.numeric)
# Get the number of NAN (not a number) values by column
na_cols_count <-sapply(BZ_DATA, function(y) sum(length(which(is.na(y)))))
na_cols_count[c("Open", "High", "Low", "Last", "Change", "Settle", "Volume", "Previous Day Open Interest", "returns")]
# This gives how many missing values are in each column. The "Change" column has too many missing values, so we select all columns except this one to incorporate into our working dataset.
BZ_DATA <- select(BZ_DATA,c(1,2,3,4,6,7,8,9))
# To account for the missing data in other columns, we select for only complete cases
BZ_DATA <- BZ_DATA[complete.cases(BZ_DATA),]
summary(BZ_DATA) # Shortcut summary
(statistics <- do.call(data.frame, 
                      list(mean = round(apply(BZ_DATA, 2, mean),4),
                           sd = round(apply(BZ_DATA, 2, sd),4),
                           median = round(apply(BZ_DATA, 2, median),4),
                           min = round(apply(BZ_DATA, 2, min),4),
                           max = round(apply(BZ_DATA, 2, max),4)))) # Better formatted summary

# Visualizing important features of this dataset
returns <- brent$returns
qqnorm(returns, main="BZ Returns") 
qqline(returns, col="red") 
# The qqplots indicate the data is non-normally distributed
# The finding that the futures price data is non-normal is consistent with the fact that futures prices tend to follow a leptokurtic shape (fat tails, high peaks)
ggplot(BZ_DATA, aes(x = returns)) +
  geom_histogram() 
# The histogram confirms the leptokurtic nature of these prices

dates <- brent$date
one_std <- sd(returns,na.rm = TRUE)
two_std <- one_std * 2 
plot(returns ~ dates, type='l',col='blue', main = "Returns over time")
abline(h=c(-one_std,one_std),col='red')
abline(h=c(-two_std,two_std),col='black')
# This plot shows that the Brent futures were most volatile in 2020, which confirms what we know from a fundamentals understanding of how Brent crude futures have been reacting to uncertainty during the COVID-19 pandemic

plot(returns ~ dates, type='l',col='blue', main = "Returns over time (outliers removed)", ylim = c(-0.15, 0.15)) # Removing the -0.3 return still shows high volatility in 2020 compared to previous years
abline(h=c(-one_std,one_std),col='red')
abline(h=c(-two_std,two_std),col='black')

brent_volatility <- brent %>% subset(date > '2010-01-01') %>% mutate(year = substr(brent$date,1,4))
brent_volatility <- brent_volatility[complete.cases(brent_volatility), ]
# We then append a new column to this dataset using standard deviation as a measure of volatility
brent_volatility <- brent_volatility %>% group_by(year) %>% summarise(volatility = sd(returns)) %>% mutate(volatility)
ggplot(brent_volatility, aes(x=year, y=volatility)) +
  geom_col() +
  theme_wsj(color = "gray") +
  theme(axis.title=element_text(size=12)) +
  ggtitle("Volatility in Brent Crude Futures Returns (2014-2020)") +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Visualization using quantmod and tidyquant
brent_vis <- Quandl('CHRIS/CME_BZ1', type = "xts")
candleChart(brent_vis)
addSMA(col="red")
addEMA()
colnames(brent_vis)[4] <- "Close"
brent_vis$date <- time(brent_vis) #Extracts dates as numeric values
# Bollinger Bands and volume
chartSeries(brent_vis, subset="2014::2020",
            theme = chartTheme("white"),
            TA = "addVo(); addBBands(n = 20, sd = 2, draw = 'bands', on = -1)")
zoomChart("2020:", yrange=NULL) # Zooms in to show bands for 2020
# Autocorrelation plots
acf(brent$returns, main = "Autocorrelation of returns", na.action = na.pass)
grid()
acf(brent$Last, main = "Autocorrelation of closing prices", na.action = na.pass)
grid()
# These autocorrelation plots indicate that closing prices are more autocorrelated than returns

# Part 2(a) - Luxor Trading Strategy
# Data
currency('USD')
sym <- na.omit(get(getSymbols("BZ=F"))["2019::2020"])
mkdata <- Cl(sym)
future("BZ=F", currency = "USD", multiplier = 1000)

# Set the parameters
initDate <- "2019-01-01" #Date of initiation 
from <- "2019-01-01" #Start date of the data set
to <- "2020-10-01" #End date of the data set
initEq <- 1e5 #Initial equity of $100,000

# Clear any residuals from previous runs 
rm.strat(portfolio.st)
rm.strat(account.st)

# Assign names to portfolio and account
strategy.st <- portfolio.st <- account.st <- "luxor"

# Initiate portfolio and account
initPortf(portfolio.st, "sym", initDate = initDate) #Initiate portfolio
initAcct(account.st, portfolios = strategy.st, initDate = initDate, initEq = initEq) #Initiate account
initOrders(portfolio = portfolio.st, initDate = initDate) #Initiate account
strategy(strategy.st, store = TRUE) #Store all the events in the strategy

# Add indicators
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mkdata)), # Note these indicators are using closing prices to calculate the moving averages
                               n = 10),
              label = "nFast") # Fast Moving Average ie SMA(10)

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mkdata)),
                               n = 30),
              label = "nSlow") # Slow Moving Average ie SMA(30)

# Add signals 
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"), # The long signal occurs when the fast SMA is > or = the slow SMA
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"), # The short signal occurs when the fast SMA is < the slow SMA
           label = "short")

# Add rules to enter
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.005,
                          prefer = "High", 
                          TxnFees = -10,  # Assuming my broker charges $10 for placing a long order
                          replace = FALSE), # Will not replace any open orders
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

# Add rules to exit
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long", # We exit our long positions when going short
                          ordertype = "market", # Buys the future at the prevailing market price
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE), # Any open orders are replaced
         type = "exit",
         label = "Exit2SHORT")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

# Apply the strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio, account, and equity
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# Part 2(b) - Performance Analysis
# Plot performance
myTheme <- chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart.Posn(strategy.st,theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2)')

# Trade Statistics
tstats <- tradeStats(portfolio.st)
kable(t(tstats))

# Trade Related Statistics
require(dplyr)
tab.trades <- tstats %>% 
  mutate(Trades = Num.Trades, 
         Win.Percent = Percent.Positive, 
         Loss.Percent = Percent.Negative, 
         WL.Ratio = Percent.Positive/Percent.Negative) %>% 
  select(Trades, Win.Percent, Loss.Percent, WL.Ratio)
kable(t(tab.trades))

# Profit Related Statistics
tab.profit <- tstats %>% 
  select(Net.Trading.PL, Gross.Profits, Gross.Losses, Profit.Factor)
kable(t(tab.profit))

# Averages
tab.wins <- tstats %>% 
  select(Avg.Trade.PL, Avg.Win.Trade, Avg.Losing.Trade, Avg.WinLoss.Ratio)

kable(t(tab.wins))

# Performance Summary
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

# Performance Statistics
tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
kable(tab.perf)

# Risk Statistics
tab.risk <- table.Arbitrary(rets,
                            metrics=c(
                              "StdDev.annualized",
                              "maxDrawdown",
                              "VaR",
                              "ES"),
                            metricsNames=c(
                              "Annualized StdDev",
                              "Max DrawDown",
                              "Value-at-Risk",
                              "Conditional VaR"))
kable(tab.risk)

# Order book
(ob <- getOrderBook(portfolio.st))

# Account summary
require(lattice)
a <- getAccount(account.st)
xyplot(a$summary, type = "h", col = 4)

# Equity curve
equity <- a$summary$End.Eq
plot(equity, main = "Equity Curve")

# Account performance summary 
ret <- Return.calculate(equity, method = "log")
charts.PerformanceSummary(ret, colorset = bluefocus, 
                          main = "Strategy Performance")

# Cumulative returns
rets <- PortfReturns(Account = account.st)
chart.CumReturns(rets, colorset = rich10equal, legend.loc = "topleft", 
                 main="SPDR Cumulative Returns")

# Annualized returns
(ar.tab <- table.AnnualizedReturns(rets))