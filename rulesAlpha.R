#' ### FF3
#' Eugene Fama and Ken French have studied market returns for over 5 decades and have developed a number of concepts that expand on the market factor. A seminal paper in {**need reference**} developed a more general regression model by adding two more factors to the market index factor. They did this by designing observable indices that could be replicated in the market by buying or selling short identifiable securities which we call Size and Value indexes.
#' 
#' The Size index is created by taking the return of small cap stocks and subtracting the return of big cap stocks. The actual contruction involves 3 portfolios each for the small cap and big cap portfolios. The exact contruction can be found here at [Description of Fama/French Benchmark Factors](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_bench_factor.html) . This index is designated **SMB** and is indexed parallel to the market index we call $R_{mkt}$ .
#' 
#' The Value index is also parallel to $R_{mkt}$ and is designated HML. This index involves a calculation where high book to market value stock returns are subtracted from low book to market value stock returns. A description can also be found at [Description of Fama/French Benchmark Factors](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_bench_factor.html). 
#' 
#' With our two indices we can now describe the market return of the ith security as
#' 
#' $$R_{i,t} = \alpha_{i} + \beta_{i}R_{mkt,t} + s_{i}SMB_{t}+ h_{i}HML_{t} + \varepsilon_{i,t}$$
#' 
#' We'll use this model form to create an optimized pure alpha portfolio via optimization. We'll need get the  $\beta_i$,  $s_i$, and $h_i$ coefficients for each of the securities in our portfolio.
#' 
#' 
#' 
#' ## Pure $\alpha$ FF3 Optimized Portfolio 
#' To accomplish this we need to execute several steps, which are  (with R function in parenthesis)
#' 
#' -  Load price data for the assets we want to model plus the market index  (getSymbols from quantmod)
#' -  Calculate returns  (ClCl from quantmod)
#' -  Load FF3 factors (code provided)
#' -  Calculate $\beta_i$,  $s_i$, and $h_i$ for each asset versus the market index   (lm from base R)
#' -  Set optimization constraints to eliminate exposure to $\beta_i$,  $s_i$, and $h_i$ plus diversify   (PortfolioAnalytics)
#' -  Calculate the weights, $\omega_i$ for each asset using optimizer  (optimize.portfolio from PortfolioAnalytics)
#' 
#' ### Load Asset Data
#' First we start by loading asset data into R using quantmod. We'll load a number of assets here that we can use later for analyzing commodity CTA strategies plus some international indices but later we will add ETF's that we can use as proxies for some of the laternate strategies we'll be using later.
#' 
#' We also need to load the packages we will be using, so we do that first.
#' 
# Load the required packages
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(downloader)


osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


osInvestAll <- function (data, timestamp, orderqty, ordertype, 
                         orderside, equity, portfolio, symbol, ruletype, ..., initEq) {
  datePos <- format(timestamp,"%Y-%m-%d %H:%M:%OS")
  
  datePos <- strptime(c(datePos), format = "%Y-%m-%d %H:%M:%OS", tz = 
                        "UTC") + 86400 #for daily data
  
  updatePortf(Portfolio=portfolio,Symbol=symbol,Dates=paste0(start(data), 
                                                             "/", datePos))
  # After updating portfolio profit, we can extract the Net.Trading.PL 
  
  trading_pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
  
  equity <- initEq + trading_pl
  ClosePrice <- getPrice(data, prefer = "Close")[datePos]
  UnitSize <- as.numeric((equity / ClosePrice))
  UnitSize1 <- round(UnitSize, digits = 8)
  ifelse(is.na(UnitSize1),0,UnitSize1)
}


osInvestAllShort <- function(...) { -osInvestAll(...)}


#######  add or remove assets here #################################
symbolsCTA <- c("GLD","IAU","USO")

# For example: 
# symbolsCTA <- c("GLD", "IAU", "SLV", "DBC", "USO", 
#              "PPLT", "DBE", "BNO", "UGAZ", "GLL",
#              "PALL", "VXX", "VIXM", "SPY", "QQQ",
#              "EWJ", "EWG", "INDA", "MCHI", "EWZ",
#              "EWQ", "EWH", "FXI", "CORN", "WEAT", "SOYB")
###################################################################

## Set up investment strategy params
init_date <- "2014-01-01"
start_date <- "2014-01-01"
end_date <- "2019-04-11"


getSymbols(Symbols = symbolsCTA, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = TRUE)

# chart_Series(Cl(GLD))
# chart_Series(Cl(SLV))





### Initialize Portfolio
# Here we set up some accounting to keep track of how well or badly we are doing with our strategy.


# everyone get $10MM to start
init_equity <- 10000000 # $10,000,000

currency("USD")
stock(symbolsCTA, 
      currency = "USD", 
      multiplier = 1)


portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"


#################^^ Run once at startup (unless you change assets in symbolsCTA) ^^^^^^^#################
#################^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#################
#########################################################################################################





#########################################################################################################
##### to re-run start here   ############################################################################
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)



initPortf(name = portfolio.st,
          symbols = symbolsCTA,
          initDate = init_date)


initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)


initOrders(portfolio = portfolio.st,
           symbols = symbolsCTA,
           initDate = init_date)

strategy(strategy.st, store = TRUE)


### Set up Inidcator
# We use a fast and a slow Simple Moving Average **(SMA)** in the underlying asset we are looking at as two indicators to compare against each other to decide entry for long and short positions. The speed of the fast/slow averages are kept the same across assets  assets in the portfolio, but there is no reason this needs to be the case. The speed could be asset specific with a bit of programming ((ie ***SMA*** windows are the same for every asset ) )It may be something you might look to optimize).

################################################
# SMA speeds can be modified here
fastSMA=15
slowSMA=45
################################################


# indicators are set here -> do not touch! ######
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = fastSMA),
              label = "nFast")

add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = slowSMA), 
              label = "nSlow")
######################################################



### Set up signal -Do not touch! ##############################################################
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
### Set up signal -Do not touch! ^^^^^^^^^^######################################




### Set up Rules
# Once we have indicators we can add rules we can set up a trading strategy. We need to size the trade so we use the amount of equity we have divided by the number of assets we are working with from our data pull. In this exercise we use the same trade size whether going long or short. We do not update the trade size for changes in our trading account equity.


#### Change trade rules here for all assets ###########

# Trade sizes for long/short

tradeSize=init_equity/length(symbolsCTA)  # allocate capital equally to trade strategies
.orderqty=tradeSize
.txnfees=.threshold=0
stopLimLong=.04
stopLimShort=.015
#######################################################



######  Add rules here - Do not touch! ###########################################




add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', threshold=.threshold,
                        orderqty= .orderqty,
                        tradeSize=tradeSize,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty= -.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="long", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=TRUE,
                          threshold=quote( stopLimLong ),
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='chain', parent="EnterLONG",
         label='StopLossLong',
         enabled=TRUE
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="short", sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='stoplimit',
                          tmult=TRUE,
                          threshold=quote( stopLimShort ),
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='chain', parent="EnterSHORT",
         label='StopLossShort',
         enabled=TRUE
)

######  Add rules here - Do not touch! ##########################################
######^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##########################################


### Get the Results - run to end w/o changing unless you want different output
# Now run the rules/signal/strategies against the data and get the results 
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}

results <- applyStrategy(strategy.st, portfolios = portfolio.st,verbose=FALSE)
updatePortf(portfolio.st,verbose=F)
updateAcct(account.st)
updateEndEq(account.st)


### Save returns as CTAreturns to remember below
CTAreturns<- Return.calculate(end_eq, method="log")



#' Now Load the symbols we want to use for our FF3
######## Choose Assets Here #########################
symbols <- c("GLD", "IAU", "SLV", "DBC", "USO",
             "PPLT", "DBE", "BNO", "UGAZ", "GLL",
             "PALL", "VXX", "VIXM", "QQQ",
             "EWJ", "EWG", "INDA", "MCHI", "EWZ",
             "EWQ", "EWH", "FXI", "CORN", "WEAT",
             "SOYB")

## Set up investment strategy params
init_date <- "2014-01-01"
start_date <- "2014-01-01"
end_date <- "2019-01-01"


### Get  Market Data  ##############################
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = init_date,
           to = end_date,
           adjust = TRUE)


#' ### Load Benchmark Data for $R_{mkt,t}, SMB_{t}, & HML_{t}$ Calculation
#' Now we will load the factor data from Ken French's website where we obtain daily $R_{mkt,t}, SMB_{t}, & HML_{t}$ observations and transform them to an xts object, which is a generalized time series structure in R that we will use throughout:

factorFile <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
download(factorFile, "F-F_Research_Data_Factors_daily_CSV.zip")
unzip(zipfile="F-F_Research_Data_Factors_daily_CSV.zip")
dataFF3 <- read.csv("F-F_Research_Data_Factors_daily.CSV",
skip=4,header=T)
dataFF3=dataFF3[1:length(dataFF3[,1])-1,]
ff3=xts(dataFF3[,2:5]/100,order.by=as.Date(dataFF3[,1],"%Y%m%d"))
head(ff3)

#' ### Calculate $\beta_i$,  $s_i$, and $h_i$ for our Asset Universe
#' Now we have the returns for our index in the ff3 xts data structure, so we can move on to calculate the $R_{mkt,t}, SMB_{t}, & HML_{t}$ for each security by looping over the securtities and regressing them against the benchmark returns using the R function **lm()** and save them into a data frame called **factorCoeffs**. We will also capture the returns calculaated for each asset for use in the optimiztaion in a structure called **rets**
#' 

## Set up dates to calculate over
init_date <- "2014-12-31"
start_date <- "2019-01-01"
end_date <- index(last(ff3))
dRange=paste0(start_date,"/",end_date)
clRange=paste0(init_date,"/",end_date)

### Initialize data frame to capture for  $\beta_i$,  $s_i$, and $h_i$
factorCoefs=data.frame()
rets=data.frame()

clcl=ClCl(SPY)[clRange]
idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])  
rets=clcl
colnames(rets)="SPY"

factorCoefs=data.frame("SPY",fit$coefficients[2],fit$coefficients[3],fit$coefficients[4])
names(factorCoefs)<-c("Symbol","Mkt","SMB","HML")
rownames(factorCoefs)=""

# now we'll do our CTA here
idx=intersect(as.Date(index(CTAreturns)),as.Date(index(ff3)))
###  now fama french fit   ####
fit=lm(CTAreturns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])  
factorCoefs=rbind(factorCoefs,data.frame(Symbol="CTAc",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,CTAreturns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"CTAc")


## Calculate beta's for the assets
for(symbol in symbols){

  clcl=ClCl(get(symbol))[clRange]
  idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
  ###  now fama french fit   ####
  fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
  factorCoefs=rbind(factorCoefs,data.frame(Symbol=symbol,Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
  rets=cbind(rets,clcl)
  colnames(rets)=c(colnames(rets[,-ncol(rets)]),symbol)


}
names(factorCoefs)<-c("Symbol","Mkt","SMB","HML")
rownames(factorCoefs)=c()
rets=rets[complete.cases(rets)]



#' Create portfolio object
pspec <- portfolio.spec(assets=colnames(rets))

#' Box constraint for position size where we can either be fully short or long an asset
lo_constr <- box_constraint(assets=pspec$assets, min=c(-15, .5,rep(-5,ncol(rets)-2)), max=c(0, 10, rep(5,ncol(rets)-2)))

#' Position limit constraints, number of long and short positions allowed, which we set to any
pl_constr <- position_limit_constraint(assets=pspec$assets, max_pos=ncol(rets),max_pos_short = ncol(rets))
lev_constr <- weight_sum_constraint(min_sum=.99, max_sum=2.0)

#' FF3 exposure constraint.
#' The exposure constraint and group constraint are equivalent to test that
#' they result in the same solution.
#' c(MKT,SMB,HML)
lower <- -c(.001, 0.001, 0.001)
upper <- c(.001, 0.001, 0.001)
mf=as.matrix(factorCoefs[,-1])
rownames(mf)=factorCoefs$Symbol
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=mf, lower=lower, upper=upper)

#' Set objective to maximize return.
ret_obj <- return_objective(name="mean")

#' Run optimization on minimum variance portfolio with leverage, long only,
#' and group constraints.
#opta <- optimize.portfolio.rebalancing(R=rets, portfolio=pspec,
opta <- optimize.portfolio(R=rets, portfolio=pspec,

                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(ret_obj), trace=FALSE,
                           optimize_method="pso",verbose = FALSE)
opta

# check results:
myStratRets=xts(rets%*%opta$weights,order.by=index(rets))
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)

### And check ff3 contraint was met in data:mkt <.2 #####
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])  
summary(fit)
#' 
#' Using the data we see that we see that with a mixed long short portfolio of the 25 assets we get an expected positive daily return ($\alpha$) of about .1635% . This would be a pure ($\alpha$) since we constrained the factor weights to zero in order to eliminate any factor loading from our 3 factor model. We did not say anything aboout what risk we are taking to try and achieve this ($\alpha$), so we will redo the analysis and ask the optimizer to limit risk while increasing ($\alpha$) using quadratic utility.


#' Set objective to maximize return and minuimize variance
ret_obj <- return_objective(name="mean")
var_obj <- portfolio_risk_objective(name="var")

#' Run optimization on mean variance with quadratic utility
opta2 <- optimize.portfolio(R=rets, portfolio=pspec,
                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(ret_obj,var_obj), trace=FALSE,
                           optimize_method="pso",verbose = FALSE)
opta2

# check results:
myStratRets=xts(rets%*%opta2$weights,order.by=index(rets))
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)

### And check ff3 contraint was met in data:mkt <.2 #####
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])  
summary(fit)


#' If we change optimizers, from **pso** to **ROI**, we can use the maxSR=TRUE argument to maximize the Sharpe Ratio instead of using quadratic utility.
#' 
opta3 <- optimize.portfolio(R=rets, portfolio=pspec,
                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(ret_obj,var_obj), trace=FALSE,maxSR=TRUE,
                           optimize_method="ROI",verbose = T)
opta3

#check results
myStratRets=xts(rets%*%opta3$weights,order.by=index(rets))
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])  
summary(fit)
#' 
#' Now lets open up the factor weights to be long or short factors and see what happend as we allow ($\alpha$) to include returns on our factor risks,

#' #' FF3 exposure constraint.
#' #' c(MKT,SMB,HML)
lower <- c(-.5, -.1,- 0.1)
upper <- c(0.0, 1.5, 1.5)
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=mf, lower=lower, upper=upper)
opta4 <- optimize.portfolio(R=rets, portfolio=pspec,
                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(ret_obj,var_obj), trace=FALSE,maxSR=TRUE,
                           optimize_method="ROI",verbose = FALSE)
opta4

#' 
#' We see our mean dropped but our standard devaition dropped significantly, yiedling a much higher Sharpre Ratio. Ths is what we expect, as we loosen constraints, our optimization should get better. 
#' 
#' ### Optimizers
#' There are several optimizers available in optimize.portfolio, and some of the objectives are unique to the optimizer selected. The reader is encouraged to read the documentaion for **PortfolioAnalytics** to get familiar with the options available and their capability.
#' 
#' 
#' ## Now lets look at the returns from the last strtategy, calculate sharpe and get factor weights for resulting strategy opta4
myStratRets=xts(rets%*%opta4$weights,order.by=index(rets))
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)

### And check ff3 contraint was met in data:mkt <.2 #####
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])  
summary(fit)