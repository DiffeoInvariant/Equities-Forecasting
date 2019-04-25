source("EquitiesData.R")

#import EquitiesData.R

#Fama-French factors
make_linmod_exog <- function( start,end){
  dat <- get_KFDL_3Factor(start,end)
  dropcols <- c('Date', 'RF')
  dat <- dat[,!(names(dat) %in% dropcols)]
  dat <- standardize_cols(dat, 3)
  constCol <- as.data.frame(matrix(rep(1.0, nrow(dat)), ncol = 1))
  dat <- cbind(constCol, dat)
  return(dat)
}
make_standardized_price <- function(ticker, stock_data, isRet = FALSE){
  stock <- dplyr::filter(stock_data,symbol == ticker)
  price <- stock$'adjusted'
  if(isRet){
    ret <- price_to_ret(price)
    ret <- (ret - mean(ret))/sd(ret)
  } else{
    return(price)
  }
  
}

linmod_residuals <- function(fitted_vals, true_values){
  true_values <- as.vector(true_values)
  resid <- as.double(1:length(fitted_vals))
  for(i in 1:length(fitted_vals)){
    resid[i] <- fitted_vals[i] - true_values[i] 
  }
  return(resid)
}
linmod_square_residuals <- function(fitted_vals, true_values){
  resid <- as.double(1:length(fitted_vals))
  
  for(i in 1:length(fitted_vals)){
    resid[i] <- (fitted_vals[i] - true_values[i])^2
  }
  return(resid)
}

#param retSeries: time series vector (NOT ts or xts object) of returns
#param lags: vector of ints where each number corresponds to a column of returns lagged by that many days
#param prevPeriodLens: vector of ints, each being lengths of returns periods to calculate; for example, if
#prevPeriodLens = c(5,10,20), include 5-day, 10-day, and 20-day returns
#Test on priceSeries[maxPeriod+3:end]
xgboost_return_exog <- function(priceSeries, lags, prevPeriodLens, factorDat){
  #make sure it's a vector
  priceSeries <- as.vector(priceSeries)
  maxLag <- max(lags)
  maxPeriod <- max(prevPeriodLens)
  maxb <- 0
  if(maxLag > maxPeriod){
    maxb <- maxLag + 1
  } else{
    maxb <- maxPeriod + 1
  }
  dailyRet <- price_to_period_ret(priceSeries,1)
  olen <- length(priceSeries)
  seriesLen <- olen - maxb + 1
  #we can't use the first start many elements because we need 
  # to compute lage and longer-period returns
  #make data matrix with lags
  exogMat <- as.matrix(apply(factorDat, 2,as.numeric))[(maxb):(seriesLen),]
  for(l in lags){
    #if(l != maxLag){
    lagRet <- price_to_period_ret(priceSeries,1)[(maxb - l):(seriesLen - l)]
    # } else{
    #    lagRet <- price_to_period_ret(priceSeries,1)[1:(seriesLen - l + 1)]
    #}
    exogMat <- cbind(exogMat, lagRet)
  }
  for(p in prevPeriodLens){
    periodRet <- as.vector(price_to_period_ret(priceSeries, p))
    periodRet <- periodRet[(maxb - p ):(seriesLen - p )]
    exogMat <- cbind(exogMat, periodRet)
  }
  exogMat <- as.matrix(exogMat[,-1])
  return(exogMat)
}
#returns binary classification predictions, 1 -> positive return, 0 -> negative return
xgPred <- function(xgMod, predExog){
  preds <- predict(xgMod,as.matrix(predExog))
  #adjPreds <- preds + 0.5 - mean(preds)
  #adjPreds <- as.numeric(adjPreds > 0.5)
  #return(adjPreds)
  return(as.numeric(preds > 0.5))
}
#returns percent correct
xgPredEval <- function(binp, obs){
  toterr <- 0
  for( i in 1:length(binp)){
    if(binp[i] != obs[i]){
      toterr <- toterr + 1
    }
  }
  return(100 - 100*as.double(toterr)/length(binp))
}
xgBinLab <- function(contData){
  return(as.numeric(contData > 0))
}


# Compute R^2 from true and predicted values
Pearson_Rsq <- function(true, predicted) {
  rss <- sum((predicted - true)^2)
  tss <- sum((true - mean(true))^2)
  rsq <- 1 - rss / tss
  return (rsq)
}
linmod_preds <- function(exog, beta, rrFree){
  X <- as.matrix(standardize_cols(exog,3)[,-1])
  constCol <- matrix(rep(1.0, nrow(X)), ncol = 1)
  X <- cbind(constCol, X)
  preds <- X %*% as.matrix(beta) + as.matrix(rrFree)
  return(preds)
}
