library(magrittr)
library(dplyr)
library(parallel)
library(GAS)


ridge_regression_model <- function(ticker, stock_data, exog_data, rf, start,end){
  #use log returns
  stock <- stock_data %>% dplyr::filter(symbol == ticker)
  #use log returns
  ret <- price_to_ret(stock)#[-1]
  ret <- adjusted_rets(rf,ret)
  ret <- (ret - mean(ret))/sd(ret)
  
  model <- glmnet(data.matrix(exog_data), ret,  alpha = 0, standardize = TRUE)
  return(model)
}
ridge_model_selector <- function(ridge_reg){
  best_lambda <- min(ridge_reg$lambda)
  return(best_lambda)
}
#return_type is whether to return the model or predict(model, training data) for fit analysis
make_best_ridge_fit <- function(ticker, stock_data, exog_data, rf, start, end, return_type = c("model","fit")){
  models <- ridge_regression_model(ticker,stock_data, exog_data, rf, start,end)
  lbda <- ridge_model_selector(models)
  
  stock_ret <- stock_data %>% dplyr::filter(symbol == ticker)
  #use log returns
  ret <- price_to_ret(stock_ret)
  ret <- adjusted_rets(rf,ret)
  ret <- (ret - mean(ret))/sd(ret)
  if(length(ret) != length(rf)){
    print("Error, length mismatch. Returning -1.")
    return(-1)
  } else {
    best_mod <- glmnet(data.matrix(exog_data), ret, alpha = 0, lambda = lbda, standardize = TRUE)
    if(return_type == "model"){
      return(best_mod)
    }
    else if(return_type == "fit"){
      return( predict(best_mod, newx = data.matrix(exog_data)))
    }
    else{
      return( list("model" = best_mod, "fit"= predict(best_mod, newx = data.matrix(exog_data) ) ))
    }
  }
}

GLS_regression_model <- function(ticker, stock_data,rf, start,end){
  dat <- make_linmod_exog(start,end)
  #drop constant column
  dat <- dat[,-1]
  #use log returns
  stock <- stock_data %>% dplyr::filter(symbol == ticker)
  #use log returns
  ret <- price_to_ret(stock)#[-1]
  
  ret <- adjusted_rets(rf,ret)
  ret <- (ret - mean(ret))/sd(ret)
  
  model <- glm(ret ~ data.matrix(dat))
  return(model)
  
}

OLS_regression_model <- function(ticker, stock_data, start,end){
  dat <- make_linmod_exog(start,end)
  #use log returns
  stock <- stock_data %>% dplyr::filter(symbol == ticker)
  #use log returns
  ret <- price_to_ret(stock)[-1]
  ret <- adjusted_rets(get_KFDL_3Factor(start,end),ret)
  ret <- (ret - mean(ret))/sd(ret)
  
  model <- lm(ret ~ data.matrix(dat))
  return(model)
  
}
#stock_data is dataframe of all equities
#start is first date as an integer, yearMonthDay
#end is last date in same format
#model: Ridge or GLS
#output: Beta for matrix where each column has ticker and then betas. Model for
#vector where each entry is a model, labeled with a ticker
linreg_all_equities <- function(stock_data, exog_data, rf, start, end, model="Ridge", output = "Beta"){
  syms <- stock_data$symbol
  tickers <- unique(syms)
  outdf <- data.frame(matrix(ncol = length(tickers), nrow=0), col.names=names(tickers))
  if( model == "Ridge"){
    for( t in tickers){
      ridgeMod <- make_best_ridge_fit(t, stock_data, exog_data, rf, start, end, "model")
      if(ridgeMod != -1){
        if(output == "Beta"){
          outdf[t] <- ridgeMod$beta
        } else{
          outdf[t] <- c(-1,-1,-1,-1)
        }
      } else{
        outdf[t] <- c(-1,-1,-1,-1)
      }
    }
  } else {
    return -1
  }
  return(outdf)
}

# Compute R^2 from true and predicted values
Pearson_Rsq <- function(true, predicted) {
  rss <- sum((predicted - true)^2)
  tss <- sum((true - mean(true))^2)
  rsq <- 1 - rss / tss
  return (rsq)
}
unstandardized_linmod_preds <- function(exog, beta, rrFree, inSampleY){
  X <- as.matrix(standardize_cols(exog,3)[,-1])
  constCol <- matrix(rep(1.0, nrow(X)), ncol = 1)
  X <- cbind(constCol, X)
  preds <- X %*% as.matrix(beta) + as.matrix(rrFree)
  #un-standardize
  preds <- preds*sd(inSampleY) + mean(inSampleY)
  return(preds)
}


#uses GAS model for the whole thing
#pass the (differenced, if needed) residuals of the linear factor model
GAS_model_mean_var <- function(linmod_resid){
  GAS_spec <- UniGASSpec(Dist = "ast", ScalingType="Identity",
                         GASPar = list(location = FALSE, scale = TRUE,skewness = TRUE, shape = TRUE,shape2 = TRUE))
  model <- UniGASFit(GAS_spec, linmod_resid)
  return(model)
}

rolling_GAS_mean_var <- function(linmod_resid, forecast_length, refit_every){
  GAS_spec <- UniGASSpec(Dist = "ast", ScalingType="Identity",
                         GASPar = list(location = TRUE, scale = TRUE,skewness = TRUE, shape = TRUE,shape2 = TRUE))
  model <- UniGASRoll(data=linmod_resid, GASSpec = GAS_spec, 
                      ForecastLength = forecast_length, RefitEvery = refit_every,
                      RefitWindow="recursive")
  return(model)
}

xgFit <- function(ytrain, exog){
  ylab <- xgBinLab(ytrain)
  binXG <- xgboost(data=exog, label = ylab,
                   gamma = 5, eta = 0.03, subsample = 0.8,
                   max.depth = 5, nthread = 3, nrounds = 75,
                   objective="binary:logistic", eval_meteric = "error", verbose=0)
  return(binXG)
}
#if ret = "Acc", returns percent correct. If ret ="Pred", returns predictions. if "Mod", returns model
xgFitPredict <- function(priceSeries, lags, prevPeriodLens, factorDat, TrainLen, testLen, ret="Pred"){
  exog <- xgboost_return_exog(priceSeries, lags, prevPeriodLens, factorDat)
  maxp <- max(prevPeriodLens)
  
  yobs <- price_to_period_ret(priceSeries,1)
  yobs <- yobs[(maxp+3):length(yobs)]
  ytrain <- yobs[1:TrainLen]
  ytest <- yobs[(TrainLen+1):(TrainLen + testLen)]
  
  fitMod <- xgFit(ytrain, exog[1:TrainLen,])
  if(ret == "Mod"){
    return(fitMod)
  }
  preds <- xgPred(fitMod, as.matrix(exog[(TrainLen+1):(TrainLen + testLen),]))
  if(ret == "Pred"){
    return(preds)
  } else if(ret == "Acc"){
    accuracy <- xgPredEval(preds, xgBinLab(ytest))
    return(accuracy)
  }
}
xgBigDataEval <- function(stockDf, lags, prevPeriodLens, factorDat, TrainLen, testLen){  
  tickers <- sort(unique(stockDf$symbol))
  #make sure every series has full data
  seriesLen <- length((stockDf %>% dplyr::filter(symbol == 'AAPL'))$adjusted )
  #matrix to hold scores
  data <- setNames(data.frame(matrix(ncol = length(tickers), nrow = 1)), tickers)
  maxl <- length(tickers)
  pb <- txtProgressBar(min=0, max=maxl, style=3)
  tdone <- 0
  for( t in tickers ){
    priceDat <- (stockDf %>% dplyr::filter(symbol == t))$adjusted
    if(length(priceDat) == seriesLen){
      tries <- 0
      fit <- FALSE
      maxTry <- 10
      while(!fit){
        result <- tryCatch({accuracy <- xgFitPredict(priceDat, lags,
                                                     prevPeriodLens, factorDat,
                                                     TrainLen, testLen, "Acc")
        }, error = function(err){
             print(err)
            fit <- TRUE
        }, finally = {
        if(accuracy > 50 || tries >= maxTry){
          fit <- TRUE
          data[1,t] <- accuracy
          tdone <- tdone + 1
          setTxtProgressBar(pb, tdone)
        } else {
          tries <- tries + 1
        }
       }) 
       }#end while
    } else {
      tdone <- tdone + 1
      setTxtProgressBar(pb, tdone)
      print("Error: ")
      print(t)
      print(" does not have the correct number of trading days. Dropping ")
      print(t)
      print(".")
    }
  }
  data <- data[sapply(data, function(x) !any(is.na(x)))]
  return(data)
}

