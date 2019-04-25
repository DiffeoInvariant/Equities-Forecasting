library(tidyquant)
library(dplyr)
#tidyquant data getting
get_tq_sp500_data <- function(start, end, ...){
  tickers <- list(...)
  if(length(tickers) > 0){
    data <- tibble(symbol = tickers) %>%
      mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices", from = start, to = end)))
  } else{
    data <- tq_index("SP500") %>% tq_get(get = "stock.prices", from = start, to = end)
  }
  return(data)
}

get_daily_returns <- function(start, end, returns, ...){
  tickers <- list(...)
  if(length(tickers) > 0){
    data <- returns %>% group_by(symbol) %>% dplyr::filter(symbol %in% tickers) %>% tq_transmute(select = adjusted, 
                                                                                                 mutate_fun = periodReturn, 
                                                                                                 period     = "daily", 
                                                                                                 col_rename = "daily.returns") 
  } else{
    data <- returns %>% group_by(symbol) %>% tq_transmute(select = adjusted, 
                                                          mutate_fun = periodReturn, 
                                                          period     = "daily", 
                                                          col_rename = "daily.returns") 
  }
  return(data[-1,])
}

get_daily_log_returns <- function(start, end, returns){
  log_returns <- returns %>% group_by(symbol) %>% tq_transmute(select = adjusted, 
                                                               mutate_fun = periodReturn, 
                                                               type       = "log", 
                                                               period     = "daily",
                                                               col_rename = "daily.log.ret")
  return(log_returns[-1,])
}

get_price_to_book <- function(start, end, ticker, ExPriceData){
  data <- ticker %>% tq_get(get = "key.ratios", from = start, to = end)
  pricebook_annual_df <-  data %>% 
    dplyr::filter(section == "Valuation Ratios") %>% 
    unnest() %>%
    dplyr::filter(category == "Price to Book")   
  abv_dates <- pricebook_annual_df$date
  #print(pricebook_annual_df$date)
  abvd_prices_df <- ExPriceData %>% 
    group_by(symbol) %>% 
    dplyr::filter(symbol == ticker) %>%
    dplyr::filter(date %in% abv_dates)
  #print(abvd_prices_df$'adjusted')
  annual_book_val <- (1.0/pricebook_annual_df$value) %*% (abvd_prices_df$'adjusted')
  pricebook <- (ExPriceData %>% group_by(symbol))$'adjusted'
  for(i in 1:dim(ExPriceData)[1]){
    pricebook[i] <- pricebook[i]/annual_book_val[i]
  }
  return(pricebook)
}

price_to_log_ret <- function(stock_dataframe){
  lr <- Return.calculate(xts(stock_dataframe$"adjusted", order.by=as.Date(stock_dataframe$"date")), method = "log")
  lr <- na.omit(lr)
  return(lr)
}
price_to_ret <- function(stock_dataframe){
  ret <- Return.calculate(xts(stock_dataframe$"adjusted", order.by=as.Date(stock_dataframe$"date")), method = "discrete")
  ret <- na.omit(ret)
  return(ret)
}
#note priceSeries must be a vector of doubles
price_to_period_ret <- function(priceSeries, periodLen){
  #r indexing starts at 1 instead of 0, so increment so we don't
  #go out of bounds in the loop
  periodLen <- periodLen +1
  returns <- priceSeries
  for(i in periodLen:length(priceSeries)){
    #return is (new price - old price)/(old price)
    returns[i] <- (priceSeries[i] - priceSeries[i-(periodLen - 1)])/ priceSeries[i-(periodLen - 1)]
  }
  return(returns[-(1:periodLen)])
}

sign_log_abs <- function(x){
  return( (sign(x)) * log(abs(x)))
}
ret_to_logret <- function(returns){
  log_returns <- lapply(returns, sign_log_abs)
  return(log_returns %>% unlist %>% as.vector() %>% na.omit())#as.vector(unlist(log_returns)))
}
em1 <- function(x){
  return(exp(x)-1)
}
log_to_simple_ret <- function(log_returns){
  sr <- lapply(log_returns, em1)
  return(unlist(sr))
}
ret_to_price <- function(returns, init_price){
  price <- as.vector(rep(init_price, length(returns)))
  for(i in 2:length(returns)){
    price[i] <- price[i-1]*(1+ returns[i-1])
  }
  return(price)
}

adjusted_rets <- function(rf, returns){
  returns <- returns - rf
  return(returns)
}
adjusted_rets_inverter <- function(rf, returns){
  returns <- returns + rf
  return(returns)
}
standardize_cols <- function(mat, ncols){
  for( c in 1:ncols){
    mat[,c] <- (mat[,c] - mean(mat[,c]))/sd(mat[,c])
  }
  return(mat)
}
#acceptable date range:
#July 1, 1926 - February 28, 2019
#express, say, 1926-07-01 as 19260701
get_KFDL_3Factor <- function(start, end){
  data <- read.csv("/Users/zanejakobs/Desktop/Regression Project/F-F_Research_Data_Factors_daily.csv") 
  data$Date <- as.vector(data$Date)
  data <- data %>% dplyr::filter(Date >= start & Date <= end)   
  return(data)
}

#writes historical price data to OR project local repo
write_historical_price_data <- function(start, end){
  dir <- "/Users/zanejakobs/Desktop/ORProj/Data/Historical/prices_2016.csv"
  sp500data <- get_tq_sp500_data(start,end) %>% group_by(symbol)
  write.csv(sp500data, file=dir)
}
write_historical_market_data <- function(start,end){
  dir <- "/Users/zanejakobs/Desktop/ORProj/Data/Historical/market_2016.csv"
  market <- tq_get("SPY", from = start, to = end)
  write.csv(market, file=dir)
}
big_returns_mat <- function(stockDf, nDays){
  
  tickers <- sort(unique(stockDf$symbol))
  #print(tickers)
  seriesLen <- length( price_to_period_ret((stockDf %>% filter(symbol == 'AAPL'))$adjusted, nDays) )
  data <- setNames(data.frame(matrix(ncol = length(tickers), nrow = seriesLen)), tickers)
  
  for( t in tickers ){
    priceDat <- stockDf %>% filter(symbol == t)
    ret <- price_to_period_ret(priceDat$adjusted, nDays)
    if(length(ret) == seriesLen){
      data[,t] <- ret
    } else {
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

