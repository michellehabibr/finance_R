#Created by Michelle Habib
#CALCULATES IMPLIED VOLATILITY USING BINARY SEARCH FOR EUROPEAN OPTIONS
#USING BLACK SCHOLES EQUATION

implied_vol <- function(type = "c", option_price = 7.074361, lower_bound = 0.0, upper_bound = 1.0, diff = 0.001){
  count <- 0
  repeat{
    count <- count+1
    volatility <- (lower_bound + upper_bound)/2
    calculated_price <- blackScholes(v = volatility, type = type)
    
    if(abs(option_price - calculated_price) < diff){
     break
    }
    if(option_price > calculated_price){
      lower_bound <- volatility
    }
    else{
      upper_bound <- volatility
    }
  }
  return(signif(volatility, digits = 2))
}