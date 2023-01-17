#Created by Michelle Habib
#CALCULATES BOND PRICE

#ytm - yield to maturity (per year)
#coupon_rate - coupon rate (per year)
#t - time to maturity (in years)
#m - coupon payments (per year)
#par - par value

bond_price <- function(ytm = 0.03, coupon_rate = 0.05, t = 4, m = 2, par = 100){
  coupon <- (coupon_rate/m)*par #coupon paid in each period
  r <- ytm/m #required rate of return for each period
  n <- t*m #periods to maturity
  
  price <-0
  for(i in 1:n){
    price <- price + coupon/(1+r)**i
  }
  
  price <- price + par/(1+r)**i
  return(price)
}