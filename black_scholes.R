#European Option Pricing (Assuming no dividend payment)
#s0 - Spot price (Underlying asset price)
#k - Strike price
#v - Annual volatility
#t - Time to maturity, in years
#r - Annual risk-free rate
#type = call (c) or put (p)

blackScholes <- function(s0 = 45, k = 42, v = .20, t = .8, r = 0.1, type = "c"){
  price <- 0
  
  #Calculate d1 and d2
  d1 <- (log(s0/k) + (r + v**2/2)*t)/(v*sqrt(t))
  d2 = d1 - v*sqrt(t)
  
  #Check if call or put
  while(type != "c" && type != "p"){
    type <- readline("Please specify option type (c for call or p for put): ")
  }
  if(type == "c"){
    k <- k*-1
  }
  if(type == "p"){
    s0 <- s0 * -1
    d1 <- d1 * -1
    d2 <- d2 * -1
  }
  
  #Calculate price
    price = s0*pnorm(d1) + k*exp(-1*r*t)*pnorm(d2)
  return(price)
}