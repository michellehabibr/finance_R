#Created by Michelle Habib
#CALCULATES OPTION PRICE USING BINOMIAL TREES
#n - number of steps
#style - american or european (a or e)
#type - call or put (c or p)
#dt - delta t

binomial_trees <- function(n = 10000, style = "e", type = "c", s0 = 45, k = 42, v = .20, t = 0.8, r = 0.1){
  dt <- t/n
  u <- exp(v*sqrt(dt))
  d <- 1/u
  p <- (exp(r*dt)-d)/(u-d)
  q <- 1-p
  
  #Underlying asset binomial tree
  underlying_tree <- matrix(0,n+1,n+1)
  dvalue <-0
  result <- 0
  for(i in 1:(n+1)){ 
    uvalue <- 0
    for(j in i:(n+1)){
      result <- s0*(u**uvalue)*(d**dvalue)
      underlying_tree[i, j] <-result 
      uvalue <- uvalue +1
    }
    dvalue <- dvalue +1
  }
  
  #Option price binomial tree pt 1
  option_tree <- matrix(0,n+1,n+1)
  #Payoff calculation at time t
  for(i in 1:(n+1)){ 
    if(type=="c") option_tree[i, n+1] <- max(underlying_tree[i,n+1]-k, 0)
    if(type=="p") option_tree[i, n+1] <- max(k-underlying_tree[i,n+1], 0)
  }
  
  #Option price binomial tree pt 2
  #European option
  if(style=="e"){
    for (j in n:1){
      for(i in 1:j){ 
        option_tree[i, j] <- (p*option_tree[i, j+1] + q*option_tree[i+1, j+1])*exp(-1*r*dt)
      }
    }
  }
  
  #American option
  if(style=="a"){
    
    #Call option
    if(type=="c"){
      for (j in n:1){
        for(i in 1:j){ 
            option_tree[i, j] <- max((p*option_tree[i, j+1] + q*option_tree[i+1, j+1])*exp(-1*r*dt),underlying_tree[i,j]-k)
        }
     }
    }
    
    #Put option
    if(type=="p"){
      for (j in n:1){
        for(i in 1:j){ 
          option_tree[i, j] <- max((p*option_tree[i, j+1] + q*option_tree[i+1, j+1])*exp(-1*r*dt),k-underlying_tree[i,j])
        }
      }
    }
  }
  #Price of option
  return(option_tree[1,1])
}