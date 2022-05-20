EuropeanBinomial <-
function(s, K, r, b, v, t, n, pow, cap, type1, type2){
  
  #input:
  #s = price of underlying
  #K = strike price
  #r = risk free rate
  #b = cost of carry rate
  #v = volatility
  #t = time to maturity
  #n =  number of time steps used for valuation.
  #pow = power the power options are raised to
  #cap = cap is the cap on the payoff for any capped option.
  #type1 = Call or Put
  #type2 = type of payoff
  #output: price of a european option given by a the binomial method  
  if( type1 == "C"){
    
    g <- 1
    
  }
  
  if(type1 == "P"){
    
    g <- -1
    
  }
  
  BinPayoff <- function(s, K, pow, cap, type2, g){
    
    if(type2 == "PV"){
      
      BinPayoff <- max(g*(s-K), 0)
      
    } else if(type2 == "PC"){
      
      BinPayoff <- s^pow
      
    } else if(type2 == "CPC"){
      
      BinPayoff <- min(s^pow, cap)
      
    } else if(type2 == "PC"){
      
      BinPayoff <- g*(s/K)^pow
      
    } else if(type2 == "SPO"){
      
      BinPayoff <- max(g*(s^pow)- K, 0)
      
    } else if(type2 == "CPO"){
      
      BinPayoff <- min(max(g*(s^pow) - K, 0), cap)
      
    } else if(type2 == "POpt"){
      
      BinPayoff <- max((g*(s-K)),0)^pow
      
    } else if(type2 == "CPOpt"){
      
      BinPayoff <- min(max(g*(s - K), 0)^pow, cap)
      
    } else if(type2 == "SO"){
      
      BinPayoff <- max(g*(sin(s) - K), 0)
      
    } else if(type2 == "CO"){
      
      BinPayoff <- max(g* (cos(s) - K), 0)
      
    } else if(type2 == "TO"){
      
      BinPayoff <- max(g*(tan(s) - K), 0)
      
    } else if(type2 == "LC"){
      
      BinPayoff <- log(s / K)
      
    } else if(type2 == "LO"){
      
      BinPayoff <- max(log(s / K), 0)
      
    } else if(type2 == "SQRTC"){
      
      BinPayoff <- sqrt(s / K)
      
    } else if(type2 == "SQRTO"){
      
      BinPayoff <- sqrt( max(g*(s-K), 0))
      
    }
  }
  
  
  dt <- t / n
  u <- exp(v * sqrt(dt))
  d <- 1 / u
  p <- (exp(b * dt) - d) / (u - d)
  
  sum <- 0
  
  if(type1 == "C"){
    
    for(j in 0:n){
      
      si <- s * u^j * d^(n - j)
      sum <- sum + choose(n,j) * p^j * (1-p)^(n-j) * BinPayoff(s, K, pow, cap, type2, g)
      
    }
    
  }
  
  if(type1 == "P"){
    
    for(j in 0:n){
      
      si <- s * u^j * d^(n - j)
      sum <- sum + chose(n,j) * p^j * (1-p)^(n-j) * BinPayoff(type2, g, s, K, pow, cap)
    }
    
  }
  
  price <- exp(-r*t)*sum
  return(round(price,2))
  
}
