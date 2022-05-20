CPO <-
function(s ,K  , r, b, v, p, c, t, type){
  
  #Power option is an option whose payoff is based on the price of an underlying asset 
  #raised to a power. It is designed to allow the buyer (holder) to take a leveraged 
  #view on a specific asset or its volatility.
  #A capped option caps the maximum possible profit for its holder. 
  #When the underlying asset closes at or beyond a specified price, the option automatically exercises
  #We use the Esser (2003) gives the closed-form solution to price the option
  #input:
          #s = price of the underlying
          #K  = striK e price
          #r = risK  free interest rate
          #b = cost of carrying rate
          #v = volatility
          #p = power
          #c = maximum predefined level of the payoff
          #t = maturity
          #type = call "C" or put "P"
  #output: price of a power capped option
  
  e1 <- ((log(s/K ^(1/p)) + (b + (p - 0.5) * v ^(2))) * t) / (v * sqrt(t))
  e2 <- e1  - p*v*sqrt(t)
  
  if(type == "C"){
    
    e3 <- ((log(s/(c+K )^(1/p))) + (b + (p - 0.5 ) * v^(2))*t) / (v * sqrt(t))
    e4 <- e3 - p*v*sqrt(t)
    price <- s^(p) * exp(((p - 1) * (r + p*v^(2)/2) - p*(r - b))*t) * (pnorm(e1) - pnorm(e2)) -
      exp(-r*t) * (K *pnorm(e2) - (c + K ) * pnorm(e4))
    
  }
  
  if(type == "P"){
    
    e3 <- ((log(s/(K  - c)^(1/p))) + (b + (p - 0.5 ) * v^(2))*t) / (v * sqrt(t))
    e4 <- e3 - p*v*sqrt(t)
    price <- exp(-r*t) * (K *pnorm(-e2) - (K  - c) * pnorm(-e4)) - s^(p) * exp(((p - 1) * (r + p*v^(2)/2) - p*(r - b))*t) * 
      (pnorm(-e1) - pnorm(-e2))
  }
  
  return(round(price,2))
}
