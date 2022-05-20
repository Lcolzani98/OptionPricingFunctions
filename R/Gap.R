Gap <-
function(s, K1, K2, r, b, v,t, type){
  
  #Gap option is binary option whose stated strike price is different from its payoff strike. 
  #That is, there is a gap between the price at which the option can be exercised and the 
  #price at which it would produce a payoff to the holder.
  #We price the option using the Reinerand Rubinstein (1991b) formula
  #input: 
          #s = underlying value
          #K1 = strike price
          #K2 = trigger price
          #r = interest free rate
          #b = cost of carrying rate
          #v = volatility
          #t = maturity
          #type = call "C" or put "P"
  #output: price of the gap option
  
  d1 <- (log(s/K1) + (b + v^(2)/2) * t) / (v *sqrt(t))
  d2 <- d1 - v*sqrt(t)
  
  if( type == "C"){
    
    price <- s*exp((b - r) * t) * pnorm(d1) - K2*exp(-r * t) * pnorm(d2)
    
  }
  
  if( type == "P"){
    
    price <- K2 * exp(-r * t) * pnorm(-d2) - s*exp((b - r) * t) * pnorm(-d1)
    
  }
  
  return(round(price,4))
  
}
