AoN <-
function(s, K , r, b, v, t, type){
  
  #Asset-or-Nothing Options is a type of digital option whose payout is fixed after 
  #the underlying asset exceeds the predetermined threshold or strike price. The payout 
  #depends only on whether or not the underlying asset closes above the striK e price-in 
  #the money (ITM)-at the expiration date. It does not matter how deep ITM as the payout 
  #is fixed.
  #We price the Asset-or-Nothing Options with the Cox and Rubinstein (1985) formula.
  #input: 
          #s = price of the underlying
          #K  = strike price
          #r = risk  free interest rate
          #b = cost of carrying rate
          #v = volatility
          #t = maturity of the option
          #type = call "C" or put "P"
  #output: price of the asset or nothing
  
  d <- (log(s/K ) + (b + v^(2)/2) * t) / (v * sqrt(t))
  
  if(type == "C"){
    
    price <- s * exp((b - r)*t) * pnorm(d)
    
  }
  
  if(type == "P"){
    
    price <- s * exp((b - r)*t) * pnorm(-d)
    
  }
  
  return(round(price,2))
}
