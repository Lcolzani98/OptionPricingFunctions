CashorNothing <-
function(s, K , X, r, b, v, t, type){
  
  #Cash-or-nothing options are a type of digital or binary option used in forex trading 
  #that either pays off or expires worthless. these options pay in full value if a condition 
  #is met, or zero if not; there is no partial or multiple payment
  #input:
          #s = underlying value
          #K = strike price
          #X = cash payout
          #r = interest free rate
          #b = cost of carrying rate
          #v = volatility
          #t = maturity
          #type = call "C" or put "P"
  #output: price of the cash or nothing option
  
  d <- (log(s/K ) + (b - v^(2)/2) * t) / (v * sqrt(t))
  
  if( type == "C"){
    
    price <- X *exp(-r*t) * pnorm(d)
    
  }
  
  if( type == "P"){
    
    price <- X *exp(-r*t) * pnorm(-d)
    
  }
  
  return(round(price,2))
}
