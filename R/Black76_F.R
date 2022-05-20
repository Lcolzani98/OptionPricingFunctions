Black76_F <-
function(s, K , f, r, v, t, t_f, type){
  
  #BlacK 's Model, also known as the Black  76 Model, is a versatile derivatives pricing model
  #for valuing assets such as options on futures and capped variable rate debt securities.
  #Traders in commodity markets often use the Black -76 model to value
  #options on commodity futures. When it comes to commodity options
  #on forwards, the Black -76 formula holds only for the case when the
  #forward contract expires at the same time as the option contract T.
  #In the case where there is delivery of a forward contract that has a different
  #expiration date, one has only lock ed in the payoff from the option
  #but will receive the intrinsic value first at the forward's expiration.
  #The BlacK -76 formula has to be adjusted for this effect. So we introduce the 
  #BlacK  76-F formula by Haug
  #input:
          #s = price of the underlying
          #K  = strike price
          #f = foward price
          #r = risK  free rate
          #v = volatility express in annual term
          #t = time to maturity of the option contract express in annual term
          #t_f = time to maturity of the foward contract express in annual term
          #type = type of the option Call "C" or Put "P"
  #output: price of option given by Black 76-F 
  
  d1 <- (log(f/K ) + (v^(2)/2) * t)/(v * sqrt(t))
  d2 <- d1 - v*sqrt(t)
  
  if(type == "C"){
    
    price <- exp(-r*t_f)*(f*pnorm(d1) - K  * pnorm(d2))
    
  }
  
  if(type == "P"){
    
    price <- exp(-r*t_f)*(K  * pnorm(-d2) - f*pnorm(-d1))
    
  }
  
  return(round(price,2))
  
}
