Chooser <-
function(s, K , b, r, v, t,t1){
  
  #A chooser option is an option contract that allows the holder to decide whether 
  #it is to be a call or put prior to the expiration date. Chooser options usually 
  #have the same strike price and expiration date regardless of what decision the holder makes.
  #input: 
          #s = price of the underlying
          #K = striK e price
          #b = cost of carry rate
          #r = risK  free rate
          #v = volatility 
          #t = time to maturity 
          #t1 = time to choose between put  or  call
  #output: chooser option price
  
  d <- (log(s/K ) + (b + v^(2)/2)*t)/(v*sqrt(t))
  y <- (log(s/K ) + b*t + v^(2)*t1/2)/(v*sqrt(t1))
  
  price <- s*exp((b-r)*t)*pnorm(d)-K *exp(-r*t)*pnorm(d - v*sqrt(t)) - s*exp((b-r)*t)*
    pnorm(-y) + K *exp(-r*t)*pnorm(-y + v*sqrt(t1))
  
  return(round(price,2))
}
