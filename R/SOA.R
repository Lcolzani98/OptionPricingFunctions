SOA <-
function( s1, s2, r, v1, v2, rho, t, b1, b2, K, q1, q2, type){
  
  #A European spread option can be valued using the
  #standard Black and Scholes (1973) model by performing the following
  #transformation, as originally shown by Kirk (1995)
  #input:
  #s1 = value of underlying 1
  #s2 = value of underlying 2
  #K = strike price
  #r = risk free rate
  #b1 = cost of carry asset 1
  #b2 = cost of carry asset 2
  #v1 = volatility asset 1
  #v2 = volatility asset 2
  #rho = correlation btwn asset 1 and 2
  #t = time to maturity
  #q1 = quantity of asset 1
  #q2 = quantity of asset 2
  #output: price spread option approximation
  
  s <- (q1*s1*exp((b1-r)*t)) / (q2*s2*exp((b2-r)*t) + K*exp(-r*t))
  f <- (q2*s2*exp((b2-r)*t)) / (q2*s2*exp((b2-r)*t) + K*exp(-r*t))
  v <- sqrt(v1^2 + (v2*f)^2 - 2*rho*v1*v2*f)
  d1 <- (log(s) + (v^(2)/2)*t) / (v*sqrt(t))
  d2 <- d1 - v*sqrt(t)
  
  if(type == "C"){
    
    price <- (q2*s2*exp((b2 - r)*t) + K*exp(-r*t)) * (s*pnorm(d1) - pnorm(d2))
    
  }
  
  if(type == "P"){
    
    price <- (q2*s2*exp((b2 - r)*t) + K*exp(-r*t)) * (pnorm(-d2) - s*pnorm(-d1))
    
  }
  
  return(round(price,2))
}
