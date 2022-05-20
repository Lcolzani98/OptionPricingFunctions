PO <-
function(s1, s2, K, r, b1, b2, v1, v2, rho, t, type ){
  
  #Zhang (1998) describes formulas for product options.
  #input: 
  #s1 = value of underlying 1
  #s2 = value of underlying 2
  #K = strike price
  #r = risk free rate
  #b1 = cost of carry rate 1
  #b2 = cost of carry rate 2
  #v1 = volatility asset 1
  #v2 = volatility asset 2
  #rho = correlation
  #t = time to maturity
  #output: price of the product option
  
  f <- s1*s2*exp((b1 + b2 + rho*v1*v2)*t)
  v <- sqrt(v1^2 + v2^2 + 2*rho*v1*v2)
  d1 <- (log(f/K) + t*((v^2)/2))/(v*sqrt(t))
  d2 <- d1 - v*sqrt(t)
  
  if(type == "C"){
    
    price <- exp(-r*t)*(f*pnorm(d1) - K*pnorm(d2))
    
  }
  
  if(type == "P"){
    
    price <- exp(-r*t)*(K*pnorm(d1) - f*pnorm(d2))
    
  }
  
  return(round(price,4))
}
