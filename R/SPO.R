SPO <-
function( s, K, r, b, v, t, p, type ){
  
  # Standard Power Options (aka asymmetric power options) have nonlinear payoff at maturity. 
  # They apply the power to the underlying asset price at maturity.
  # input:
  # s = underlying value
  # K = strike price
  # r = risk free rate
  # q = dividend yield
  # v = volatility
  # t = maturity
  # p = power > 0
  # type = call "C" or put "P"
  # output: price of the Standard Power Option
  
  d1 <-  (log(s/K^(1/p) ) + ( b + (p - 0.5)*v^2 )* t )/ v*sqrt(t)
  d2 <- d1 - p*v*sqrt(t)
  
  if( type == "C"){
    
    price <- (s^p) * exp(((p - 1)*( r + p*(v^2 / 2)) - p*(r - b))*t) * pnorm(d1) - K * exp(-r*t) * pnorm(d2)
    
  }
  
  if( type == "P"){
    
    price <- K*exp(-r*t)*pnorm(-d2) - (s^p)*exp((p - 1)*( r + p*(v^2 / 2) - p*(r - b))*t)*pnorm(-d1)
    
  }
  
  return(round(price,2))  
  
}
