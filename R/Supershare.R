Supershare <-
function(s, K_l, K_h, r, b, v, t){
  
  #Supershare option is a type of binary, where in a common binary option the payout would be a set dollar 
  #amount should the underlying be greater than (or less than) the strike. 
  #In a Supershare option, there is a lower and upper boundary. If the underlying at 
  #expiry is between these boundaries the payoff is: Payoff = Underlying / LowerBoundary
  #If the underlying is outside these boundaries the payoff is zero
  #A supershare option, originally introduced by Hakansson (1976)
  #input: 
          #s = price of the underlying
          #K_l = lower strike limit
          #K_h = high strike limit
          #r = risK  free rate
          #b = cost of carrying rate
          #v = volatility
          #t = time to maturity
  #output: price of the supershare option
  
  d1 <- (log(s/K_l) + (b + v^(2)/2) * t)/(v*sqrt(t))
  d2 <- (log(s/K_h) + (b + v^(2)/2) * t)/(v*sqrt(t))
  price <- (s * (exp((b - r) * t)) / K_l) * (pnorm(d1) - pnorm(d2))
  
  return(round(price,2))
  
}
