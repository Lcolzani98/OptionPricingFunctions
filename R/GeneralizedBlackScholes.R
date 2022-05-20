GeneralizedBlackScholes <-
function(s, K, r, b, v, t, type){
  
  #The Black-Scholes-Merton model can be "generalized" by incorporating
  #a cost-of-carry rate b. This model can be used to price European
  #options on stocks, stocks paying a continuous dividend yield, options
  #on futures, and currency options  
  #input:
          #s = price of the underlying asset
          #K = strike price
          #r = risk free rate
          #b = cost of carrying rate
          #v = volatility express in annual term
          #t = time to maturity of the option express in annual term
          #type = type of option Call "C" or Put "P
  #output: price of the option givben by Generalized Black and Scholes
  
  d1 <- (log(s/K) + (b + v^2/2)*t) / (v*sqrt(t))
  d2 <- d1 - v*sqrt(t)
  
  if(type=="C"){
    price <- (s*exp((b - r)*t)*pnorm(d1) - K*exp(-r*t)*pnorm(d2))
  }
  
  if(type=="P"){
    price <-  (K*exp(-r*t)*pnorm(-d2) - s * exp((b - r)*t) * pnorm(-d1))
  }
  
  return(round(price,2))
}
