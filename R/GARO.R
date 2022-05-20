GARO <-
function( s, K, r, b, v, t, type ){
  
  # Geometric Average Rate Option #
  # The geometric average rate option is a specific Asian option and therefore depends on an average price of the underlier. Here this average is calculated geometrically.
  # We price the option using the formula by Kemna and Vorst (1990), where the geometric average option can be priced as a standard option by changing the volatility and cost-of-carry term
  # input:
  # s = underlying value
  # K = strike price
  # r = risk free rate
  # b = cost of carrying rate
  # v = volatility
  # t = maturity
  # type = call "C" or put "P"
  # output: price of the Geometric Average Rate Option
  
  b_a <- 0.5 * ( b - (v^2 / 6))
  v_a <- v/sqrt(3)
  d1 <- (log(s / K) + (b_a + (v_a^2) / 2) * t) / (v_a*sqrt(t))
  d2 <- d1 - v_a * sqrt(t)
  
  if( type == "C"){
    
    price <- s*exp((b_a-r)*t)*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
    
  }
  
  if( type == "P"){
    
    price <- K*exp(-r*t)*pnorm(-d2) - s*exp((b_a-r)*t)*pnorm(-d1)
    
  }
  
  return(round(price,2))  
}
