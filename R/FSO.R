FSO <-
function( s, r, b, v, t, tau, alpha, type){
  
  
  # Forward Start Option #
  # A forward start option with time to maturity T starts at-the-money or proportionally in- or out-of-the-money after a known elapsed time t in the future. The strike is set equal to a positive constant a times the asset price S after the known time t. If a is less than unity, the call (put) will start 1 - a percent in-the-money (out-of-the- money); if a is unity, the option will start at-the-money; and if a is larger than unity, the call (put) will start a - 1 percentage out-of-the- money (in-the-money).A forward start option can be priced using the
  # input:
  # s = underlying value
  # tau = arbitrary time interval
  # r = risk free rate
  # q = dividend yield
  # v = volatility
  # t = maturity
  # type = call "C" or put "P"
  # alpha = positive constant
  # output: price of the Forward Start Option
  
  d1 <- ( log(1/alpha) + (b + v^(2)/2 )*(t - tau)) / (v*sqrt(t-tau))
  d2 <- d1 - v*sqrt(t - tau)
  
  if( type == "C"){
    
    price <- s*exp((b - r)*t)*(exp((b-r)*(t - tau))*pnorm(d1) - alpha*exp(-r*( t - tau ))*pnorm(d2))
    
  }
  
  if( type == "P"){
    
    price <- (s*exp(b-r)*t)*(alpha*exp^(-r(t-tau))*pnorm(-d2) - exp((b-r)*(t-tau))*pnorm(-d1))
    
  }
  
  return(round(price,4))
}
