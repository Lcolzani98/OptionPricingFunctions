SWPTN <-
function(t_1, f, K, r, t, v, u, type){
  
  # SWAPTIONS #
  # It is usual to distinguish between the following:
  # Payer Swaption The right but not the obligation to pay the fixed
  # rate and receive the floating rate in the underlying swap. 
  # Receiver Swaption The right but not the obligation to receive the
  # fixed rate and pay the floating rate in the underlying swap. 
  # input:
  # t_1 = tenor of swap in years
  # f = forward price of underlying swap
  # K = strike price
  # r = risk free rate
  # t = time to expiration
  # v = volatility
  # u = number of compoundings per year
  # output: price of the Swaption
  
  d1 <- ( log(f/K) + (v^(2)/2)*t ) / (v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  
  if( type == "C" ){
    
    price <- (( 1 - ( 1 / ( 1 + f / u ) ^ ( t_1 * u ))) / f ) * exp( -r * t ) * ( f * pnorm(d1) - K * pnorm(d2))
    
  }
  
  if( type == "P"){
    
    price <- (( 1 - ( 1 / ( 1 + f / u ) ^ ( t_1 * u ))) / f ) * exp( -r * t ) * ( K * pnorm(-d2) - f * pnorm(-d1))
    
  }
  
  return(round(price,2))  
}
