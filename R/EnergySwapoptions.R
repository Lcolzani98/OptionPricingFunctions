EnergySwapoptions <-
function(s, K , f, x, n, r, r_x, r_b, r_e, r_p, v, t, t_b, type){
  
  #European options on energy swaps, also called energy swaptions,
  #are options that at maturity give a delivery of an energy swap at the
  #strike price (but not necessarily physical delivery of any energy). The
  #swap can have either financial or physical settlement.
  #If a call swaption is in-the-money at maturity, the option has delivery of a swap. 
  #The payoutl from the option is thus not received immediately at expiration, but rather 
  #during the delivery period of the underlying swap (forward).
  #the energy call swaption formula is derived from Haug, 2005a
  #input:
          #s = underlying price
          #K  = strike price
          #f = foward/swap price observed in the market
          #x = is the number of compounding per year
          #n = is the number of settlements in the delivery period for the particular forward 
          #    contracts
          #r = risK  free rate 
          #r_x = is a swap rate starting at the beginning of the delivery period and ending at 
          #      the end of the delivery period with j compoundings per year, equal to the number 
          #      of fixings in the delivery period.
          #r_b = is a risK -free continuous compounding zero coupon rate with t_b years to maturity
          #r_e = is a risK -free continuous compounding zero coupon rate with time to maturity equal 
          #      to from now to the end of the delivery period
          #r_p = is a risK -free continuous compounding zero coupon rate with forward start at the 
          #     option maturity t and ending at the beginning of the delivery period t_b
          #v = volatility 
          #t = time to maturity of the option
          #t_b = is the time to the beginning of the forward delivery period
          #type = type of option "C" Call or "P" Put
  #output: price of the energy swapotion
  
  Black76 <- function(s, K , f, r, v, t,type){
    
    Black76 <- d1 <- (log(f/K ) + (v^(2)/2) * t)/(v * sqrt(t))
    d2 <- d1 - v*sqrt(t)
    
    if(type == "C"){
      
      black76 <- exp(-r*t)*(f*pnorm(d1) - K  * pnorm(d2))
      
    }
    
    if(type == "P"){
      
      black76 <- exp(-r*t)*(K  * pnorm(-d2) - f*pnorm(-d1))
      
    }
    
    return(black76)
    
  }
  
  if(type == "C"){
    
    price <- (1 - (1/(1+r_x/x)^n))/r_x * x/n * exp(-r_p*(t_b-t)) * Black76(s, K , f, r, v, t,type)
    
  }
  
  if(type == "P"){
    
    price <- (1 - (1/(1+r_x/x)^n))/r_x * x/n * exp(-r_p*(t_b-t)) * Black76(s, K , f, r, v, t,type)
    
  }
  
  return(round(price,2)) 
}
