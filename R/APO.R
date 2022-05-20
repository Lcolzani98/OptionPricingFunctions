APO <-
function(s, K, r, b,v, type){
  
  #A perpetual option is a non-standard, or exotic, financial option that has 
  #no fixed maturity and no exercise limitation. While the life of a standard option 
  #can range from a few days to several years, a perpetual option (XPO) can be 
  #exercised at any time and without any expiration.
  #input: 
  #s = underlying value
  #K = strike price
  #r = risk free rate
  #b = cost of carry rate
  #v = volatility
  #output: price of perpetual american option
  y1 <- 0.5 - (b/v^2) + sqrt(((b/v^2) - 0.5)^2 + (2*r/v^2))
  y2 <- 0.5 - (b/v^2) - sqrt(((b/v^2) - 0.5)^2 + (2*r/v^2))
  
  if( type == "C"){
    
    price <- (K/(y1 -1)) * (((y1 - 1)/y1) * s/K)^y1
    
  }
  
  if( type == "P"){
    
    price <- (K/(1-y2)) * (((y2 - 1)/y2) * s/K)^y2
    
  }
  
  return(round(price,2))  
}
