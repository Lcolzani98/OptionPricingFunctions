Powered <-
function(s, K , r, b, v, p, t, type){
  
  #Esser (2003) describes how to value these options
  #input: 
          #s = price of the underlying
          #K  = strike price
          #r = risK  free interest rate
          #b = cost of carrying rate 
          #v = volatility
          #p = power
          #t = maturity
          #type = call "C" or put "P"
  #output:
  
  price <- 0
  if(type == "C"){
    
    for(j in 1:p){
      d <- (log(s/K ) + (b + (p - j - 0.5) * v^(2))*t) / (v * sqrt(t))
      price <- price + choose(p,j) * s^(p - j) *
              (- K ) ^ (j) * exp((p -j -1) * (r + (p - j)* v^(2)/2)*t - (p - j) * (r - b) * t) * 
               pnorm(d)
      
    }
  }
  
  if(type == "P"){
    
    for(j in 1:p){
      d <- (log(s/K ) + (b + (p - j - 0.5) * v^(2))*t) / (v * sqrt(t))
      price <- price + choose(p,j) * (-s)^(p - j) *
        (K ) ^ (j) * exp((p -j -1) * (r + (p - j)* v^(2)/2)*t - (p - j) * (r - b) * t) * 
        pnorm(-d)
    }
  }
  return(round(price,2))
}
