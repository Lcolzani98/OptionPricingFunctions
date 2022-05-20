DoubleBarrier <-
function(s, K, b, r, v, t, l, u, vec, delta1, delta2, type){
  
  #A double-barrier option is Knocked either in or out if the underlying
  #price touches the lower boundary l or the upper boundary u prior
  #to expiration. The price of a double Knock -in call is equal to the portfolio of
  #a long standard call and a short double Knock -out call, with identical
  #strikes and time to expiration. Similarly, a double Knock -in put is
  #equal to a long standard put and a short double KnocK -out put. Double barrier
  #options can be priced using the Ikeda and Kuintomo (1992) formula.
  #input: 
  #s = underlying price
  #K  = strike price
  #b = cost of carry rate
  #r = risk  free rate
  #v = volatility
  #t = time to maturity
  #l = lower boundary
  #u = upper boundary
  #vec = 
  #delta1 = delta 1 determine the curvature of u 
  #delta2 = delta 2 determine the curvature of l
  #type: 
  #1 up-and-out-down-and-out call "co"
  #2 up-and-in-down-and-in call "ci"
  #3 up-and-out-down-and-out put "po"
  #4 up-and-in-down-and-in put "pi"
  #output: price of the double barrier option 
  
  GeneralizedBlackScholes <- function(s, K, r, b, v, t, type){
    
    d1 <- (log(s/K) + (b + v^2/2)*t) / (v*sqrt(t))
    d2 <- d1 - v*sqrt(t)
    
    if(type=="co" | type == "ci"){
      bs <- (s*exp((b - r)*t)*pnorm(d1) - K*exp(-r*t)*pnorm(d2))
    }
    
    if(type=="po" | type == "pi"){
      bs <-  (K*exp(-r*t)*pnorm(-d2) - s * exp((b - r)*t) * pnorm(-d1))
    }
    
    return(bs)
  }
  
  sum1 <- 0 
  sum2 <- 0
  if((type == "co") | (type == "ci")){
    
    f = u * exp(delta1*t)
    for(k in vec[1]:vec[2]){ 
      
      mu1 <- (2 *(b - delta2 - k*(delta1 - delta2)))/v^(2) + 1
      mu2 <- 2*k*((delta1 - delta2)/v^(2))
      mu3 <-  (2 *(b - delta2 + k*(delta1 - delta2)))/v^(2) + 1
      
      d1 <- (log(s * u^(2 * k)/(K  * l^(2 * k))) + (b + v^(2) / 2) * t) / (v * sqrt(t))
      d2 <- (log(s *u^(2 * k)/(f * l^(2 * k))) + (b + v^(2) / 2) * t) / (v * sqrt(t))
      d3 <- (log(l^(2 * k + 2)/(K  * s * u^(2 * k))) + (b + v^(2)/ 2) * t) / (v * sqrt(t))
      d4 <- (log(l^(2 * k + 2)/(f * s * u^(2 * k))) + (b + v^(2)/ 2) * t) / (v * sqrt(t))
      
      sum1 <- sum1 + (u^(k)/l^(k))^(mu1) * (l/s)^(mu2) * (pnorm(d1) - pnorm(d2)) - 
        (l^(k+1)/(u^(k) * s))^(mu3) * (pnorm(d3) - pnorm(d4))
      
      sum2 <- sum2 + (u^(k)/l^(k))^(mu1 - 2) * (l/s)^(mu2) * (pnorm(d1 - v*sqrt(t)) - pnorm(d2 - v*sqrt(t))) - 
        (l^(k+1)/(u^(k) * s))^(mu3 - 2) * (pnorm(d3 - v * sqrt(t)) - pnorm(d4 - v*sqrt(t)))
      
    }
    
    price <- s * exp((b - r) * t) * sum1 - K *exp(-t * r) * sum2
    
  }
  
  if((type == "po") | (type == "pi")){
    
    e = l * exp(delta2 * t)
    for(k in vec[1]:vec[2]){
      
      mu1 <- (2 *(b - delta2 - k*(delta1 - delta2)))/v^(2) + 1
      mu2 <- 2*k*((delta1 - delta2)/v^(2))
      mu3 <-  (2 *(b - delta2 + k*(delta1 - delta2)))/v^(2) + 1
      
      y1 <- (log(s * u^(2 * k)/(e * l^(2 * k))) + (b + v^(2) / 2) * t) / (v * sqrt(t))
      y2 <- (log(s *u^(2 * k)/(K  * l^(2 * k))) + (b + v^(2) / 2) * t) / (v * sqrt(t))
      y3 <- (log(l^(2 * k + 2)/(e * s * u^(2 * k))) + (b + v^(2)/ 2) * t) / (v * sqrt(t))
      y4 <- (log(l^(2 * k + 2)/(K  * s * u^(2 * k))) + (b + v^(2)/ 2) * t) / (v * sqrt(t))
      
      sum2 <- sum2 + (u^(k)/l^(k))^(mu1 - 2) * (l/s)^(mu2) * (pnorm(y1 - v*sqrt(t)) - pnorm(y2 - v*sqrt(t))) - 
        (l^(k+1)/(u^(k) * s))^(mu3 - 2) * (pnorm(y3 - v * sqrt(t)) - pnorm(y4 - v*sqrt(t)))
      sum1 <- sum1 + (u^(k)/l^(k))^(mu1) * (l/s)^(mu2) * (pnorm(y1) - pnorm(y2)) - 
        (l^(k+1)/(u^(k) * s))^(mu3) * (pnorm(y3) - pnorm(y4))
      
    }
    
    price <- K  * exp(-r * t) * sum1 - s * exp((b - r) * t) * sum2
  }
  
  if(type == "ci"){
    
    price <- GeneralizedBlackScholes(s, K, r, b, v, t, type) - price
  }
  
  if(type == "pi"){
    
    price <- GeneralizedBlackScholes(s, K, r, b, v, t, type) - price
  }
  
  return(round(price,2))
}
