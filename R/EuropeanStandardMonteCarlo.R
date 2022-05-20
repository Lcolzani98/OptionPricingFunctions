EuropeanStandardMonteCarlo <-
function(s, K, r, b, v, nSim, t, type){
  
  #Monte Carlo simulation is a numerical method that is useful in many
  #situations when no closed-form solution is available. Monte Carlo
  #simulating in option pricing, originally introduced by Boyle (1977),
  #can be used to value most types of European options and, as we will
  #see, also American options
  #input:
  #s = price of underliying
  #K = strike price
  #r = risk free rate
  #b = cost of carry rate
  #v = volatility
  #nSim= number of simulations
  #t = time to maturity
  #type = call "C" or put "P"
  #output: price of an option given by a Montecarlo simulation for a European option
  
  sum <- 0
  
  if( type == "C"){
    
    for(j in 1:nSim){
      
      st <- s*exp(((b - v^2) / 2)*t + v * sqrt(t)*rnorm(j))
      sum <- sum + max( (st - K), 0)
      
    }
    
  }
  
  if(type == "P"){
    
    for(j in 1:nSim){
      
      st <- s*exp(((b - v^2) / 2)*t + v * sqrt(t)*rnorm(j))   
      sum <- sum + max( (K - st), 0)
      
    }
    
    price <- (exp(-r*t) * sum) / nSim
    
  }
  
  price <- (exp(-r*t) * sum) / nSim
  return(round(price,2))
  
}
