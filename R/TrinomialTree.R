TrinomialTree <-
function(s, K, r, b, v, t, nSim, type1, type2){
  
  #The trinomial option pricing model is an option pricing model incorporating three possible 
  #values that an underlying asset can have in one time period. The three possible values the 
  #underlying asset can have in a time period may be greater than, the same as, or less than 
  #the current value.
  #Before fill the input we must choose nSim(number of step) >= integer((b^2*t)/2*v^2) + 1 o.w 
  #probability will be negative
  #our code is based on the Espen Haug's Excel VBA trinomial tree approach which is computationally faster 
  #than the one developed by Fabrice Rouah.
  #input:
          #s = price of the underlying
          #K = strike price
          #r = risk free rate
          #b = cost of carrying rate
          #v = volatility
          #t = time to maturity of the option
          #nSim = number of simulation
          #type1 = type of option Call "C" or Put "P"
          #type2 = type of option European "European" or American "American" 
  #output:price given by trinomail tree
  
  if(nSim < integer(((b^2*t)/2*v^2) + 1)){
    
    print("Choose nSim >= integer(((b^2*t)/2*v^2) + 1) to avoid negative probabilities")
    break
    
  }else{
    
    priceTrinomial <- rep(0,2*nSim+1) 
    deltaT <- t/nSim
    up <- exp(v*sqrt(2*deltaT))
    down <- exp(-v*sqrt(2*deltaT))
    
    #probability of going up and down
    probabilityUp <- ((exp(b * deltaT/2) - exp(-v*sqrt(deltaT/2)))/(exp(v*sqrt(deltaT/2)) - exp(-v*sqrt(deltaT/2))))^(2)
    probabilityDown <-((exp(v * sqrt(deltaT/2)) - exp(b * deltaT/2))/(exp(v*sqrt(deltaT/2)) - exp(-v*sqrt(deltaT/2))))^(2)
    
    #probability of staying at the same level
    probabilityStatic <- 1 - probabilityUp - probabilityDown
    
    #now we build the trinomial for the stock price evolution
    #for european option
    for(k in 0:(2*nSim)){
      if(type1 =="C"){
        priceTrinomial[k+1] <- max((s * up^(max(k - nSim, 0)) * down^(max(nSim * 2 - nSim - k,0))) - K, 0)
      }
      if(type1 == "P"){
        priceTrinomial[k+1] <- max(K - (s * up^(max(k - nSim, 0)) * down^(max(nSim * 2 - nSim - k,0))) ,0)
      }
    }
    for(j in (nSim-1):0){
      for(k in 0:(j*2)){
        if(type2 == "European"){
          priceTrinomial[k+1] <- (probabilityUp*priceTrinomial[k+3] + probabilityDown*priceTrinomial[k+1]+ 
                                    probabilityStatic*priceTrinomial[k+2]) * exp(-r * deltaT)
        }
        if(type2 == "American"){
          if(type1 == "C"){
            priceTrinomial[k+1] <- max((s * up^(max(k - j, 0)) * down^(max(j * 2 - j - k,0))) - K, 
                                       (probabilityUp*priceTrinomial[k+3] + probabilityDown*priceTrinomial[k+1]+ 
                                          probabilityStatic*priceTrinomial[k+2]) * exp(-r * deltaT))
          }
          if(type1 == "P"){
            priceTrinomial[k+1] <- max(K - (s * up^(max(k - j, 0)) * down^(max(j * 2 - j - k,0))) ,
                                       (probabilityUp*priceTrinomial[k+3] + probabilityDown*priceTrinomial[k+1]+ 
                                          probabilityStatic*priceTrinomial[k+2]) * exp(-r * deltaT))
          }
        }
      }
    }
   
    return(round(priceTrinomial[1],2))
  }
  
}
