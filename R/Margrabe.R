Margrabe <-
function(s1, s2, v1, v2, t, rho, q1, q2) {
  
  #Margrabe formula is an option pricing formula applicable to an option to exchange 
  #one risKy asset for another risKy asset at maturity
  #input:
          #s1 = price of the first risK y asset 
          #s2 = price of the second risK y asset 
          #v1 = volatility first asset
          #v2 =  volatility second asset
          #t = maturity
          #rho = correlation coefficient between asset 1 and asset 2 
          #q1 = expected dividend rates of the prices s1  under the appropriate risK -neutral measure
          #q2 = expected dividend rates of the prices s2  under the appropriate risK -neutral measure
  #output: price of an option to exchange one risKy asset for another risKy asset at maturity
          
  v <- sqrt(v1^2 + v2^2 - 2 * v1 * v2 * rho)
  d1 <- ( log(s1/s2) + ( q2-q1 + v^2/2 ) * t )/ (v*sqrt(t))
  d2 <- ( log(s1/s2) + ( q2-q1 - v^2/2 ) * t ) /(v*sqrt(t))
  Magrabe <- s1*exp(-q1*t)*pnorm(d1) - s2*exp(-q2*t)*pnorm(d2)
  
  return(round(Magrabe,2))
}
