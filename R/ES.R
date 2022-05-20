ES <-
function( f, u, n, r_ji, t_b, r_b ){
  
  # ENERGY SWAPS/FORWARDS #
  # Oil and electricity swaps are actively traded in the energy markets. 
  # Given the presence of traded contracts with quoted market prices for instance, 
  # a swap (forward) we can come up with a way to value the swap relative to other swaps
  # input:
  # f = forward/swap price
  # u = number of compounding per year
  # n = number of settlements in the delivery period for the forward contract
  # r_ui = risk - free interest swap rate starting at the beginning of the delivery period and ending at the i period, with u compounding per year
  # t_b = time to the beginning of the forward delivery period
  # r_b = risk free continuously compounded zero coupon rate with Tb years to maturity.
  # output: price of the Energy Swap
  
  ratio <- (exp(-r_b*t_b)/n)
  price <- 0
  for(k in 1:n){
    
    price <- price + (f/(1+(r_ji/u))^(k))
    
  }
  
  price <- price * ratio
  
  return(round(price,2))  
}
