\name{PO}
\alias{PO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of a Product Option
}
\description{
compute the price of a product option}
\usage{
PO(s1, s2, K, r, b1, b2, v1, v2, rho, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s1}{value of underlying 1}
  \item{s2}{value of underlying 2}
  \item{K}{strike price}
  \item{r}{risk free rate}
  \item{b1}{cost of carry rate 1}
  \item{b2}{cost of carry rate 2}
  \item{v1}{volatility asset 1}
  \item{v2}{volatility asset 2}
  \item{rho}{correlation}
  \item{t}{time to maturity}
  \item{type}{call "C" or put "P"}
}
\details{
Zhang (1998) describes formulas for product options.}
\value{ price of a product option give the price of the asset 1 s1, the price of asset 2 s2, the strike price K, teh risk free rate r, the cost of carrying rate of the first asset 1, the cost of carrying rate of asset 2, the volatility of asset 1, the volatility of asset 2, the correlation between the two asset rho, the time to maturity t, and the type of option call "C" or put "P"

}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{PO(100,105,15000,0.07,0.02,0.05,0.3,0.3,-0.5,0.1,"C")
## The function is currently defined as
function (s1, s2, K, r, b1, b2, v1, v2, rho, t, type) 
{
    f <- s1 * s2 * exp((b1 + b2 + rho * v1 * v2) * t)
    v <- sqrt(v1^2 + v2^2 + 2 * rho * v1 * v2)
    d1 <- (log(f/K) + t * ((v^2)/2))/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
     if(type == "C"){
    
    price <- exp(-r*t)*(f*pnorm(d1) - K*pnorm(d2))
    
  }
  
  if(type == "P"){
    
    price <- exp(-r*t)*(K*pnorm(d1) - f*pnorm(d2))
    
  }
  
  return(round(price,4))
  }
}

