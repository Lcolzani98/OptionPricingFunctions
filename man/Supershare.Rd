\name{Supershare}
\alias{Supershare}
\title{ Price of a Supershare Option
}
\description{ The function computes the price of an Supershare Option
}
\usage{
Supershare(s, K_l, K_h, r, b, v, t)
}
\arguments{
  \item{s}{
price of the underlying}
  \item{K_l}{
lower strike limit}
  \item{K_h}{
 high strike limit}
  \item{r}{
risK  free rate}
  \item{b}{
cost of carrying rate}
  \item{v}{
volatility}
  \item{t}{
time to maturity}
}
\details{  
  Supershare option is a type of binary, where in a common binary option the payout would be a set dollar 
  amount should the underlying be greater than (or less than) the strike. 
  In a Supershare option, there is a lower and upper boundary. If the underlying at 
  expiry is between these boundaries the payoff is: Payoff = Underlying / LowerBoundary
  If the underlying is outside these boundaries the payoff is zero
  A supershare option, originally introduced by Hakansson (1976)
}
\value{
Price of a Supershare Option given the underlying price s, the lower strike limit K_l, the high stirke limit K_h,  the risk free rate r, the volatility v, the time to maturity of the option t

}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{ Supershare(100,90,110,0.1,0,0.2,0.25)


## The function is currently defined as
function (s, K_l, K_h, r, b, v, t) 
{
    d1 <- (log(s/K_l) + (b + v^(2)/2) * t)/(v * sqrt(t))
    d2 <- (log(s/K_h) + (b + v^(2)/2) * t)/(v * sqrt(t))
    price <- (s * (exp((b - r) * t))/K_l) * (pnorm(d1) - pnorm(d2))
    return(round(price, 2))
  }
}

