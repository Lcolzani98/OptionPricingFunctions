\name{Chooser}
\alias{Chooser}
\title{Price of a Chooser Option}
\description{ The function computes the price of a Chooser Option
}
\usage{
Chooser(s, K, b, r, v, t, t1)
}
\arguments{
  \item{s}{
price of the underlying}
  \item{K}{
strike price}
  \item{b}{
cost of carry rate}
  \item{r}{
risK  free rate}
  \item{v}{
volatility }
  \item{t}{
time to maturity }
  \item{t1}{
time to choose between put  or  call}
}
\details{
A chooser option is an option contract that allows the holder to decide whether 
  it is to be a call or put prior to the expiration date. Chooser options usually 
  have the same strike price and expiration date regardless of what decision the holder makes.}
\value{ The price of a Chooser Option given the undelying price s, the strike price K,  the cost of carrying rate b, the risk free rate r, the volatility v, the time to maturity of the option t and time to choose between put  or  call t1.

}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{ Chooser(50,50,0.08,0.08,0.25,0.5, 0.25)
## The function is currently defined as
function (s, K, b, r, v, t, t1) 
{
    d <- (log(s/K) + (b + v^(2)/2) * t)/(v * sqrt(t))
    y <- (log(s/K) + b * t + v^(2) * t1/2)/(v * sqrt(t1))
    price <- s * exp((b - r) * t) * pnorm(d) - K * exp(-r * t) * 
        pnorm(d - v * sqrt(t)) - s * exp((b - r) * t) * pnorm(-y) + 
        K * exp(-r * t) * pnorm(-y + v * sqrt(t1))
    return(round(price, 2))
  }
}
