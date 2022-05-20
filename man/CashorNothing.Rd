\name{CashorNothing}
\alias{CashorNothing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Price of a Cash or Nothing  Option}
\description{
compute the price of a cash or nothing option}
\usage{
CashorNothing(s, K, X, r, b, v, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{price of the underlying}
  \item{K}{strike price}
  \item{X}{cash payout}
  \item{r}{risk free interest rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility}
  \item{t}{time to maturity}
  \item{type}{call "C" or put "P"}
}
\details{
Cash-or-nothing options are a type of digital or binary option used in forex trading that either pays off or expires worthless. these options pay in full value if a condition is met, or zero if not; there is no partial or multiple payment}
\value{
price of a cash or nothing option given the value of the underlying s, the strike price K, the risk free rate r, the cost of carrying rate b, the volatility v, the time to maturity t , and the type of option call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{CashorNothing(100,80, 10,0.06,0,0.35,0.75,"P") 


## The function is currently defined as
function (s, K, X, r, b, v, t, type) 
{
    d <- (log(s/K) + (b - v^(2)/2) * t)/(v * sqrt(t))
    if (type == "C") {
        price <- X * exp(-r * t) * pnorm(d)
    }
    if (type == "P") {
        price <- X * exp(-r * t) * pnorm(-d)
    }
    return(round(price, 2))
  }
}

