\name{Powered}
\alias{Powered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Price of a Powered Option}
\description{
Compute the price of a powered option}
\usage{
Powered(s, K, r, b, v, p, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{price of the underlying}
  \item{K}{strike price}
  \item{r}{risk free rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility}
  \item{p}{power}
  \item{t}{maturity of the option}
  \item{type}{type of option "C" or "P"}
}
\details{
Esser (2003) describes how to value these options}
\value{price of a powered option given the underlying price s, the strike price K, the risk free ratem the cost of carrying ratem the volatility v, the power p, the time to maturity of the option t and the type of option call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}

\note{}


\seealso{}
\examples{Powered(100,100,0.1,0.07,0.1,2,0.5,"C")
## The function is currently defined as
function (s, K, r, b, v, p, t, type) 
{
    price <- 0
    if (type == "C") {
        for (j in 1:p) {
            d <- (log(s/K) + (b + (p - j - 0.5) * v^(2)) * t)/(v * 
                sqrt(t))
            price <- price + choose(p, j) * s^(p - j) * (-K)^(j) * 
                exp((p - j - 1) * (r + (p - j) * v^(2)/2)*t - 
                  (p - j) * (r - b) * t) * pnorm(d)
        }
    }
    if (type == "P") {
        for (j in 1:p) {
            d <- (log(s/K) + (b + (p - j - 0.5) * v^(2)) * t)/(v * 
                sqrt(t))
            price <- price + choose(p, j) * (-s)^(p - j) * (K)^(j) * 
                exp((p - j - 1) * (r + (p - j) * v^(2)/2)*t - 
                  (p - j) * (r - b) * t) * pnorm(-d)
        }
    }
    return(round(price, 2))
  }
}

