\name{GeneralizedBlackScholes}
\alias{GeneralizedBlackScholes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generalized BlackScholes formula}
\description{compute price of a option call or put with the Generalized BlackScholes formula}
\usage{
GeneralizedBlackScholes(s, K, r, b, v, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{price of the underlying asset}
  \item{K}{strike price}
  \item{r}{risk free rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility express in annual term}
  \item{t}{time to maturity of the option express in annual term}
  \item{type}{type of option Call "C" or Put "P}
}
\details{The Black-Scholes-Merton model can be "generalized" by incorporating a cost-of-carry rate b. This model can be used to price European options on stocks, stocks paying a continuous dividend yield, options on futures, and currency options  }
\value{price of a option call or put given the price of the underlying s, strike price K, risk free rate r, cost of carrying rate b, volatility express in annual term v, time to maturity of the option express in annual term t, type of option Call "C" or Put "P}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{
%%  ~~further notes~~
}
\seealso{}
\examples{GeneralizedBlackScholes(75,70,0.1,0.05,0.35,0.5,"P")
## The function is currently defined as
function (s, K, r, b, v, t, type) 
{
    d1 <- (log(s/K) + (b + v^2/2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if (type == "C") {
        price <- (s * exp((b - r) * t) * pnorm(d1) - K * exp(-r * 
            t) * pnorm(d2))
    }
    if (type == "P") {
        price <- (K * exp(-r * t) * pnorm(-d2) - s * exp((b - 
            r) * t) * pnorm(-d1))
    }
    return(round(price, 2))
  }
}

