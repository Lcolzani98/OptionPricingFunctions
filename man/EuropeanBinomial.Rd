\name{EuropeanBinomial}
\alias{EuropeanBinomial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of a European Binomial Option
}
\description{
The function computes the price of a European Binomial Option
}
\usage{
EuropeanBinomial(s, K, r, b, v, t, n, pow, cap, type1, type2)  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
rice of underlying
}
  \item{K}{
strike price
}
  \item{r}{
risk free rate
}
  \item{b}{
cost of carry rate
}
  \item{v}{
volatility
}
  \item{t}{
time to maturity
}
  \item{n}{
number of time steps used for valuation
}
  \item{pow}{
power the power options are raised to
}
  \item{cap}{
cap is the cap on the payoff for any capped option
}
  \item{type1}{
Call or Put
}
  \item{type2}{
type of payoff
}
}
\details{
It is capable of pricing any European option on a single asset, whose
payoff is not path-dependent.
}
\value{
Price of a European Binomial Option given the prcie of the underlying s, the strike price K, the risk free rate , the cost of carrying rate b, the volatility v, the time to maturiy of the option t, number of time steps used for valuation n, power the power options are raised to pow,the cap on the payoff for any capped option
cap, the type of option type1, type of payoff
 type2
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{EuropeanBinomial(100,90,0.05,0.02,0.3,0.5,3,2,50,"C","CPC")

## The function is currently defined as
function (s, K, r, b, v, t, n, pow, cap, type1, type2) 
{
    if (type1 == "C") {
        g <- 1
    }
    if (type1 == "P") {
        g <- -1
    }
    BinPayoff <- function(s, K, pow, cap, type2, g) {
        if (type2 == "PV") {
            BinPayoff <- max(g * (s - K), 0)
        }
        else if (type2 == "PC") {
            BinPayoff <- s^pow
        }
        else if (type2 == "CPC") {
            BinPayoff <- min(s^pow, cap)
        }
        else if (type2 == "PC") {
            BinPayoff <- g * (s/K)^pow
        }
        else if (type2 == "SPO") {
            BinPayoff <- max(g * (s^pow) - K, 0)
        }
        else if (type2 == "CPO") {
            BinPayoff <- min(max(g * (s^pow) - K, 0), cap)
        }
        else if (type2 == "POpt") {
            BinPayoff <- max((g * (s - K)), 0)^pow
        }
        else if (type2 == "CPOpt") {
            BinPayoff <- min(max(g * (s - K), 0)^pow, cap)
        }
        else if (type2 == "SO") {
            BinPayoff <- max(g * (sin(s) - K), 0)
        }
        else if (type2 == "CO") {
            BinPayoff <- max(g * (cos(s) - K), 0)
        }
        else if (type2 == "TO") {
            BinPayoff <- max(g * (tan(s) - K), 0)
        }
        else if (type2 == "LC") {
            BinPayoff <- log(s/K)
        }
        else if (type2 == "LO") {
            BinPayoff <- max(log(s/K), 0)
        }
        else if (type2 == "SQRTC") {
            BinPayoff <- sqrt(s/K)
        }
        else if (type2 == "SQRTO") {
            BinPayoff <- sqrt(max(g * (s - K), 0))
        }
    }
    dt <- t/n
    u <- exp(v * sqrt(dt))
    d <- 1/u
    p <- (exp(b * dt) - d)/(u - d)
    sum <- 0
    if (type1 == "C") {
        for (j in 0:n) {
            si <- s * u^j * d^(n - j)
            sum <- sum + choose(n, j) * p^j * (1 - p)^(n - j) * 
                BinPayoff(s, K, pow, cap, type2, g)
        }
    }
    if (type1 == "P") {
        for (j in 0:n) {
            si <- s * u^j * d^(n - j)
            sum <- sum + chose(n, j) * p^j * (1 - p)^(n - j) * 
                BinPayoff(type2, g, s, K, pow, cap)
        }
    }
    price <- exp(-r * t) * sum
    return(round(price, 2))
  }
}

