\name{SPO}
\alias{SPO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Price of a Standard Power Option

}
\description{
The function computes the price of a standard Power Option 
}
\usage{
SPO(s, K, r, b, v, t, p, type)
}
\arguments{
  \item{s}{
underlying value
}
  \item{K}{
strike price
}
  \item{r}{
risk free rate
}
  \item{b}{
cost of carry
}
  \item{v}{
volatility
}
  \item{t}{
maturity
}
  \item{p}{
power > 0
}
  \item{type}{
call "C" or put "P"
}
}
\details{
Standard Power Options (aka asymmetric power options) have nonlinear payoff at maturity. 
They apply the power to the underlying asset price at maturity.
}
\value{
price of a standard powered option given the underlying price s, the strike price K, the risk free ratem the cost of carrying ratem the volatility v, the power p, the time to maturity of the option t and the type of option call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{ SPO(10,100,0.08,0.02,0.3,0.5,2,"C")

## The function is currently defined as
function (s, K, r, b, v, t, p, type) 
{
    d1 <- (log(s/K^(1/p)) + (b + (p - 0.5) * v^2) * t)/v * sqrt(t)
    d2 <- d1 - p * v * sqrt(t)
    if (type == "C") {
        price <- (s^p) * exp(((p - 1) * (r + p * (v^2/2)) - p * 
            (r - b)) * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
    }
    if (type == "P") {
        price <- K * exp(-r * t) * pnorm(-d2) - (s^p) * exp((p - 
            1) * (r + p * (v^2/2) - p * (r - b)) * t) * pnorm(-d1)
    }
    return(round(price, 2))
  }
}

