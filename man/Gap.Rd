\name{Gap}
\alias{Gap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of a Gap Option
}
\description{
The function computes the price of a Gap Option
}
\usage{
Gap(s, K1, K2, r, b, v, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
underlying value
}
  \item{K1}{
strike price
}
  \item{K2}{
trigger price
}
  \item{r}{
interest free rate
}
  \item{b}{
cost of carryig rate
}
  \item{v}{
volatility
}
  \item{t}{
maturity
}
  \item{type}{
call "C" or put "P"
}
}
\details{
Gap option is binary option whose stated strike price is different from its payoff strike. 
That is, there is a gap between the price at which the option can be exercised and the 
price at which it would produce a payoff to the holder.
We price the option using the Reinerand Rubinstein (1991b) formula
}
\value{
Price of a Gap Option given the price of the underlying s, the strike price K1, the trigger price K2, the risk free rate r, the cost of carrying rate b, the volatility v, the time to maturity of the option t, the type of option call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{Gap(50,50,57,0.09,0.09,0.2,0.5,"C")

## The function is currently defined as
function (s, K1, K2, r, b, v, t, type) 
{
    d1 <- (log(s/K1) + (b + v^(2)/2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if (type == "C") {
        price <- s * exp((b - r) * t) * pnorm(d1) - K2 * exp(-r * 
            t) * pnorm(d2)
    }
    if (type == "P") {
        price <- K2 * exp(-r * t) * pnorm(-d2) - s * exp((b - 
            r) * t) * pnorm(-d1)
    }
    return(round(price, 4))
  }
}

