\name{SOA}
\alias{SOA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spread Option Approximation
}
\description{
The function computes an approximation of the price of a Spread Option 
}
\usage{
SOA(s1, s2, r, v1, v2, rho, t, b1, b2, K, q1, q2, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s1}{
value of underlying 1
}
  \item{s2}{
value of underlying 2
}
  \item{r}{
risk free rate
}
  \item{v1}{
volatility asset 1
}
  \item{v2}{
volatility asset 2
}
  \item{rho}{
correlation btwn asset 1 and 2
}
  \item{t}{
time to maturity
}
  \item{b1}{
cost of carry asset 1
}
  \item{b2}{
cost of carry asset 2
}
  \item{K}{
strike price
}
  \item{q1}{
quantity of asset 1
}
  \item{q2}{
quantity of asset 2
}
  \item{type}{
call "C" or put "P"
}
}
\details{
A European spread option can be valued using the
standard Black and Scholes (1973) model by performing the following
transformation, as originally shown by Kirk (1995)
}
\value{
Approximation of the price of a Spread Option given value of underlying 1 s1, value of underlying 2 s2, risk free rate r, volatility asset 1 v1 , volatility asset 2 v2, correlation btwn asset 1 and 2 rho, time to maturity t,  the cost of carry asset 1 b1, cost of carry asset 2 b2, strike price ,quantity of asset 1 q1, quantity of asset 2 q2 and the type call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{SOA(28,20,0.05,0.29,0.36,0.42,0.25,0,0,7,1,1,"C")

## The function is currently defined as
function (s1, s2, r, v1, v2, rho, t, b1, b2, K, q1, q2, type) 
{
    s <- (q1 * s1 * exp((b1 - r) * t))/(q2 * s2 * exp((b2 - r) * 
        t) + K * exp(-r * t))
    f <- (q2 * s2 * exp((b2 - r) * t))/(q2 * s2 * exp((b2 - r) * 
        t) + K * exp(-r * t))
    v <- sqrt(v1^2 + (v2 * f)^2 - 2 * rho * v1 * v2 * f)
    d1 <- (log(s) + (v^(2)/2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if (type == "C") {
        price <- (q2 * s2 * exp((b2 - r) * t) + K * exp(-r * 
            t)) * (s * pnorm(d1) - pnorm(d2))
    }
    if (type == "P") {
        price <- (q2 * s2 * exp((b2 - r) * t) + K * exp(-r * 
            t)) * (pnorm(-d2) - s * pnorm(-d1))
    }
    return(round(price, 2))
  }
}

