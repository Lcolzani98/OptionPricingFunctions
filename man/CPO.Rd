\name{CPO}
\alias{CPO}
\title{ Price of a Capped Power Option
}
\description{ The function computes the price of a Capped Power Option
}
\usage{
CPO(s, K, r, b, v, p, c, t, type)
}

\arguments{
  \item{s}{
price of the underlying}
  \item{K}{
strike price}
  \item{r}{
risK  free interest rate}
  \item{b}{
 cost of carrying rate}
  \item{v}{
volatility}
  \item{p}{
power}
  \item{c}{
maximum predefined level of the payoff}
  \item{t}{
maturity}
  \item{type}{
call "C" or put "P"}
}
\details{
Power option is an option whose payoff is based on the price of an underlying asset 
  raised to a power. It is designed to allow the buyer (holder) to take a leveraged 
  view on a specific asset or its volatility.
  A capped option caps the maximum possible profit for its holder. 
  When the underlying asset closes at or beyond a specified price, the option automatically exercises
  We use the Esser (2003) gives the closed-form solution to price the option}
\value{ Price of a Capped Power Option given the price of the underlying s, the strike price K, the risk free rate r, the cost of carrying rate b, the volatility v, the time to maturity t and the type call "C" or put "P".

}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{CPO(100,100,0.1,0.07,0.1,2,30 ,0.5,"C")
## The function is currently defined as
function (s, K, r, b, v, p, c, t, type) 
{
    e1 <- ((log(s/K^(1/p)) + (b + (p - 0.5) * v^(2))) * t)/(v * 
        sqrt(t))
    e2 <- e1 - p * v * sqrt(t)
    if (type == "C") {
        e3 <- ((log(s/(c + K)^(1/p))) + (b + (p - 0.5) * v^(2)) * 
            t)/(v * sqrt(t))
        e4 <- e3 - p * v * sqrt(t)
        price <- s^(p) * exp(((p - 1) * (r + p * v^(2)/2) - p * 
            (r - b)) * t) * (pnorm(e1) - pnorm(e3)) - exp(-r * 
            t) * (K * pnorm(e2) - (c + K) * pnorm(e4))
    }
    if (type == "P") {
        e3 <- ((log(s/(K - c)^(1/p))) + (b + (p - 0.5) * v^(2)) * 
            t)/(v * sqrt(t))
        e4 <- e3 - p * v * sqrt(t)
        price <- exp(-r * t) * (K * pnorm(-e2) - (K - c) * pnorm(-e4)) - 
            s^(p) * exp(((p - 1) * (r + p * v^(2)/2) - p * (r - 
                b)) * t) * (pnorm(-e1) - pnorm(-e2))
    }
    return(round(price, 2))
  }
}
