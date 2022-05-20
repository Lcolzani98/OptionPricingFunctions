\name{AoN}
\alias{AoN}

\title{ Price of an Asset or Nothing Option}

\description{ The function computes the price of an Asset or Nothing Option
}
\usage{
AoN(s, K, r, b, v, t, type)
}
\arguments{
  \item{s}{
price of the underlying
}
  \item{K}{
 strike price
}
  \item{r}{
risk  free interest rate}
  \item{b}{
 cost of carrying rate}
  \item{v}{
volatility}
  \item{t}{
maturity}
  \item{type}{
call "C" or put "P"}
}
\details{
Asset-or-Nothing Options is a type of digital option whose payout is fixed after 
  the underlying asset exceeds the predetermined threshold or striK e price. The payout 
  depends only on whether or not the underlying asset closes above the striK e price-in 
  the money (ITM)-at the expiration date. It does not matter how deep ITM as the payout 
  is fixed.
  We price the Asset-or-Nothing Options with the Cox and Rubinstein (1985) formula.}
\value{ Price of an Asset or Nothing Option given s, K, r, b, v, t and type.
}

\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{ AoN(70,65,0.07,0.02,0.27,0.5,"P")

## The function is currently defined as
function (s, K, r, b, v, t, type) 
{
    d <- (log(s/K) + (b + v^(2)/2) * t)/(v * sqrt(t))
    if (type == "C") {
        price <- s * exp((b - r) * t) * pnorm(d)
    }
    if (type == "P") {
        price <- s * exp((b - r) * t) * pnorm(-d)
    }
    return(round(price, 2))
  }
}
