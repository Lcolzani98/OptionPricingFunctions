\name{APO}
\alias{APO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of an American Perpetual Option
}
\description{The fucntion computes the price of an American Perpetual Option
}
\usage{
APO(s, K, r, b, v, type)} 
%- maybe also 'usage' for other objects documented here.
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
cost of carry rate
}
  \item{v}{volatility
}
  \item{type}{
call "C" or put "P"
}
}
\details{
  A perpetual option is a non-standard, or exotic, financial option that has 
  no fixed maturity and no exercise limitation. While the life of a standard option 
  can range from a few days to several years, a perpetual option (XPO) can be 
  exercised at any time and without any expiration.
}
\value{
Price of an American Perpetual Option given s, K, r, b, v, type
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{ APO(90,100,0.1,0.02,0.25,"C")

## The function is currently defined as
function (s, K, r, b, v, type) 
{
    y1 <- 0.5 - (b/v^2) + sqrt(((b/v^2) - 0.5)^2 + (2 * r/v^2))
    y2 <- 0.5 - (b/v^2) - sqrt(((b/v^2) - 0.5)^2 + (2 * r/v^2))
    if (type == "C") {
        price <- (K/(y1 - 1)) * (((y1 - 1)/y1) * s/K)^y1
    }
    if (type == "P") {
        price <- (K/(1 - y2)) * (((y2 - 1)/y2) * s/K)^y2
    }
    return(round(price, 2))
  }
}
