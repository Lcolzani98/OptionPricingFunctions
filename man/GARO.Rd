\name{GARO}
\alias{GARO}
\title{Price of a Geometric Average Rate Option}
\description{compute the price of a geometric average rate option}
\usage{
  GARO( s, K, r, b, v, t, type )
}
\arguments{
  \item{s}{underlying value}
  \item{K}{strike price]}
  \item{r}{risk free rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility}
  \item{t}{maturity of the option [years]}
  \item{type}{call "C" or put "P"}
}
\details{The geometric average rate option is a specific Asian option and therefore depends on an average price of the underlier. Here this average is calculated geometrically.We price the option using the formula by Kemna and Vorst (1990), where the geometric average option can be priced as a standard option by changing the volatility and cost-of-carry term}
\value{price of a geometric average rate option call or put given the price of the underlying S, K the strike price, r the risk free rate, b cost of carrying rate, v volatilitity , t the maturity of the option [years] and the type of option call "C" or put "P"  }
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\examples{GARO(80,85,0.05,0.08,0.2,0.25,"P")


## The function is currently defined as
function (s, K, r, b, v, t, type) 
{
    b_a <- 0.5 * (b - (v^2/6))
    v_a <- v/sqrt(3)
    d1 <- (log(s/K) + (b_a + (v_a^2)/2) * t)/(v_a * sqrt(t))
    d2 <- d1 - v_a * sqrt(t)
    if (type == "C") {
        price <- s * exp((b_a - r) * t) * pnorm(d1) - K * exp(-r * 
            t) * pnorm(d2)
    }
    if (type == "P") {
        price <- K * exp(-r * t) * pnorm(-d2) - s * exp((b_a - 
            r) * t) * pnorm(-d1)
    }
    return(round(price, 2))
  }
}
