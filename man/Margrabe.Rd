\name{Margrabe}
\alias{Margrabe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Margrabe formula for option pricing 
}
\description{
the function uses the Margrabe formula to compute option prices
}
\usage{
Margrabe(s1, s2, v1, v2, t, rho, q1, q2) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s1}{
price of the first risky asset
}
  \item{s2}{
price of the second risky asset 
}
  \item{v1}{
volatility first asset
}
  \item{v2}{
volatility second asset
}
  \item{t}{
maturity
}
  \item{rho}{
correlation coefficient between asset 1 and asset 2 
}
  \item{q1}{
expected dividend rates of the prices s1  under the appropriate risK -neutral measure
}
  \item{q2}{
expected dividend rates of the prices s2  under the appropriate risK -neutral measure
}
}
\details{
Margrabe formula is an option pricing formula applicable to an option to exchange 
one risky asset for another risy asset at maturity
}
\value{
Price of an option computed using Mrgrabe Formula given the price of the first risky asset s1, price of the second risky asset s2, volatility of the first asset v1, volatility of the second asset v2, maturity of the option t, correlation coefficient between asset 1 and asset 2 rho, expected dividend rates of the prices s1  under the appropriate risK -neutral measure q1, expected dividend rates of the prices s2  under the appropriate risK -neutral measure q2
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{Margrabe(125,100,0.45,0.47,1,0,0.04,0.02)

## The function is currently defined as
function (s1, s2, v1, v2, t, rho, q1, q2) 
{
    v <- sqrt(v1^2 + v2^2 - 2 * v1 * v2 * rho)
    d1 <- (log(s1/s2) + (q2 - q1 + v^2/2) * t)/(v * sqrt(t))
    d2 <- (log(s1/s2) + (q2 - q1 - v^2/2) * t)/(v * sqrt(t))
    Magrabe <- s1 * exp(-q1 * t) * pnorm(d1) - s2 * exp(-q2 * 
        t) * pnorm(d2)
    return(round(Magrabe, 2))
  }
}

