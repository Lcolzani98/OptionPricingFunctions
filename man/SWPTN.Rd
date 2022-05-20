\name{SWPTN}
\alias{SWPTN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of a Swaption
}
\description{
The function computes the price of a Swaption
}
\usage{
SWPTN(t_1, f, K, r, t, v, u, type)
}

\arguments{
  \item{t_1}{
tenor of swap in years
}
  \item{f}{
forward price of underlying swap
}
  \item{K}{
strike price
}
  \item{r}{
risk free rate
}
  \item{t}{
time to expiration
}
  \item{v}{
volatility
}
  \item{u}{
number of compoundings per year
}
  \item{type}{
call "C" or put "P"
}
}
\details{
It is usual to distinguish between the following:
Payer Swaption The right but not the obligation to pay the fixed
rate and receive the floating rate in the underlying swap. 
Receiver Swaption The right but not the obligation to receive the
fixed rate and pay the floating rate in the underlying swap. 
}
\value{
Price of a Swaption given tenor of swap in years t_1, forward price of underlying swap f, the strike price K, risk free rate r, time to expiration t,the volatility v, number of compoundings per year u and the type call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}

\examples{SWPTN(4,0.07,0.075,0.06,2,0.2,2,"C")
## The function is currently defined as
function (t_1, f, K, r, t, v, u, type) 
{
    d1 <- (log(f/K) + (v^(2)/2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if (type == "C") {
        price <- ((1 - (1/(1 + f/u)^(t_1 * u)))/f) * exp(-r * 
            t) * (f * pnorm(d1) - K * pnorm(d2))
    }
    if (type == "P") {
        price <- ((1 - (1/(1 + f/u)^(t_1 * u)))/f) * exp(-r * 
            t) * (K * pnorm(-d2) - f * pnorm(-d1))
    }
    return(round(price, 2))
  }
}

