\name{FSO}
\alias{FSO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price of a Forward Start Option
}
\description{
The function computes the price of a Forward Start Option
}
\usage{
FSO(s, r, b, v, t, tau, alpha, type)  }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
underlying value
}
  \item{r}{
risk free rate
}
  \item{b}{ cost of carry

}
  \item{v}{
volatility
}
  \item{t}{
maturity}
  \item{tau}{
arbitrary time interval
}
  \item{alpha}{
positive constant
}
  \item{type}{
call "C" or put "P"
}
}
\details{
A forward start option with time to maturity T starts at-the-money or proportionally in- or out-of-the-money after a known elapsed time t in the future. The strike is set equal to a positive constant a times the asset price S after the known time t. If a is less than unity, the call (put) will start 1 - a percent in-the-money (out-of-the- money); if a is unity, the option will start at-the-money; and if a is larger than unity, the call (put) will start a - 1 percentage out-of-the- money (in-the-money).}
\value{
Price of the Forward Start Option given the underlying price s, the time to maturity t, he risk free rate r, the cost of carrying rate b,the volatility v, an arbitrary time interval tau, a positive constant alpha and the type of option call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{FSO(60,1,0.08,0.04,0.3,0.25,1.1,"C")
## The function is currently defined as
function (s, r, b, v, t, tau, alpha, type) 
{
    d1 <- (log(1/alpha) + (b + v^(2)/2) * (t - tau))/(v * sqrt(t - 
        tau))
    d2 <- d1 - v * sqrt(t - tau)
    if (type == "C") {
        price <- s * exp((b - r) * t) * (exp((b - r) * (t - tau)) * 
            pnorm(d1) - alpha * exp(-r * (t - tau)) * pnorm(d2))
    }
    if (type == "P") {
        price <- (s * exp(b - r) * t) * (alpha * exp^(-r(t - 
            tau)) * pnorm(-d2) - exp((b - r) * (t - tau)) * pnorm(-d1))
    }
    return(round(price, 4))
  }
}

