\name{EuropeanStandardMonteCarlo}
\alias{EuropeanStandardMonteCarlo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
European Option price computed using Monte Carlo simulation
}
\description{
The function computes the price of a European Option using Monte Carlo simulation}
\usage{
EuropeanStandardMonteCarlo(s, K, r, b, v, nSim, t, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
price of underliying
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
  \item{v}{
volatility
}
  \item{nSim}{
number of simulations
}
  \item{t}{
time to maturity
}
  \item{type}{
call "C" or put "P"
}
}
\details{
Monte Carlo simulation is a numerical method that is useful in many
situations when no closed-form solution is available. Monte Carlo
simulating in option pricing, originally introduced by Boyle (1977),
can be used to value most types of European options and, as we will
see, also American options
}
\value{
Price European Option computed using Monte Carlo simulation given the price of the underlying s, the trike price K, the risk free rate r, the cost of carrying rate b, the volatility v, the number of simulation nSim, the time to maturity of the option t, the type of call "C" or put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{EuropeanStandardMonteCarlo(100,90,0.05,0.02,0.3,1000,0.5,"C")
## The function is currently defined as
function (s, K, r, b, v, nSim, t, type) 
{
    sum <- 0
    if (type == "C") {
        for (j in 1:nSim) {
            st <- s * exp(((b - v^2)/2) * t + v * sqrt(t) * rnorm(j))
            sum <- sum + max((st - K), 0)
        }
    }
    if (type == "P") {
        for (j in 1:nSim) {
            st <- s * exp(((b - v^2)/2) * t + v * sqrt(t) * rnorm(j))
            sum <- sum + max((K - st), 0)
        }
        price <- (exp(-r * t) * sum)/nSim
    }
    price <- (exp(-r * t) * sum)/nSim
    return(round(price, 2))
  }
}

