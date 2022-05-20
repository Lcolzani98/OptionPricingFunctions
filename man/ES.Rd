\name{ES}
\alias{ES}

\title{
Price of an Energy Swap/Forward
}
\description{
The function computes the price of both Energy Swaps and Forwards
}
\usage{
ES(f, u, n, r_ji, t_b, r_b)  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
forward/swap price
}
  \item{u}{
number of compounding per year
}
  \item{n}{
number of settlements in the delivery period for the forward contract
}
  \item{r_ji}{
risk - free interest swap rate starting at the beginning of the delivery
}
  \item{t_b}{
time to the beginning of the forward delivery period
}
  \item{r_b}{
risk free continuously compounded zero coupon rate with Tb years to
}
}
\details{
Oil and electricity swaps are actively traded in the energy markets. 
Given the presence of traded contracts with quoted market prices for instance, 
a swap (forward) we can come up with a way to value the swap relative to other swaps
}
\value{
Price of an Energy Swap/Forward given the forward/swap price f, the number of compounding per year u, the number of settlements in the delivery period for the forward contract
n,the risk - free interest swap rate starting at the beginning of the delivery r_ji,the time to the beginning of the forward delivery period
t_b, the risk free continuously compounded zero coupon rate with Tb years to
r_b
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{
}
\examples{ ES(35,365,90,0.05,0.5,0.04)
## The function is currently defined as
function (f, u, n, r_ji, t_b, r_b) 
{
    ratio <- (exp(-r_b * t_b)/n)
    price <- 0
    for (k in 1:n) {
        price <- price + (f/(1 + (r_ji/u))^(k))
    }
    price <- price * ratio
    return(round(price, 2))
  }
}

