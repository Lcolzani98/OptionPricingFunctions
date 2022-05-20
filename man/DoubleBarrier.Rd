\name{DoubleBarrier}
\alias{DoubleBarrier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Price of a Double-barrier Option
}
\description{compute the price of a double-barrier option
}
\usage{
DoubleBarrier(s, K, b, r, v, t, l, u, vec, delta1, delta2, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{underlying price}
  \item{K}{strike price}
  \item{b}{cost of carry rate}
  \item{r}{risk  free rate}
  \item{v}{volatility}
  \item{t}{time to maturity of the option}
  \item{l}{lower boundary}
  \item{u}{upper boundary}
  \item{vec}{the limit of the iteration}
  \item{delta1}{determine the curvature of u}
  \item{delta2}{determine the curvature of l}
  \item{type}{type of option: 
                up-and-out-down-and-out call "co"
                up-and-in-down-and-in call "ci"
                up-and-out-down-and-out put "po"
                up-and-in-down-and-in put "pi"}
}
\details{A double-barrier option is Knocked either in or out if the underlying price touches the lower boundary l or the upper boundary u prior
to expiration.Double barrier options can be priced using the Ikeda and Kuintomo (1992) formula}
\value{price of  a double barrier option given the underlying price s, the strike price K, the cost of carrying rate b, the risk free rate, time to maturity of the option t, the lower boundary l, the upper boundary u, the number of barrier vec, curvature of the upper boundary delta1,the curvature of the lower boundary delta2 and the type of option up-and-out-down-and-out call "co" or up-and-in-down-and-in call "ci" or up-and-out-down-and-out put "po" or up-and-in-down-and-in put "pi"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{DoubleBarrier(100,100,0.1,0.1,0.15,0.25,50,150,c(-5,5),0,0,"co")
## The function is currently defined as
function (s, K, b, r, v, t, l, u, vec, delta1, delta2, type) 
{
    GeneralizedBlackScholes <- function(s, K, r, b, v, t, type) {
        d1 <- (log(s/K) + (b + v^2/2) * t)/(v * sqrt(t))
        d2 <- d1 - v * sqrt(t)
        if (type == "co" | type == "ci") {
            bs <- (s * exp((b - r) * t) * pnorm(d1) - K * exp(-r * 
                t) * pnorm(d2))
        }
        if (type == "po" | type == "pi") {
            bs <- (K * exp(-r * t) * pnorm(-d2) - s * exp((b - 
                r) * t) * pnorm(-d1))
        }
        return(bs)
    }
    sum1 <- 0
    sum2 <- 0
    if ((type == "co") | (type == "ci")) {
        f = u * exp(delta1 * t)
        for (k in vec[1]:vec[2]) {
            mu1 <- (2 * (b - delta2 - k * (delta1 - delta2)))/v^(2) + 
                1
            mu2 <- 2 * k * ((delta1 - delta2)/v^(2))
            mu3 <- (2 * (b - delta2 + k * (delta1 - delta2)))/v^(2) + 
                1
            d1 <- (log(s * u^(2 * k)/(K * l^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            d2 <- (log(s * u^(2 * k)/(f * l^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            d3 <- (log(l^(2 * k + 2)/(K * s * u^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            d4 <- (log(l^(2 * k + 2)/(f * s * u^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            sum1 <- sum1 + (u^(k)/l^(k))^(mu1) * (l/s)^(mu2) * 
                (pnorm(d1) - pnorm(d2)) - (l^(k + 1)/(u^(k) * 
                s))^(mu3) * (pnorm(d3) - pnorm(d4))
            sum2 <- sum2 + (u^(k)/l^(k))^(mu1 - 2) * (l/s)^(mu2) * 
                (pnorm(d1 - v * sqrt(t)) - pnorm(d2 - v * sqrt(t))) - 
                (l^(k + 1)/(u^(k) * s))^(mu3 - 2) * (pnorm(d3 - 
                  v * sqrt(t)) - pnorm(d4 - v * sqrt(t)))
        }
        price <- s * exp((b - r) * t) * sum1 - K * exp(-t * r) * 
            sum2
    }
    if ((type == "po") | (type == "pi")) {
        e = l * exp(delta2 * t)
        for (k in vec[1]:vec[2]) {
            mu1 <- (2 * (b - delta2 - k * (delta1 - delta2)))/v^(2) + 
                1
            mu2 <- 2 * k * ((delta1 - delta2)/v^(2))
            mu3 <- (2 * (b - delta2 + k * (delta1 - delta2)))/v^(2) + 
                1
            y1 <- (log(s * u^(2 * k)/(e * l^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            y2 <- (log(s * u^(2 * k)/(K * l^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            y3 <- (log(l^(2 * k + 2)/(e * s * u^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            y4 <- (log(l^(2 * k + 2)/(K * s * u^(2 * k))) + (b + 
                v^(2)/2) * t)/(v * sqrt(t))
            sum2 <- sum2 + (u^(k)/l^(k))^(mu1 - 2) * (l/s)^(mu2) * 
                (pnorm(y1 - v * sqrt(t)) - pnorm(y2 - v * sqrt(t))) - 
                (l^(k + 1)/(u^(k) * s))^(mu3 - 2) * (pnorm(y3 - 
                  v * sqrt(t)) - pnorm(y4 - v * sqrt(t)))
            sum1 <- sum1 + (u^(k)/l^(k))^(mu1) * (l/s)^(mu2) * 
                (pnorm(y1) - pnorm(y2)) - (l^(k + 1)/(u^(k) * 
                s))^(mu3) * (pnorm(y3) - pnorm(y4))
        }
        price <- K * exp(-r * t) * sum1 - s * exp((b - r) * t) * 
            sum2
    }
    if (type == "ci") {
        price <- GeneralizedBlackScholes(s, K, r, b, v, t, type) - 
            price
    }
    if (type == "pi") {
        price <- GeneralizedBlackScholes(s, K, r, b, v, t, type) - 
            price
    }
    return(round(price, 2))
  }
 }
