\name{BAWApproximation}
\alias{BAWApproximation}
\title{Quadratic approximation method by Barone-Adesi and Whaley for an American option }
\description{
compute the price of an American call or put option with the Barone-Adesi and Whaley approximation }
\usage{
BAWApproximation(s, K, r, b, v, t, type)
}
\arguments{
  \item{s}{price of the underlying asset}
  \item{K}{strike price}
  \item{r}{risk free rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility}
  \item{t}{time to maturity of the option}
  \item{type}{type of option Call "C" or Put "P"}
}
\details{
The quadratic approximation method by Barone-Adesi and Whaley (1987) can be used to price American call and put options on an underlying asset with cost-of-carry rate b. 
When b > r, the American call value is equal to the European call value and can then be found by using the generalized Black-Scholes-Merton (BSM) formula. Themodel is fast and accurate for most practical input values}
\value{price of an american option given the price of the underlying s, the strike price K, the risk free rate r, the cost of carrying rate b, the volatility v, the time to maturity of the option t and the type of option Call "C" or Put "P"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{BAWApproximation(100,100,0.1,0,0.15,0.1,"C")
## The function is currently defined as
function (s, K, r, b, v, t, type) 
{
    GeneralizedBlackScholes <- function(s, K, r, b, v, t, type) {
        d1 <- (log(s/K) + (b + v^2/2) * t)/(v * sqrt(t))
        d2 <- d1 - v * sqrt(t)
        if (type == "C") {
            price <- (s * exp((b - r) * t) * pnorm(d1) - K * 
                exp(-r * t) * pnorm(d2))
        }
        if (type == "P") {
            price <- (K * exp(-r * t) * pnorm(-d2) - s * exp((b - 
                r) * t) * pnorm(-d1))
        }
        return(price)
    }
    m <- 2 * r/v^(2)
    n <- 2 * b/v
    k <- 1 - exp(-r * t)
    q1 <- (-(n - 1) - sqrt((n + 1)^(2) + 4 * m/k))/2
    q2 <- (-(n - 1) + sqrt((n + 1)^(2) + 4 * m/k))/2
    CriticalCommodityPrice <- function(K, r, b, v, t, type) {
        if (type == "C") {
            cpi <- K/(1 - 2 * ((-(n - 1) + sqrt((n - 1)^2 + 4 * 
                m)))^(-1))
            h2 <- -(b * t + 2 * v * sqrt(t)) * K/(cpi - k)
            seedValue <- K + (cpi - K) * (1 - exp(h2))
            d1 <- (log(seedValue/K) + (b + v^(2)/2) * t)/(v * 
                sqrt(t))
            LHS <- seedValue - K
            RHS <- GeneralizedBlackScholes(s, K, r, b, v, t, 
                type) + (1 - exp((b - r) * t) * pnorm(d1)) * 
                seedValue/q2
            slope <- (exp(b - r) * t) * pnorm(d1) * (1 - 1/q2) + 
                1/q2 * (1 - ((exp(b - r) * t) * dnorm(d1))/(v * 
                  sqrt(t)))
            error <- 1e-05
            while (abs(RHS - LHS)/K > error) {
                seedValue <- (K + RHS - slope * seedValue)/(1 - 
                  slope)
                d1 <- (log(seedValue/K) + (b + v^(2)/2) * t)/(v * 
                  sqrt(t))
                LHS <- seedValue - K
                RHS <- GeneralizedBlackScholes(s, K, r, b, v, 
                  t, type) + (1 - exp((b - r) * t) * pnorm(d1)) * 
                  seedValue/q2
                slope <- (exp(b - r) * t) * pnorm(d1) * (1 - 
                  1/q2) + 1/q2 * (1 - ((exp(b - r) * t) * dnorm(d1))/(v * 
                  sqrt(t)))
            }
        }
        if (type == "P") {
            cpi <- K/(1 - 2 * ((-(n - 1) - sqrt((n - 1)^2 + 4 * 
                m)))^(-1))
            h1 <- (b * t - 2 * v * sqrt(t)) * K/(K - cpi)
            seedValue <- cpi + (K - cpi) * exp(h1)
            d1 <- (log(seedValue/K) + (b + v^(2)/2) * t)/(v * 
                sqrt(t))
            LHS <- K - seedValue
            RHS <- GeneralizedBlackScholes(s, K, r, b, v, t, 
                type) - (1 - exp((b - r) * t) * pnorm(d1)) * 
                seedValue/q1
            slope <- -(exp(b - r) * t) * pnorm(-d1) * (1 - 1/q1) + 
                1/q1 * (1 + ((exp(b - r) * t) * dnorm(-d1))/(v * 
                  sqrt(t)))
            error <- 1e-05
            while (abs(RHS - LHS)/K > error) {
                seedValue <- (K - RHS + slope * seedValue)/(1 + 
                  slope)
                d1 <- (log(seedValue/K) + (b + v^(2)/2) * t)/(v * 
                  sqrt(t))
                LHS <- K - seedValue
                RHS <- GeneralizedBlackScholes(s, K, r, b, v, 
                  t, type) - (1 - exp((b - r) * t) * pnorm(d1)) * 
                  seedValue/q1
                slope <- -(exp(b - r) * t) * pnorm(-d1) * (1 - 
                  1/q1) + 1/q1 * (1 + ((exp(b - r) * t) * dnorm(-d1))/(v * 
                  sqrt(t)))
            }
        }
        return(seedValue)
    }
    if (type == "C") {
        if (b >= r) {
            price <- BlackScholes(s, K, r, v, t, type == "C")
        }
        else {
            sStar <- CriticalCommodityPrice(K, r, b, v, t, type)
            d1 <- (log(sStar/K) + (b + v^(2)/2) * t)/(v * sqrt(t))
            a2 <- sStar/q2 * (1 - exp((b - r) * t)) * pnorm(d1)
            if (s < sStar) {
                price <- GeneralizedBlackScholes(s, K, r, b, 
                  v, t, type) + a2 * (s/sStar)^(q2)
            }
            else {
                price <- s - K
            }
        }
    }
    if (type == "P") {
        sStar <- CriticalCommodityPrice(K, r, b, v, t, type)
        d1 <- (log(sStar/K) + (b + v^(2)/2) * t)/(v * sqrt(t))
        a1 <- -sStar/q1 * (1 - exp((b - r) * t)) * pnorm(-d1)
        if (s > sStar) {
            price <- GeneralizedBlackScholes(s, K, r, b, v, t, 
                type) + a1 * (s/sStar)^(q1)
        }
        else {
            price <- K - s
        }
    }
    return(round(price, 2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
