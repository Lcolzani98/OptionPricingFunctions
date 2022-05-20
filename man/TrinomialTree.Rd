\name{TrinomialTree}
\alias{TrinomialTree}
\title{Trinomial Tree option pricing}
\description{Compute the price of a European or American call or put option using the trinomial tree model
}
\usage{
TrinomialTree(s, K, r, b, v, t, nSim, type1, type2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{price of the underlying}
  \item{K}{strike price}
  \item{r}{risk free rate}
  \item{b}{cost of carrying rate}
  \item{v}{volatility}
  \item{t}{time to maturity of the option}
  \item{nSim}{number of simulation}
  \item{type1}{type of option Call "C" or Put "P"}
  \item{type2}{type of option European "European" or American "American" }}
\details{The trinomial option pricing model is an option pricing model incorporating three possible values that an underlying asset can have in one time period. The three possible values the underlying asset can have in a time period may be greater than, the same as, or less than the current value.Before fill the input we must choose nSim(number of step) >= integer((b^2*t)/2*v^2) + 1 otherwise probability will be negative.
Our code is based on the Espen Haug's Excel VBA trinomial tree approach which is computationally faster than the one developed by Fabrice Rouah.}
\value{price of a European or American call or put option using the trinomial model with input the underlying price s, strike price K, risk free rate r, cost of carrying rate b, volatility v, time to maturitty of the option t, number of simulation, the type1 call "C" or  put "P" and the type2 "European" or "American"
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{TrinomialTree(100, 110, 0.1 ,0.1, 0.27, 0.5, 9, "P", "American")

## The function is currently defined as
function (s, K, r, b, v, t, nSim, type1, type2) 
{
    if (nSim < integer(((b^2 * t)/2 * v^2) + 1)) {
        print("Choose nSim >= integer(((b^2*t)/2*v^2) + 1) to avoid negative probabilities")
        break
    }
    else {
        priceTrinomial <- rep(0, 2 * nSim + 1)
        deltaT <- t/nSim
        up <- exp(v * sqrt(2 * deltaT))
        down <- exp(-v * sqrt(2 * deltaT))
        probabilityUp <- ((exp(b * deltaT/2) - exp(-v * sqrt(deltaT/2)))/(exp(v * 
            sqrt(deltaT/2)) - exp(-v * sqrt(deltaT/2))))^(2)
        probabilityDown <- ((exp(v * sqrt(deltaT/2)) - exp(b * 
            deltaT/2))/(exp(v * sqrt(deltaT/2)) - exp(-v * sqrt(deltaT/2))))^(2)
        probabilityStatic <- 1 - probabilityUp - probabilityDown
        for (k in 0:(2 * nSim)) {
            if (type1 == "C") {
                priceTrinomial[k + 1] <- max((s * up^(max(k - 
                  nSim, 0)) * down^(max(nSim * 2 - nSim - k, 
                  0))) - K, 0)
            }
            if (type1 == "P") {
                priceTrinomial[k + 1] <- max(K - (s * up^(max(k - 
                  nSim, 0)) * down^(max(nSim * 2 - nSim - k, 
                  0))), 0)
            }
        }
        for (j in (nSim - 1):0) {
            for (k in 0:(j * 2)) {
                if (type2 == "European") {
                  priceTrinomial[k + 1] <- (probabilityUp * priceTrinomial[k + 
                    3] + probabilityDown * priceTrinomial[k + 
                    1] + probabilityStatic * priceTrinomial[k + 
                    2]) * exp(-r * deltaT)
                }
                if (type2 == "American") {
                  if (type1 == "C") {
                    priceTrinomial[k + 1] <- max((s * up^(max(k - 
                      j, 0)) * down^(max(j * 2 - j - k, 0))) - 
                      K, (probabilityUp * priceTrinomial[k + 
                      3] + probabilityDown * priceTrinomial[k + 
                      1] + probabilityStatic * priceTrinomial[k + 
                      2]) * exp(-r * deltaT))
                  }
                  if (type1 == "P") {
                    priceTrinomial[k + 1] <- max(K - (s * up^(max(k - 
                      j, 0)) * down^(max(j * 2 - j - k, 0))), 
                      (probabilityUp * priceTrinomial[k + 3] + 
                        probabilityDown * priceTrinomial[k + 
                          1] + probabilityStatic * priceTrinomial[k + 
                        2]) * exp(-r * deltaT))
                  }
                }
            }
        }
        return(round(priceTrinomial[1], 2))
    }
  }
}
