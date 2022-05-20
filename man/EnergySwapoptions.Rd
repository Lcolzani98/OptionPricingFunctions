\name{EnergySwapoptions}
\alias{EnergySwapoptions}
\title{Price of a Energy Swapoptions}
\description{
compute the price of a energy swapoption}
\usage{
EnergySwapoptions(s, K, f, x, n, r, r_x, r_b, r_e, r_p, v, t, t_b, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{underlying price}
  \item{K}{strike price}
  \item{f}{foward/swap price observed in the market}
  \item{x}{number of compounding per year}
  \item{n}{number of settlements in the delivery period for the particular forward contracts}
  \item{r}{risK  free rate}
  \item{r_x}{swap rate starting at the beginning of the delivery period and ending at the end of the delivery period with j compoundings per year, equal to the number of fixings in the delivery period.}
  \item{r_b}{risK -free continuous compounding zero coupon rate with t_b years to maturity}
  \item{r_e}{risK -free continuous compounding zero coupon rate with time to maturity equalto from now to the end of the delivery period}
  \item{r_p}{risK -free continuous compounding zero coupon rate with forward start at the option maturity t and ending at the beginning of the delivery period t_b}
  \item{v}{volatility}
  \item{t}{time to maturity of the option}
  \item{t_b}{the time to the beginning of the forward delivery period}
  \item{type}{type of option "C" Call or "P" Put}
}
\details{European options on energy swaps, also called energy swaptions, are options that at maturity give a delivery of an energy swap at the strike price (but not necessarily physical delivery of any energy). The swap can have either financial or physical settlement. If a call swaption is in-the-money at maturity, the option has delivery of a swap. The payout from the option is thus not received immediately at expiration, but rather during the delivery period of the underlying swap (forward).
The energy call swaption formula is derived from Haug, 2005a
}
\value{price of a Energy Swapotion given the underlying price s, strike price K, foward/swap price observed in the market f. the number of compounding per year x, the number of settlements in the delivery period for the particular forward contracts n, risK  free rate r, swap rate starting at the beginning of the delivery period and ending at the end of the delivery period with j compoundings per year, equal to the number of fixings in the delivery period r_x , risK -free continuous compounding zero coupon rate with t_b years to maturity r_b, risK -free continuous compounding zero coupon rate with time to maturity equal to from now to the end of the delivery period r_e, risK -free continuous compounding zero coupon rate with forward start at the option maturity t and ending at the beginning of the delivery period t_b r_p, volatility v, time to maturity of the option t, 
the time to the beginning of the forward delivery period t_b, type of option "C" Call or "P" Put}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{
EnergySwapoptions(33,35,33,365,92,0.05,0.05,0.05,0.05,0.05,0.18,0.5,0.546,"C")
## The function is currently defined as
function (s, K, f, x, n, r, r_x, r_b, r_e, r_p, v, t, t_b, type) 
{
    Black76 <- function(s, K, f, r, v, t, type) {
        Black76 <- d1 <- (log(f/K) + (v^(2)/2) * t)/(v * sqrt(t))
        d2 <- d1 - v * sqrt(t)
        if (type == "C") {
            black76 <- exp(-r * t) * (f * pnorm(d1) - K * pnorm(d2))
        }
        if (type == "P") {
            black76 <- exp(-r * t) * (K * pnorm(-d2) - f * pnorm(-d1))
        }
        return(black76)
    }
    if (type == "C") {
        price <- (1 - (1/(1 + r_x/x)^n))/r_x * x/n * exp(-r_p * 
            (t_b - t)) * Black76(s, K , f, r, v, t,type)
    }
    if (type == "P") {
        price <- (1 - (1/(1 + r_x/x)^n))/r_x * x/n * exp(-r_p * 
            (t_b - t)) * Black76(s, K , f, r, v, t,type)
    }
    return(round(price, 2))
  }
}

