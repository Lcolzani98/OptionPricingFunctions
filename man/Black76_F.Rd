\name{Black76_F}
\alias{Black76_F}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BlacK  76-F formula}
\description{
compute the price of a call or put option using the BlacK  76-F
}
\usage{
Black76_F(s, K, f, r, v, t, t_f, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{price of the underlying}
  \item{K}{strike price}
  \item{f}{foward price}
  \item{r}{risK  free rate}
  \item{v}{volatility express in annual term}
  \item{t}{time to maturity of the option contract express in annual term}
  \item{t_f}{time to maturity of the foward contract express in annual term}
  \item{type}{type of the option Call "C" or Put "P"}
}
\details{
BlacK 's Model, also known as the Black  76 Model, is a versatile derivatives pricing model for valuing assets such as options on futures and capped variable rate debt securities. Traders in commodity markets often use the Black -76 model to value options on commodity futures. When it comes to commodity options on forwards, the Black -76 formula holds only for the case when the forward contract expires at the same time as the option contract T.
In the case where there is delivery of a forward contract that has a different expiration date, one has only lock ed in the payoff from the option but will receive the intrinsic value first at the forward's expiration. The BlacK -76 formula has to be adjusted for this effect. So we introduce the BlacK  76-F formula by Haug}
\value{price of a call or put european option using Black 76-F given price of underlying s, strike price K, foward price f, risk free rate r, volatility express in annual term v, time to maturity of the option t, time to maturity of the foward contract t_f and type of option "C" or "P" 
}
\references{Espen Gaarder Haug(2007):The Complete Guide to Option Pricing Formulas}
\author{Colzani Luca, Magni Marta, Mancassola Gaia, Kakkanattu Jenson}
\note{}
\seealso{}
\examples{Black76_F(19,19,19,0.1,0.28,0.75,1,"C")
## The function is currently defined as
function (s, K, f, r, v, t, t_f, type) 
{
    d1 <- (log(f/K) + (v^(2)/2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if (type == "C") {
        price <- exp(-r * t_f) * (f * pnorm(d1) - K * pnorm(d2))
    }
    if (type == "P") {
        price <- exp(-r * t_f) * (K * pnorm(-d2) - f * pnorm(-d1))
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
