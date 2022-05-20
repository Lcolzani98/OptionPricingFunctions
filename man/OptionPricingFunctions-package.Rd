\name{OptionPricingFunctions-package}
\alias{OptionPricingFunctions-package}
\alias{OptionPricingFunctions}
\docType{package}
\title{
\packageTitle{OptionPricingFunctions}
}
\description{
\packageDescription{OptionPricingFunctions}
}
\details{
To use the package you need to install it and to run library("name of the package"), after that you can choose the function you waNt to use to price your option
}
\author{
\packageAuthor{OptionPricingFunctions}

Maintainer: \packageMaintainer{OptionPricingFunctions}
}
\references{At the complete guide to option pricing formulas by Espen Gaarder Haug}
\keyword{ package }
\seealso{}
\examples{
GARO(80,85,0.05,0.08,0.2,0.25,"P")
SPO(10,100,0.08,0.02,0.3,0.5,2,"C")
FSO(60,1,0.08,0.04,0.3,0.25,1.1,"C")
ES(35,365,90,0.05,0.5,0.04)
SWPTN(4,0.07,0.075,0.06,2,0.2,2,"C")
APO(90,100,0.1,0.02,0.25,"C")
PO(100,105,15000,0.07,0.02,0.05,0.3,0.3,-0.5,0.1,"C")
SOA(28,20,0.05,0.29,0.36,0.42,0.25,0,0,7,1,1,"C")
Margrabe(125,100,0.45,0.47,1,0,0.04,0.02)
Gap(50,50,57,0.09,0.09,0.2,0.5,"C")
CashorNothing(100,80, 10,0.06,0,0.35,0.75,"P") 
AoN(70,65,0.07,0.02,0.27,0.5,"P")
Supershare(100,90,110,0.1,0,0.2,0.25)
Powered(100,100,0.1,0.07,0.1,2,0.5,"C")
Chooser(50,50,0.08,0.08,0.25,0.5, 0.25)
DoubleBarrier(100,100,0.1,0.1,0.15,0.25,50,150,c(-5,5),0,0,"co")
Black76_F(19,19,19,0.1,0.28,0.75,1,"C")
GeneralizedBlackScholes(75,70,0.1,0.05,0.35,0.5,"P")
EnergySwapoptions(33,35,33,365,92,0.05,0.05,0.05,0.05,0.05,0.18,0.5,0.5466,"C")
BAWApproximation(100,100,0.1,0,0.15,0.1,"C")
TrinomialTree(100, 110, 0.1 ,0.1, 0.27, 0.5, 9, "P", "American")
CPO(100,100,0.1,0.07,0.1,2,30 ,0.5,"C")
EuropeanStandardMonteCarlo(100,90,0.05,0.02,0.3,1000,0.5,"C")
EuropeanBinomial(100,90,0.05,0.02,0.3,0.5,3,2,50,"C","CPC")
}