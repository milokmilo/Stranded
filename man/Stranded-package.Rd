\name{Stranded-package}
\alias{Stranded-package}
\alias{Stranded}
\docType{package}
\title{
Calculate Biological Parameters from Stranded Cetaceans
}
\description{
This package calculates biological parameters from stranded marine mammals, mainly dolphins with a fairly good sample size. The package calculates the mortality-at-age of the population based on life tables and Siler and/or Heligman-Pollard functions, assuming bycatch or other threat that cause an extra mortality. 

}
\details{
\tabular{ll}{
Package: \tab Stranded\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-07-01\cr
License: \tab GPL (>=2)\cr
}
You can fit a Siler model from stranded aged dolphins or a more complex Heligman-Pollard model with bycatch information

}
\author{
Camilo Saavedra with code from \code{\link[HPbayes:HPbayes-package]{HPbayes}} and \code{\link[demogR:demogR-package]{demogR}}

Maintainer: Camilo Saavedra <camilo.saavedra@vi.ieo.es>

}
\references{
Heligman, Larry and John H. Pollard. 1980 "The Age Pattern of Mortality." \emph{Journal of the Institute of Actuaries} \bold{107}:49--80.

Poole, David and Adrian Raftery. 2000. "Inference for Deterministic Simulation Models: The Bayesian Melding Approach." \emph{Journal of the American Statistical Association} \bold{95}:1244--1255.

Raftery, Adrian and Le Bao. 2009. "Estimating and Projecting Trends in HIV/AIDS Gen- eralized Epidemics Using Incremental Mixture Importance Sampling." Technical Report 560, Department of Statistics, University of Washington.
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the ~~
~~ R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
\code{\link[MASS:MASS-package]{MASS}}
}
\examples{
HP()
Siler()
}
