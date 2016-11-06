\name{InvExponential}
\alias{InvExponential}
\title{Inverse exponential distribution}
\usage{
  InvExponential(mean.time, probability = runif(1))
}
\arguments{
  \item{mean.time}{mean time to event}

  \item{proability}{normally random number}
}
\description{
  Generate draw from inverse exponential distribution.
}
\examples{
time.to.see.doctor <- 1 #week
InvExponential(time.to.see.doctor)
}

