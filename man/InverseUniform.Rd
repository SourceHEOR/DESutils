\name{InverseUniform}
\alias{InverseUniform}
\title{Inverse of uniform distribution}
\usage{
  InverseUniform(lower = 0, upper = 1, prob = runif(1))
}
\arguments{
  \item{lower}{minimum time}

  \item{upper}{maximum time}

  \item{prob}{probaility, normally random}
}
\description{
  Provides time to event from an inverse distribution.
  Probably provided elsewhere by other functions.
}
\examples{
curr.age <- 26
max.age.death <- 85
InverseUniform(lower = curr.age, upper = max.age.death)
}

