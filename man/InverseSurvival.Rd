\name{InverseSurvival}
\alias{InverseSurvival}
\title{Generate stochastic time to event}
\usage{
  InverseSurvival(a, b = 1, distribution, time,
    coefficients)
}
\arguments{
  \item{a}{scale parameter of distribution}

  \item{b}{shape parameter (e.g. b for exponential
  distribution is 1)}

  \item{distribution}{one of "expo","weib","logl","logn" or
  "gomp"}

  \item{probability}{normally random number. Providing
  probability=0.5 gives median time to event.}

  \item{coefficients}{currently a single value representing
  the sum product of the values taken by variables and the
  associated coefficients}
}
\description{
  Returns inverse of survival distributions. Designed to
  give time to event.
}
\author{
  Catrin Treharne <catrin.treharne@abacusint.com>
}

