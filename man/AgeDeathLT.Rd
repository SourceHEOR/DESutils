\name{AgeDeathLT}
\alias{AgeDeathLT}
\title{Generate stochastic age at death based on lifetables}
\usage{
  AgeDeathLT(age, country, male = TRUE, prob = runif(1),
    smr = 1, data.location = "./data/lifetables",
    prob2 = runif(1))
}
\arguments{
  \item{age}{currrent age of individual}

  \item{country}{country lifetable to be sampled from.
  england, england&Wales, Scotland, Wales, UK}

  \item{male}{gender of individual (male: TRUE,
  female:FALSE)}

  \item{prob}{probability, normally random number}

  \item{smr}{standardised mortality ratio. Must be greater
  than 0. 1 is default.}

  \item{data.location}{where the data are stored}

  \item{prob2}{another probability. Normally random. To
  provide non-integer ages}
}
\description{
  Returns a stochastic age of death for an individual of a
  given age and gender. Multiple regions are supported and
  a stndardised mortality ratio may be applied.
}
\examples{
my.age <- 26
my.country <- c("england")
my.smr <-1.3
AgeDeathLT(age = my.age, country = my.country, smr = my.smr)
}

