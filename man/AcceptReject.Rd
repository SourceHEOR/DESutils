\name{AcceptReject}
\alias{AcceptReject}
\title{Determine which mututally exclusive outcome occurs}
\usage{
  AcceptReject(p1, p2 = 0, p3 = 0, p4 = 0, p5 = 0,
    probability = runif(1))
}
\arguments{
  \item{p1}{probability of 1st outcome}

  \item{p2}{probability of 2nd outcome}

  \item{p3}{probability of 3rd outcome}

  \item{p4}{probability of 4th outcome}

  \item{p5}{probability of 5th outcome}

  \item{probability}{value to evaluate against
  probabilities (normally random number)}
}
\description{
  Uses accept/reject methodology to determine which
  mututally exclusive event occurs (maximum 6 possible
  events). Note sum of probabilities should be less than 1.
  The probabiity of the final even is defined as 1 minus
  the sum of the other probabilities.
}
\examples{
prob.coin.is.heads <- 0.5
AcceptReject(prob.coin.is.heads)

dice1 <- 1/6
dice2 <- 1/6
dice3 <- 1/6
dice4 <- 1/6
dice5 <- 1/6
dice6 <- 1/6
AcceptReject(p1=dice1, p2=dice2, p3=dice3, p4=dice4, p5=dice5)
}

