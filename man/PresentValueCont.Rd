\name{PresentValueCont}
\alias{PresentValueCont}
\title{Present value of continous quantity}
\usage{
  PresentValueCont(annual.increase, discount.rate,
    time.to.change, time.at.start = 0)
}
\arguments{
  \item{annual.increase}{annual increase in the quantity of
  interest}

  \item{discount.rate}{e.g. enter 0.035 for 3.5 percent.}

  \item{time.to.change}{how long the quantity is accrued
  for}

  \item{time.at.start}{how far through the model are we
  when we start counting. If we are at the start, this
  value is 0.}
}
\description{
  Provides present value of a quantity which is accrued
  continuously. An example might be QALYs. In perfect
  health, these are accrued at 1 per year. They are
  discounted at 3.5 percent.
}
\examples{
#5 years in perfect health
qalys.per.year <- 1
disc.rate <- 0.035
duration <- 5 #5 years

#At start of model
PresentValueCont(qalys.per.year, disc.rate, duration)

#10 years into model
PresentValueCont(qalys.per.year, disc.rate, duration, time.at.start=10)
}

