\name{RefCostRange}
\alias{RefCostRange}
\title{Summary statistics for selection of NHS ref costs}
\usage{
  RefCostRange(data)
}
\arguments{
  \item{data}{file path a .csv file in the same format as
  NHS ref costs, with 9 columns of data}
}
\description{
  Returns weighted mean, std. error (log scale) and 95% CI
  for a group of NHS Reference Costs. Assumes that they
  follow a log standard normal distribution.
}

