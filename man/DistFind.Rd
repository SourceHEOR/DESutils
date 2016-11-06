\name{DistFind}
\alias{DistFind}
\title{Retrieve AIC statistics for alternative parametric survival models}
\usage{
  DistFind(surv.object, surv.data, vars = 1,
    dlist = c("exponential", "weibull", "lognormal", "loglogistic"),
    out.path = NULL, file.name = "output")
}
\arguments{
  \item{surv.object}{a 'Surv' object}

  \item{surv.data}{survival dataset on which calcs will be
  based}

  \item{vars}{variables to include within the models}

  \item{dlist}{list of distributions to feed to survreg}

  \item{out.path}{location for storing .txt fils of fited
  model.  Defaults to current working directory.}

  \item{file.name}{name of output file created}
}
\description{
  Returns table AIC table for list of distributions given
  in dlist. Writes model output to a .txt file
}
\examples{
require(survival)
bladder
bsurv <- Surv(bladder$stop, bladder$event)
a <- DistFind(bsurv,
              bladder,
              bladder$rx,
              out.path="data/bladder",
              file.name="output")
}

