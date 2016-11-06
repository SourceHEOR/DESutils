\name{InfoTable}
\alias{InfoTable}
\title{Creating summary tables in one line of code}
\usage{
  InfoTable(data, vari, dpm = 3, dps = 3, std.dev = F,
    total.col = T, factor.na = F, ci.limit = 0.95, sub,
    html, comp = F)
}
\arguments{
  \item{data}{dataset to summarise}

  \item{total.col}{if true, produces a total column}

  \item{std.dev}{include standard deviations}

  \item{sub}{subgroups}
}
\description{
  Creates a nice summary table froma lsit of variables. Can
  output to HTML
}
\author{
  Matt Cooper
}

