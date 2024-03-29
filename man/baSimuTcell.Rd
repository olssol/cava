% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basim_simu.R
\name{baSimuTcell}
\alias{baSimuTcell}
\title{Simulate t-cell counts using Negative binomial}
\usage{
baSimuTcell(bsize, par.err, par.other, nreps = 1e+06, ...)
}
\arguments{
\item{bsize}{batch size}

\item{par.err}{a list of gamma, delta, epsilon error}

\item{par.other}{other parameters including u0, u1, v, and beta}

\item{nreps}{number of repetitions}

\item{...}{reserved parameters}
}
\value{
simulation of t-cell counts with given parameters
}
\description{
Simulate t-cell counts using Negative binomial
}
