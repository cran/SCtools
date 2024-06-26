% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plac.dist.R
\name{plac.dist}
\alias{plac.dist}
\alias{plac_dist}
\title{Plot the distribution of placebo samples for synthetic control 
    analysis with multiple treated units.}
\usage{
plac.dist(multiple.synth, nboots = 500)

plac_dist(multiple.synth, nboots = 500)
}
\arguments{
\item{multiple.synth}{An object returned by the function 
\code{\link{multiple.synth}}}

\item{nboots}{Number of bootstrapped samples of placebos to take.
Default is \code{500}. It should be higher for more reliable inference.}
}
\value{
\describe{
\item{p}{The plot.}
\item{att.t}{The observed average treatment effect.}
\item{df}{Dataframe where each row is the ATT for one bootstrapped placebo 
   sample, used to build the distribution plot.}
\item{p.value}{Proportion of bootstrapped placebo samples ATTs which are
more extreme than the observed average treatment effect. Equivalent to a 
p-value in a two-tailed test.}
}
}
\description{
Takes the output object of \code{\link{multiple.synth}} creates a 
    distribution of placebo average treatment effects, to test the 
    significance of the observed ATE. Does so by sampling k placebos 
    (where k = the number of treated units) nboots times, and calculating 
    the average treatment effect of the k placebos each time.
}
\examples{
\donttest{
## Using the toy data from Synth:
library(Synth)
data(synth.data)
set.seed(42)
## Run the function similar to the dataprep() setup:
multi <- multiple.synth(foo = synth.data,
                       predictors = c("X1", "X2", "X3"),
                       predictors.op = "mean",
                       dependent = "Y",
                       unit.variable = "unit.num",
                       time.variable = "year",
                       treatment.time = 1990,
                       special.predictors = list(
                         list("Y", 1991, "mean"),
                         list("Y", 1985, "mean"),
                         list("Y", 1980, "mean")
                       ),
                       treated.units = c(2,7),
                       control.units = c(29, 13, 17, 32),
                       time.predictors.prior = c(1984:1989),
                       time.optimize.ssr = c(1984:1990),
                       unit.names.variable = "name",
                       time.plot = 1984:1996, gen.placebos = TRUE, Sigf.ipop = 2,
                       strategy = 'multicore' )

## Plot with the average path of the treated units and the average of their
## respective synthetic controls:

multi$p

## Bootstrap the placebo units to get a distribution of placebo average
## treatment effects, and plot the distribution with a vertical line 
## indicating the actual ATT:

att.test <- plac.dist(multi)
att.test$p
}

}
