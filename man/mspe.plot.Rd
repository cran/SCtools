% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mspe.plot.R
\name{mspe.plot}
\alias{mspe.plot}
\alias{mspe_plot}
\title{Plot the post/pre-treatment MSPE ratio}
\usage{
mspe.plot(
  tdf,
  discard.extreme = FALSE,
  mspe.limit = 20,
  plot.hist = FALSE,
  title = NULL,
  xlab = "Post/Pre MSPE ratio",
  ylab = NULL
)

mspe_plot(
  tdf,
  discard.extreme = FALSE,
  mspe.limit = 20,
  plot.hist = FALSE,
  title = NULL,
  xlab = "Post/Pre MSPE ratio",
  ylab = NULL
)
}
\arguments{
\item{tdf}{An object constructed by \code{\link{generate.placebos}}.}

\item{discard.extreme}{Logical. Whether or not placebos with high 
pre-treatement MSPE should be excluded from the plot.}

\item{mspe.limit}{Numerical. Used if \code{discard.extreme} is \code{TRUE}. 
It indicates how many times the pretreatment MSPE of a placebo should 
be higher than that of the treated unit to be considered extreme and 
discarded. Default is \code{20}.}

\item{plot.hist}{Logical. If \code{FALSE}, a dotplot with each unit name 
and its post/pre treatment MSPE ratio is produced. If \code{TRUE}, 
a histogram is produced, with the frequency of each ratio. 
Should be set to \code{TRUE} when there are many controls, to make 
visualization easier.}

\item{title}{Character. Optional. Title of the plot.}

\item{xlab}{Character. Optional. Label of the x axis.}

\item{ylab}{Character. Optional. Label of the y axis.}
}
\value{
\describe{
	 \item{p.dot }{Plot with the post/pre MSPE ratios for the treated unit and
	 each placebo indicated individually. Returned if \code{plot.hist} is 
	 \code{FALSE}.}
	 \item{p.dens }{Histogram of the distribution of post/pre MSPE ratios for
	 all placebos and the treated unit. Returned if \code{plot.hist} is 
	 \code{TRUE}.}
}
}
\description{
Plots the post/pre-treatment mean square prediction error ratio 
    for the treated unit and placebos.
}
\details{
Post/pre-treatement mean square prediction error ratio is the 
    difference between the observed outcome of a unit and its synthetic 
    control, before and after treatement. A higher ratio means a small 
    pretreatment prediction error (a good synthetic control), and a high 
    post-treatment MSPE, meaning a large difference between the unit and 
    its synthetic control after the intervention. By calculating this ratio 
    for all placebos, the test can be interpreted as looking at how likely 
    the result obtained for a single treated case with a synthetic control 
    analysis could have occurred by chance given no treatement. 
    For more detailed description, see Abadie, Diamond, and Hainmueller (2011, 2014).
}
\examples{
\dontshow{## Example with toy data from Synth
library(Synth)
# Load the simulated data
data(synth.data)

# Execute dataprep to produce the necessary matrices for synth
dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 17),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
)

# run the synth command to create the synthetic control
synth.out <- synth(dataprep.out, Sigf.ipop=1)

tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 1)
## Test how extreme was the observed treatment effect given the placebos:
ratio <- mspe.test(tdf)
ratio$p.val

mspe.plot(tdf, discard.extreme = FALSE)
}
\donttest{## Example with toy data from 'Synth'
library(Synth)
# Load the simulated data
data(synth.data)

# Execute dataprep to produce the necessary matrices for 'Synth'
dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
)

# run the synth command to create the synthetic control
synth.out <- synth(dataprep.out, Sigf.ipop=2)

## run the generate.placebos command to reassign treatment status
## to each unit listed as control, one at a time, and generate their
## synthetic versions. Sigf.ipop = 2 for faster computing time. 
## Increase to the default of 5 for better estimates. 
tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)

## Test how extreme was the observed treatment effect given the placebos:
ratio <- mspe.test(tdf)
ratio$p.val

mspe.plot(tdf, discard.extreme = FALSE)
}   
}
\references{
Abadie, A., Diamond, A., Hainmueller, J. (2014). 
  Comparative Politics and the Synthetic Control Method. 
  American Journal of Political Science Forthcoming 2014.

   Synthetic : An R Package for Synthetic Control Methods in Comparative 
   Case Studies. Journal of Statistical Software 42 (13) 1–17.

   Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for 
   Synthetic Control Methods in Comparative Case Studies. 
   Journal of Statistical Software 42 (13) 1–17.

   Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for 
   Comparative Case Studies: Estimating the Effect of California's Tobacco 
   Control Program. Journal of the American Statistical Association 
   105 (490) 493–505.
}
\seealso{
\code{\link{generate.placebos}}, \code{\link{mspe.test}}, 
    \code{\link{plot_placebos}}, \code{\link[Synth]{synth}}
}
