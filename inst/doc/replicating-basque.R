## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 7, fig.align = "center")

## ----setup--------------------------------------------------------------------
library(SCtools)
library(Synth)

## -----------------------------------------------------------------------------
data("basque")

## -----------------------------------------------------------------------------
dataprep.out <- dataprep(
  foo = basque,
  predictors = c("school.illit", "school.prim", "school.med",
    "school.high", "school.post.high", "invest"),
  predictors.op = "mean",
  time.predictors.prior = 1964:1969,
  special.predictors = list(
    list("gdpcap", 1960:1969 ,"mean"),
    list("sec.agriculture", seq(1961, 1969, 2), "mean"),
    list("sec.energy", seq(1961, 1969, 2), "mean"),
    list("sec.industry", seq(1961, 1969, 2), "mean"),
    list("sec.construction", seq(1961, 1969, 2), "mean"),
    list("sec.services.venta", seq(1961, 1969, 2), "mean"),
    list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
    list("popdens",               1969,               "mean")),
  dependent = "gdpcap",
  unit.variable = "regionno",
  unit.names.variable = "regionname",
  time.variable = "year",
  treatment.identifier = 17,
  controls.identifier = c(2:16, 18),
  time.optimize.ssr = 1960:1969,
  time.plot = 1955:1997)


## ----echo=FALSE---------------------------------------------------------------
synth.out <- readRDS("synth_out.rds")

## ----eval=FALSE---------------------------------------------------------------
#  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")

## -----------------------------------------------------------------------------
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

## -----------------------------------------------------------------------------
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "real per-capita GDP (1986 USD, thousand)", Xlab = "year",
          Ylim = c(0, 12), Legend = c("Basque country",
                                      "synthetic Basque country"), 
          Legend.position = "bottomright")


## ----echo=FALSE---------------------------------------------------------------
placebo <- readRDS("basque_placebo.rds")

## ----eval=FALSE---------------------------------------------------------------
#  placebo <- generate.placebos(dataprep.out = dataprep.out,
#                               synth.out = synth.out, strategy = "multiprocess")

## -----------------------------------------------------------------------------
plot_placebos(placebo)

## -----------------------------------------------------------------------------
mspe_plot(placebo)

