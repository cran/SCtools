#' @title Plot the post/pre-treatment MSPE ratio
#' @description Plots the post/pre-treatment mean square prediction error ratio 
#'     for the treated unit and placebos.
#' @param tdf An object constructed by \code{\link{generate.placebos}}.
#' @param discard.extreme Logical. Whether or not placebos with high 
#'    pre-treatement MSPE should be excluded from the plot.
#' @param mspe.limit Numerical. Used if \code{discard.extreme} is \code{TRUE}. 
#'     It indicates how many times the pretreatment MSPE of a placebo should 
#'     be higher than that of the treated unit to be considered extreme and 
#'     discarded. Default is \code{20}.
#' @param plot.hist Logical. If \code{FALSE}, a dotplot with each unit name 
#'     and its post/pre treatment MSPE ratio is produced. If \code{TRUE}, 
#'     a histogram is produced, with the frequency of each ratio. 
#'     Should be set to \code{TRUE} when there are many controls, to make 
#'     visualization easier.
#' @param title Character. Optional. Title of the plot.
#' @param xlab Character. Optional. Label of the x axis.
#' @param ylab Character. Optional. Label of the y axis.
#' @return \describe{
#'		 \item{p.dot }{Plot with the post/pre MSPE ratios for the treated unit and
#'		 each placebo indicated individually. Returned if \code{plot.hist} is 
#'		 \code{FALSE}.}
#'		 \item{p.dens }{Histogram of the distribution of post/pre MSPE ratios for
#'		 all placebos and the treated unit. Returned if \code{plot.hist} is 
#'		 \code{TRUE}.}
#' }
#' @details Post/pre-treatement mean square prediction error ratio is the 
#'     difference between the observed outcome of a unit and its synthetic 
#'     control, before and after treatement. A higher ratio means a small 
#'     pretreatment prediction error (a good synthetic control), and a high 
#'     post-treatment MSPE, meaning a large difference between the unit and 
#'     its synthetic control after the intervention. By calculating this ratio 
#'     for all placebos, the test can be interpreted as looking at how likely 
#'     the result obtained for a single treated case with a synthetic control 
#'     analysis could have occurred by chance given no treatement. 
#'     For more detailed description, see Abadie, Diamond, and Hainmueller (2011, 2014).
#' @references Abadie, A., Diamond, A., Hainmueller, J. (2014). 
#'   Comparative Politics and the Synthetic Control Method. 
#'   American Journal of Political Science Forthcoming 2014.
#' 
#'    Synthetic : An R Package for Synthetic Control Methods in Comparative 
#'    Case Studies. Journal of Statistical Software 42 (13) 1–17.
#' 
#'    Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for 
#'    Synthetic Control Methods in Comparative Case Studies. 
#'    Journal of Statistical Software 42 (13) 1–17.
#' 
#'    Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for 
#'    Comparative Case Studies: Estimating the Effect of California's Tobacco 
#'    Control Program. Journal of the American Statistical Association 
#'    105 (490) 493–505.
#' 
#' @seealso \code{\link{generate.placebos}}, \code{\link{mspe.test}}, 
#'     \code{\link{plot_placebos}}, \code{\link[Synth]{synth}}
#' @examples 
#' \dontshow{## Example with toy data from Synth
#' library(Synth)
#' # Load the simulated data
#' data(synth.data)
#' 
#' # Execute dataprep to produce the necessary matrices for synth
#' dataprep.out<-
#'   dataprep(
#'     foo = synth.data,
#'     predictors = c("X1"),
#'     predictors.op = "mean",
#'     dependent = "Y",
#'     unit.variable = "unit.num",
#'     time.variable = "year",
#'     special.predictors = list(
#'       list("Y", 1991, "mean")
#'     ),
#'     treatment.identifier = 7,
#'     controls.identifier = c(29, 2, 17),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#' )
#' 
#' # run the synth command to create the synthetic control
#' synth.out <- synth(dataprep.out, Sigf.ipop=1)
#' 
#' tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 1)
#' ## Test how extreme was the observed treatment effect given the placebos:
#' ratio <- mspe.test(tdf)
#' ratio$p.val
#' 
## Check visually how extreme is this value in the distribution:
#' mspe.plot(tdf, discard.extreme = FALSE)
#' }
#' \donttest{## Example with toy data from 'Synth'
#' library(Synth)
#' # Load the simulated data
#' data(synth.data)
#' 
#' # Execute dataprep to produce the necessary matrices for 'Synth'
#' dataprep.out<-
#'   dataprep(
#'     foo = synth.data,
#'     predictors = c("X1"),
#'     predictors.op = "mean",
#'     dependent = "Y",
#'     unit.variable = "unit.num",
#'     time.variable = "year",
#'     special.predictors = list(
#'       list("Y", 1991, "mean")
#'     ),
#'     treatment.identifier = 7,
#'     controls.identifier = c(29, 2, 13, 17),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#' )
#' 
#' # run the synth command to create the synthetic control
#' synth.out <- synth(dataprep.out, Sigf.ipop=2)
#' 
#' ## run the generate.placebos command to reassign treatment status
#' ## to each unit listed as control, one at a time, and generate their
#' ## synthetic versions. Sigf.ipop = 2 for faster computing time. 
#' ## Increase to the default of 5 for better estimates. 
#' tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)
#' 
#' ## Test how extreme was the observed treatment effect given the placebos:
#' ratio <- mspe.test(tdf)
#' ratio$p.val
#' 
## Check visually how extreme is this value in the distribution:
#' mspe.plot(tdf, discard.extreme = FALSE)
#' }   
#' @export 

mspe.plot <-
function(tdf,
         discard.extreme=FALSE,
         mspe.limit=20, 
         plot.hist = FALSE,
         title=NULL,
         xlab='Post/Pre MSPE ratio',
         ylab=NULL) {
  
  discard.extreme <- match_logical(discard.extreme)
  plot.hist <- match_logical(plot.hist)
  
  if(!is_tdf(tdf)){
    stop("`tdf` should be generated from the `generate.placebos function.`")
  }
  
  if(!is.logical(discard.extreme)){
    stop("`discard,extrene` should be one of TRUE or FALSE")
  }
  
  if(!discard.extreme & mspe.limit != 20){
  	warning('discard.extreme is FALSE. mspe.limit will be ignored.')
  }
  
  
  
  MSPE.ratios<-unit<-treat<-year<-mspe <- NULL 
  
    data<-mspe.test(tdf,discard.extreme=discard.extreme,mspe.limit=mspe.limit)
    test<-data.frame(data$test)
    test$treat<-'control units'
    test[nrow(test),3]<-as.character(test[nrow(test),2])
    if(!plot.hist) {
    p.dot<-ggplot2::ggplot(data=test,ggplot2::aes(x = MSPE.ratios,
                                y=stats::reorder(unit,MSPE.ratios,max),
                                colour=factor(treat),
                                shape=factor(treat)))+
      ggplot2::geom_point(size=4)+
      ggplot2::scale_color_manual(values = c('gray50','black'),
                         guide = 'none')+
      ggplot2::scale_shape_manual(values = c(17,15),guide = 'none') + 
      ggplot2::labs(y=ylab,
           x=xlab,
           title=title)+
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = 'gray80'),
            panel.grid.minor=ggplot2::element_line(colour='gray90'),
            panel.background = ggplot2::element_blank(),
            axis.line.x = ggplot2::element_line(colour = 'black'),
            axis.line.y = ggplot2::element_line(colour = 'black'),
            axis.text.y=ggplot2::element_text(colour='black'),
            axis.text.x=ggplot2::element_text(colour='black')
            )
    
    return(p.dot)
    
  } else {
    p.dens<-ggplot2::ggplot(data=test,ggplot2::aes(x = MSPE.ratios,
                                 fill=factor(treat)))+
      ggplot2::geom_histogram()+
      ggplot2::scale_fill_manual(values = c('gray50','black'),
                        guide=ggplot2::guide_legend(title=NULL)) + 
      ggplot2::labs(y='Frequency',x=xlab,title=title)+
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = 'gray80'),
            panel.grid.minor=ggplot2::element_line(colour='gray90'),
            panel.background = ggplot2::element_blank(),
            axis.line.x = ggplot2::element_line(colour = 'black'),
            axis.line.y = ggplot2::element_line(colour = 'black'),
            axis.text.y=ggplot2::element_text(colour='black'),
            axis.text.x=ggplot2::element_text(colour='black'),
            legend.position='bottom',
            legend.key=ggplot2::element_blank()
                           )
  return(p.dens)
  }
}

#' @rdname mspe.plot
#' @export
mspe_plot <- mspe.plot

