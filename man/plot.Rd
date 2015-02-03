\name{plot}
\alias{perfScatterPlot}
\title{
Plot convenience functions to visualise selectors
}
\description{
Functions to plot the performance of selectors and compare them to others.
}
\usage{
perfScatterPlot(metric, modelx, modely, datax, datay=datax,
    addCostsx=NULL, addCostsy=NULL, pargs=NULL, \dots)
}
\arguments{
  \item{metric}{the metric used to evaluate the model. Can be one of
    \code{misclassificationPenalties}, \code{parscores} or \code{successes}.}
  \item{modelx}{the algorithm selection model to be plotted on the x axis. Can
      be either a model returned by one of the model-building functions or a
      function that returns predictions such as \code{vbs} or the predictor
      function of a trained model.}
  \item{modely}{the algorithm selection model to be plotted on the y axis. Can
      be either a model returned by one of the model-building functions or a
      function that returns predictions such as \code{vbs} or the predictor
      function of a trained model.}
  \item{datax}{the data used to evaluate \code{modelx}. Will be passed to the
      \code{metric} function.}
  \item{datay}{the data used to evaluate \code{modely}. Can be omitted if the
      same as for \code{modelx}. Will be passed to the \code{metric} function.}
  \item{addCostsx}{whether to add feature costs for \code{modelx}. You should
      not need to set this manually, the default of \code{NULL} will have LLAMA
      figure out automatically depending on the model whether to add costs or
      not. This should always be true (the default) except for comparison
      algorithms (i.e. single best and virtual best).}
  \item{addCostsy}{whether to add feature costs for \code{modely}. You should
      not need to set this manually, the default of \code{NULL} will have LLAMA
      figure out automatically depending on the model whether to add costs or
      not. This should always be true (the default) except for comparison
      algorithms (i.e. single best and virtual best).}
  \item{pargs}{any arguments to be passed to \code{geom_points}.}
  \item{\dots}{any additional arguments to be passed to the metrics. For example
      the penalisation factor for \code{parscores}.}
}
\details{
\code{perfScatterPlot} creates a scatter plot that compares the performances of
two algorithm selectors. It plots the performance on each instance in the data
set for \code{modelx} on the x axis versus \code{modely} on the y axis. In
addition, a diagonal line is drawn to denote the line of equal performance for
both selectors.
}
\value{
A \code{ggplot} object.
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{misclassificationPenalties}}, \code{\link{parscores}}, \code{\link{successes}}
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)
model = classify(classifier=makeLearner("classif.J48"), data=folds)

# Simple plot to compare our selector to the single best in terms of PAR10 score
library(ggplot2)
perfScatterPlot(parscores,
        model, singleBest,
        folds, satsolvers) +
    scale_x_log10() + scale_y_log10() +
    xlab("J48") + ylab("single best")

# additional aesthetics for points
perfScatterPlot(parscores,
        model, singleBest,
        folds, satsolvers,
        pargs=aes(colour = scorex)) +
    scale_x_log10() + scale_y_log10() +
    xlab("J48") + ylab("single best")
}
}
\keyword{ models }
