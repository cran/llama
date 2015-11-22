\name{tune}
\alias{tuneModel}
\title{
Tune the hyperparameters of the machine learning algorithm underlying a model
}
\description{
Functions to tune the hyperparameters of the machine learning algorithm
underlying a model with respect to a performance measure.
}
\usage{
tuneModel(ldf, llama.fun, learner, design, metric = parscores, nfolds = 10L,
    quiet = FALSE)
}
\arguments{
  \item{ldf}{the LLAMA data to use. The structure returned by \code{input}.}
  \item{llama.fun}{the LLAMA model building function.}
  \item{learner}{the mlr learner to use.}
  \item{design}{the data frame denoting the parameter values to try. Can be
    produced with the \code{ParamHelpers} package. See examples.}
  \item{metric}{the metric used to evaluate the model. Can be one of
    \code{misclassificationPenalties}, \code{parscores} or \code{successes}.}
  \item{nfolds}{the number of folds. Defaults to 10. If -1 is given,
    leave-one-out cross-validation folds are produced.}
  \item{quiet}{whether to output information on the intermediate values and
    progress during tuning.}
}
\details{
\code{tuneModel} finds the hyperparameters from the set denoted by \code{design}
of the machine learning algorithm \code{learner} that give the best performance
with respect to the measure \code{metric} for the LLAMA model type
\code{llama.fun} on data \code{ldf}. It uses a nested cross-validation
internally; the number of inner folds is given through \code{nfolds}, the number
of outer folds is either determined by any existing partitions of \code{ldf} or,
if none are present, by \code{nfolds} as well.

During each iteration of the inner cross-validation, all parameter sets
specified in \code{design} are evaluated and the one with the best performance
value chosen. The mean performance over all instances in the data is logged for
all evaluations. This parameter set is then used to build and evaluate a model
in the outer cross-validation. The predictions made by this model along with the
parameter values used to train it are returned.

Finally, a normal (not-nested) cross-validation is performed to find the best
parameter values on the \emph{entire} data set. The predictor of this model
along with the parameter values used to train it is returned. The interface
corresponds to the normal LLAMA model-building functions in that respect -- the
returned data structure is the same with a few additional values.

The evaluation across the folds sets will be parallelized automatically if a
suitable backend for parallel computation is loaded. The \code{parallelMap}
level is "llama.tune".
}
\value{
 \item{predictions}{a data frame with the predictions for each instance and test
 set. The structure is the same as for the underlying model building function
 and the predictions are the ones made by the models trained with the best
 parameter values for the respective fold.}
 \item{predictor}{a function that encapsulates the classifier learned on the
 \emph{entire} data set with the best parameter values determined on the
 \emph{entire} data set. Can be called with data for the same features with the
 same feature names as the training data to obtain predictions in the same
 format as the \code{predictions} member.}
 \item{models}{the list of models trained on the \emph{entire} data set. This is
 meant for debugging/inspection purposes.}
 \item{parvals}{the best parameter values on the entire data set used for
 training the \code{predictor} model.}
 \item{inner.parvals}{the best parameter values during each iteration of the
 outer cross-validation. These parameters were used to train the models that
 made the predictions in \code{predictions}.}
}
\author{
Bernd Bischl, Lars Kotthoff
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
library(ParamHelpers)
data(satsolvers)

learner = makeLearner("classif.J48")
# parameter set for J48
ps = makeParamSet(makeIntegerParam("M", lower = 1, upper = 100))
# generate 10 random parameter sets
design = generateRandomDesign(10, ps)
# tune with respect to PAR10 score (default) with 10 outer and inner folds
# (default)
res = tuneModel(satsolvers, classify, learner, design)
}
}
\keyword{ models }
