\name{regression}
\alias{regression}
\title{
Regression model
}
\description{
Build a regression model that predicts the algorithm to use based on the
features of the problem.
}
\usage{
regression(regressor = NULL, data = NULL,
    pre = function(x, y=NULL) { list(features=x) },
    combine = NULL, expand = identity, save.models = NA,
    use.weights = TRUE)
}
\arguments{
  \item{regressor}{
  the mlr regressor to use. See examples.
}
  \item{data}{
  the data to use with training and test sets. The structure returned by
  one of the partitioning functions.
}
  \item{pre}{
  a function to preprocess the data. Currently only \code{normalize}.
  Optional. Does nothing by default.
}
  \item{combine}{
  the function used to combine the predictions of the individual regression
  models for stacking. Default \code{NULL}. See details.
}
  \item{expand}{
  a function that takes a matrix of performance predictions (columns are
  algorithms, rows problem instances) and transforms it into a matrix with the
  same number of rows. Only meaningful if \code{combine} is not null. Default is
  the identity function, which will leave the matrix unchanged. See examples.
}
  \item{save.models}{
  Whether to serialize and save the models trained during evaluation of the
  model. If not \code{NA}, will be used as a prefix for the file name.
}
  \item{use.weights}{
  Whether to use instance weights if supported. Default \code{TRUE}.
}
}
\details{
\code{regression} takes \code{data} and processes it using \code{pre} (if
supplied). \code{regressor} is called to induce separate regression models for
each of the algorithms to predict its performance. The best algorithm is
determined from the predicted performances by examining whether performance is
to be minimized or not, as specified when creating the data structure though
\code{input}.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.
The \code{parallelMap} level is "llama.fold".

If \code{combine} is not null, it is assumed to be an mlr classifier and will be
used to learn a model to predict the best algorithm given the original features
and the performance predictions for the individual algorithms. If this
classifier supports weights and \code{use.weights} is \code{TRUE}, they will be
passed as the difference between the best and the worst algorithm. Optionally,
\code{expand} can be used to supply a function that will modify the predictions
before giving them to the classifier, e.g. augment the performance predictions
with the pairwise differences (see examples).

If all predictions of an underlying machine learning model are \code{NA}, the
prediction will be \code{NA} for the algorithm and \code{-Inf} for the score if
the performance value is to be maximised, \code{Inf} otherwise.

If \code{save.models} is not \code{NA}, the models trained during evaluation are
serialized into files. Each file contains a list with members \code{model} (the
mlr model), \code{train.data} (the mlr task with the training data), and
\code{test.data} (the data frame with the test data used to make predictions).
The file name starts with \code{save.models}, followed by the ID of the machine
learning model, followed by "combined" if the model combines predictions of
other models, followed by the number of the fold. Each model for each fold is
saved in a different file.
}
\value{
 \item{predictions}{a data frame with the predictions for each instance and test
 set. The columns of the data frame are the instance ID columns (as determined
 by \code{input}), the algorithm, the score of the algorithm, and the iteration
 (e.g. the number of the fold for cross-validation). More than one prediction
 may be made for each instance and iteration. The score corresponds to the
 predicted performance value. If stacking is used, each prediction is simply the
 best algorithm with a score of 1.}
 \item{predictor}{a function that encapsulates the regression model learned on
 the \emph{entire} data set. Can be called with data for the same features with
 the same feature names as the training data to obtain predictions in the same
 format as the \code{predictions} member.}
 \item{models}{the list of models trained on the \emph{entire} data set. This is
 meant for debugging/inspection purposes and does not include any models used to
 combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{classify}}, \code{\link{classifyPairs}}, \code{\link{cluster}},
\code{\link{regressionPairs}}
}
\references{
Kotthoff, L. (2012)
Hybrid Regression-Classification Models for Algorithm Selection.
\emph{20th European Conference on Artificial Intelligence}, 480--485.
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

res = regression(regressor=makeLearner("regr.lm"), data=folds)
# the total number of successes
sum(successes(folds, res))
# predictions on the entire data set
res$predictor(satsolvers$data[satsolvers$features])

res = regression(regressor=makeLearner("regr.ksvm"), data=folds)

# combine performance predictions using classifier
ress = regression(regressor=makeLearner("regr.ksvm"),
                  data=folds,
                  combine=makeLearner("classif.J48"))

# add pairwise differences to performance predictions before running classifier
ress = regression(regressor=makeLearner("regr.ksvm"),
                  data=folds,
                  combine=makeLearner("classif.J48"),
                  expand=function(x) { cbind(x, combn(c(1:ncol(x)), 2,
                         function(y) { abs(x[,y[1]] - x[,y[2]]) })) })
}
}
\keyword{ regression }
