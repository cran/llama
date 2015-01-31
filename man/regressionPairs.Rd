\name{regressionPairs}
\alias{regressionPairs}
\title{
Regression model for pairs of algorithms
}
\description{
Builds regression models for each pair of algorithms that predict the
performance difference based on the features of the problem. The sum over all
pairs that involve a particular algorithm is aggregated as the score of the
algorithm.
}
\usage{
regressionPairs(regressor = NULL, data = NULL,
    pre = function(x, y=NULL) { list(features=x) }, combine = NULL,
    save.models = NA, use.weights = TRUE)
}
\arguments{
  \item{regressor}{
  the regression function to use. Must accept a formula of the values to predict
  and a data frame with features. Return value should be a structure that can be
  given to \code{predict} along with new data. See examples.
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
  \item{save.models}{
  Whether to serialize and save the models trained during evaluation of the
  model. If not \code{NA}, will be used as a prefix for the file name.
}
  \item{use.weights}{
  Whether to use instance weights if supported. Default \code{TRUE}.
}
}
\details{
\code{regressionPairs} takes the training and test sets in \code{data} and
processes it using \code{pre} (if supplied). \code{regressor} is called to
induce a regression model for each pair of algorithms to predict the performance
difference between them. If \code{combine} is not supplied, the best overall
algorithm is determined by summing the performance differences over all pairs
for each algorithm and ranking them by this sum. The algorithm with the largest
value is chosen. If it is supplied, it is assumed to be an mlr classifier. This
classifier is passed the original features and the predictions for each pair of
algorithms. If the classifier supports weights and \code{use.weights} is
\code{TRUE}, the performance difference between the best and the worst algorithm
is passed as weight.

The aggregated score for each algorithm quantifies how much better it is than
the other algorithms, where bigger values are better. Positive numbers denote
that the respective algorithm usually exhibits better performance than most of
the other algorithms, while negative numbers denote that it is usually worse.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.
The \code{parallelMap} level is "llama-fold".

Training this model can take a very long time. Given \code{n} algorithms,
\code{choose(n, 2) * n} models are trained and evaluated. This is significantly
slower than the other approaches that train a single model or one for each
algorithm.

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
 may be made for each instance and iteration. The score corresponds to how much
 better performance the algorithm delivers compared to the other algorithms in
 the portfolio. If stacking is used, each prediction is simply the best
 algorithm with a score of 1.}
 \item{predictor}{a function that encapsulates the classifier learned on the
 \emph{entire} data set. Can be called with data for the same features with the
 same feature names as the training data to obtain predictions in the same
 format as the \code{predictions} member.}
 \item{models}{the models for each pair of algorithms trained on the
 \emph{entire} data set. This is meant for debugging/inspection purposes and
 does not include any models used to combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{classify}}, \code{\link{classifyPairs}}, \code{\link{cluster}},
\code{\link{regression}}
}
\examples{
if(Sys.getenv("NOT_CRAN") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

model = regressionPairs(regressor=makeLearner("regr.lm"), data=folds)
# the total number of successes
sum(successes(folds, model))
# predictions on the entire data set
model$predictor(satsolvers$data[satsolvers$features])

# combine predictions using J48 induced classifier
model = regressionPairs(regressor=makeLearner("regr.lm"), data=folds,
    combine=makeLearner("classif.J48"))
}
}
\keyword{ models }
