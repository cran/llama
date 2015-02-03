\name{classifyPairs}
\alias{classifyPairs}
\title{
Classification model for pairs of algorithms
}
\description{
Build a classification model for each pair of algorithms that predicts which one
is better based on the features of the problem. Predictions are aggregated to
determine the best overall algorithm.
}
\usage{
classifyPairs(classifier = NULL, data = NULL,
    pre = function(x, y=NULL) { list(features=x) }, combine = NULL,
    save.models = NA, use.weights = TRUE)
}
\arguments{
  \item{classifier}{
  the mlr classifier to use. See examples.
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
  The classifier function to predict the overall best algorithm given the
  predictions for pairs of algorithms. Optional. By default, the overall best
  algorithm is determined by majority vote.
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
\code{classifyPairs} takes the training and test sets in \code{data} and
processes it using \code{pre} (if supplied). \code{classifier} is called to
induce a classifier for each pair of algorithms to predict which one is better.
If \code{combine} is not supplied, the best overall algorithm is determined
by majority vote. If it is supplied, it is assumed to be a classifier with the
same properties as the other one. This classifier is passed the original
features and the predictions for each pair of algorithms.

Which algorithm is better of a pair is determined by comparing their performance
scores. Whether a lower performance number is better or not is determined by
what was specified when the LLAMA data frame was created.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.
The \code{parallelMap} level is "llama-fold".

If the given classifier supports case weights and \code{use.weights} is
\code{TRUE}, the performance difference between the best and the worst algorithm
is passed as a weight for each instance.

If all predictions of an underlying machine learning model are \code{NA}, the
prediction will be \code{NA} for the algorithm and \code{-Inf} for the score.

Training this model can take a very long time. Given \code{n} algorithms,
\code{choose(n, 2)} models are trained and evaluated. This is significantly
slower than the other approaches that train a single model or one for each
algorithm.

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
 number of times the respective algorithm was predicted to be better. If
 stacking is used, only the best algorithm for each algorithm-instance pair is
 predicted with a score of 1.}
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
\code{\link{classify}}, \code{\link{cluster}}, \code{\link{regression}},
\code{\link{regressionPairs}}
}
\references{
Xu, L., Hutter, F., Hoos, H. H., Leyton-Brown, K. (2011)
Hydra-MIP: Automated Algorithm Configuration and Selection for Mixed Integer Programming.
\emph{RCRA Workshop on Experimental Evaluation of Algorithms for Solving
Problems with Combinatorial Explosion}, 16--30.
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

res = classifyPairs(classifier=makeLearner("classif.J48"), data=folds)
# the total number of successes
sum(successes(folds, res))
# predictions on the entire data set
res$predictor(satsolvers$data[satsolvers$features])

# combine predictions using J48 induced classifier instead of majority vote
res = classifyPairs(classifier=makeLearner("classif.J48"),
                    data=folds,
                    combine=makeLearner("classif.J48"))
}
}
\keyword{ models }
