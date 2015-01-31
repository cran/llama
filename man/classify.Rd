\name{classify}
\alias{classify}
\title{
Classification model
}
\description{
Build a classification model that predicts the algorithm to use based on the
features of the problem.
}
\usage{
classify(classifier = NULL, data = NULL,
    pre = function(x, y=NULL) { list(features=x) },
    save.models = NA, use.weights = TRUE)
}
\arguments{
  \item{classifier}{
  the mlr classifier to use. See examples.

  The argument can also be a list of such classifiers.
}
  \item{data}{
  the data to use with training and test sets. The structure returned by
  one of the partitioning functions.
}
  \item{pre}{
  a function to preprocess the data. Currently only \code{normalize}.
  Optional. Does nothing by default.
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
\code{classify} takes the training and test sets in \code{data} and
processes it using \code{pre} (if supplied). \code{classifier} is called to
induce a classifier. The learned model is used to make predictions on the test
set(s).

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.
The \code{parallelMap} level is "llama-fold".

If the given classifier supports case weights and \code{use.weights} is
\code{TRUE}, the performance difference between the best and the worst algorithm
is passed as a weight for each instance.

If a list of classifiers is supplied in \code{classifier}, ensemble
classification is performed. That is, the models are trained and used to make
predictions independently. For each instance, the final prediction is determined
by majority vote of the predictions of the individual models -- the class that
occurs most often is chosen. If the list given as \code{classifier} contains a
member \code{.combine} that is a function, it is assumed to be a classifier with
the same properties as the other ones and will be used to combine the ensemble
predictions instead of majority voting. This classifier is passed the original
features and the predictions of the classifiers in the ensemble.

If all predictions of an underlying machine learning model are \code{NA}, the
prediction will be \code{NA} for the algorithm and \code{-Inf} for the score.

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
 number of classifiers that predicted the respective algorithm. If stacking is
 used, only the best algorithm for each algorithm-instance pair is predicted
 with a score of 1.}
 \item{predictor}{a function that encapsulates the classifier learned on the
 \emph{entire} data set. Can be called with data for the same features with the
 same feature names as the training data to obtain predictions in the same
 format as the \code{predictions} member.}
 \item{models}{the list of models trained on the \emph{entire} data set. This is
 meant for debugging/inspection purposes and does not include any models used to
 combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{classifyPairs}}, \code{\link{cluster}}, \code{\link{regression}},
\code{\link{regressionPairs}}
}
\references{
Kotthoff, L., Miguel, I., Nightingale, P. (2010)
Ensemble Classification for Constraint Solver Configuration.
\emph{16th International Conference on Principles and Practices of Constraint Programming}, 321--329.
}
\examples{
if(Sys.getenv("NOT_CRAN") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

res = classify(classifier=makeLearner("classif.J48"), data=folds)
# the total number of successes
sum(successes(folds, res))
# predictions on the entire data set
res$predictor(satsolvers$data[satsolvers$features])

res = classify(classifier=makeLearner("classif.svm"), data=folds)

# ensemble classification
rese = classify(classifier=list(makeLearner("classif.J48"),
                                makeLearner("classif.IBk"),
                                makeLearner("classif.svm")),
                data=folds)

# ensemble classification with a classifier to combine predictions
rese = classify(classifier=list(makeLearner("classif.J48"),
                                makeLearner("classif.IBk"),
                                makeLearner("classif.svm"),
                                .combine=makeLearner("classif.J48")),
                data=folds)
}
}
\keyword{ models }
