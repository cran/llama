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
    pre = function(x, y=NULL) { list(features=x) })
}
\arguments{
  \item{classifier}{
  the classifier function to use. Must accept a formula of the values to predict
  and a data frame with features. Return value should be a structure that can be
  given to \code{predict} along with new data. See examples.

  The argument can also be a list of such classifiers.
}
  \item{data}{
  the data to use with training and test sets. The structure returned by
  \code{trainTest} or \code{cvFolds}.
}
  \item{pre}{
  a function to preprocess the data. Currently only \code{normalize}.
  Optional. Does nothing by default.
}
}
\details{
\code{classify} takes the training and test sets in \code{data} and
processes it using \code{pre} (if supplied). \code{classifier} is called to
induce a classifier. The learned model is used to make predictions on the test
set(s).

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.

If a list of classifiers is supplied in \code{classifier}, ensemble
classification is performed. That is, the models are trained and used to make
predictions independently. For each instance, the final prediction is determined
by majority vote of the predictions of the individual models -- the class that
occurs most often is chosen. If the list given as \code{classifier} contains a
member \code{.combine} that is a function, it is assumed to be a classifier with
the same properties as the other ones and will be used to combine the ensemble
predictions instead of majority voting.
}
\value{
 \item{predictions}{a list of lists with the predictions for each test set.}
 \item{predictor}{a function that encapsulates the classifier learned on the
 \emph{entire} data set. Can be called with data for the same features with the
 same feature names as the training data to obtain predictions.}
 \item{models}{the list of models trained on the \emph{entire} data set. This is
 meant for debugging/inspection purposes and does not include any models used to
 combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{cluster}}, \code{\link{regression}}
}
\references{
Kotthoff, L., Miguel, I., Nightingale, P. (2010)
Ensemble Classification for Constraint Solver Configuration.
\emph{16th International Conference on Principles and Practices of Constraint Programming}, 321--329.
}
\examples{
\dontrun{
data(satsolvers)
trainTest = cvFolds(satsolvers)

library(RWeka)
res = classify(classifier=J48, data=trainTest)
# the total number of successes
sum(unlist(successes(trainTest, res$predictions)))
# predictions on the entire data set
res$predictor(subset(satsolvers$data, TRUE, satsolvers$features))

library(e1071)
res = classify(classifier=svm, data=trainTest)

# ensemble classification
rese = classify(classifier=list(J48, IBk, svm), data=trainTest)

# ensemble classification with a classifier to combine predictions
rese = classify(classifier=list(J48, IBk, svm, .combine=J48), data=trainTest)
}
}
\keyword{ ~models }
