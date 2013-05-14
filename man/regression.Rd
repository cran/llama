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
    combine = min, stack = F, expand = identity)
}
\arguments{
  \item{regressor}{
  the regression function to use. Must accept a formula of the values to predict
  and a data frame with features. Return value should be a structure that can be
  given to \code{predict} along with new data. See examples.
}
  \item{data}{
  the data to use with training and test sets. The structure returned by
  \code{trainTest} or \code{cvFolds}.
}
  \item{pre}{
  a function to preprocess the data. Currently only \code{normalize}.
  Optional. Does nothing by default.
}
  \item{combine}{
  the function used to combine the predictions of the individual regression
  models. Default \code{min}. See details.
}
  \item{stack}{
  whether the function given in \code{combine} is a classifier that should be
  used to predict the algorithm to choose based on the predicted performances.
  Default false.
}
  \item{expand}{
  a function that takes a matrix of performance predictions (columns are
  algorithms, rows problem instances) and transforms it into a matrix with the
  same number of rows. Only meaningful if \code{stack} is true. Default is the
  identity function, which will leave the matrix unchanged. See examples.
}
}
\details{
\code{regression} takes \code{data} and processes it using \code{pre} (if
supplied). \code{regressor} is called to induce separate regression models for
each of the algorithms to predict its performance. The predicted performances
are combined using \code{combine} to choose the best algorithm. That is, the
algorithm with the predicted performance that \code{combine} chooses is
selected.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.

If \code{stack} is true, the function given in \code{combine} is assumed to be a
classifier with the same properties as classifiers given to \code{classify}
and will be used to learn a model to predict the best algorithm given the
performance predictions for the individual algorithms. Optionally, \code{expand}
can be used to supply a function that will modify the features given to the
classifier, e.g. augment the performance predictions with the pairwise
differences (see examples).
}
\value{
 \item{predictions}{a list of lists with the predictions for each test set.}
 \item{predictor}{a function that encapsulates the regression model learned on
 the \emph{entire} data set. Can be called with data for the same features with
 the same feature names as the training data to obtain predictions.}
 \item{models}{the list of models trained on the \emph{entire} data set. This is
 meant for debugging/inspection purposes and does not include any models used to
 combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{classify}}, \code{\link{cluster}}
}
\references{
Kotthoff, L. (2012)
Hybrid Regression-Classification Models for Algorithm Selection.
\emph{20th European Conference on Artificial Intelligence}, 480--485.
}
\examples{
\dontrun{
data(satsolvers)
trainTest = cvFolds(satsolvers)

res = regression(regressor=lm, data=trainTest)
# the total number of successes
sum(unlist(successes(trainTest, res$predictions)))
# predictions on the entire data set
res$predictor(subset(satsolvers$data, TRUE, satsolvers$features))

library(RWeka)
res = regression(regressor=LinearRegression, data=trainTest)

# combine performance predictions using classifier
ress = regression(regressor=LinearRegression, data=trainTest, combine=J48, stack=TRUE)

# add pairwise differences to performance predictions before running classifier
ress = regression(regressor=LinearRegression, data=trainTest, combine=J48, stack=TRUE,
    expand=function(x) { cbind(x, combn(c(1:ncol(x)), 2,
        function(y) { abs(x[,y[1]] - x[,y[2]]) })) })
}
}
\keyword{ ~regression }
