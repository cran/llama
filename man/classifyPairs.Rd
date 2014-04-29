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
    pre = function(x, y=NULL) { list(features=x) }, combinator = NULL)
}
\arguments{
  \item{classifier}{
  the classifier function to use. Must accept a formula of the values to predict
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
  \item{combinator}{
  The classifier function to predict the overall best algorithm given the
  predictions for pairs of algorithms. Optional. By default, the overall best
  algorithm is determined by majority vote.
}
}
\details{
\code{classifyPairs} takes the training and test sets in \code{data} and
processes it using \code{pre} (if supplied). \code{classifier} is called to
induce a classifier for each pair of algorithms to predict which one is better.
The predictions for pairs of algorithms are aggregated to determine the best
overall algorithm using \code{combinator} (if supplied). It \code{combinator} is
not supplied, the best overall algorithm is determined by majority vote.

Which algorithm is better of a pair is determined by comparing their performance
scores. Whether a lower performance number is better or not is determined by
what was specified when the LLAMA data frame was created.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.

Training this model can take a very long time. Given \code{n} algorithms,
\code{choose(n, 2)} models are trained and evaluated. This is significantly
slower than the other approaches that train a single model or one for each
algorithm.
}
\value{
 \item{predictions}{a list of lists of data frames with the predictions for each
 test set. Each data frame has columns \code{algorithm} and \code{score} and is
 sorted according to preference, with the most preferred algorithm first. The
 score corresponds to the number of times the respective algorithm was predicted
 to be faster. If stacking is used, each data frame contains simply the best
 algorithm with a score of 1.}
 \item{predictor}{a function that encapsulates the classifier learned on the
 \emph{entire} data set. Can be called with data for the same features with the
 same feature names as the training data to obtain predictions.}
 \item{models}{the models for each pair of algorithms trained on the
 \emph{entire} data set. This is meant for debugging/inspection purposes and
 does not include any models used to combine predictions of individual models.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{classify}}, \code{\link{cluster}}, \code{\link{regression}}
}
\references{
Xu, L., Hutter, F., Hoos, H. H., Leyton-Brown, K. (2011)
Hydra-MIP: Automated Algorithm Configuration and Selection for Mixed Integer Programming.
\emph{RCRA Workshop on Experimental Evaluation of Algorithms for Solving
Problems with Combinatorial Explosion}, 16--30.
}
\examples{
\dontrun{
data(satsolvers)
trainTest = cvFolds(satsolvers)

library(RWeka)
res = classifyPairs(classifier=J48, data=trainTest)
# the total number of successes
sum(successes(trainTest, res))
# predictions on the entire data set
res$predictor(subset(satsolvers$data, TRUE, satsolvers$features))

# combine predictions using J48 induced classifier instead of majority vote
res = classifyPairs(classifier=J48, data=trainTest, combinator=J48)
}
}
\keyword{ ~models }
