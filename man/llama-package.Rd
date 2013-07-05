\name{llama-package}
\alias{llama-package}
\alias{llama}
\docType{package}
\title{
Leveraging Learning to Automatically Manage Algorithms
}
\description{
Leveraging Learning to Automatically Manage Algorithms provides functionality to
read and process performance data for algorithms, facilitate building models
that predict which algorithm to use in which scenario and ways of evaluating
them.
}
\details{
\tabular{ll}{
Package: \tab llama\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2013-07-05\cr
Depends: \tab plyr, foreach\cr
Suggests: \tab RWeka, FSelector\cr
License: \tab BSD\cr
}
The package provides functions to read performance data, build performance
models that enable selection of algorithms (using external machine learning
functions) and evaluate those models.

Data is input using \code{input} and can then be used to learn
performance models. There are currently four main ways to create models.
Classification (\code{classify}) creates a single machine learning model
that predicts the algorithm to use as a label. Classification of pairs of
algorithms (\code{classifyPairs}) creates a classification model for each pair
of algorithms that predicts which one is better and aggregates these predictions
to determine the best overall algorithm. Clustering (\code{cluster}) clusters
the problems to solve and assigns the best algorithm to each cluster. Regression
(\code{regression}) trains separate models for all available algorithms,
predicts the performance on a problem independently and chooses the algorithm
with the best predicted performance.

Various functions to split the data into training and test set(s) and to
evaluate the performance of the learned models are provided.

Please note that this is an alpha release. Bugs should be expected and the code
used with care. More sophisticated functionality has not been implemented yet.
Function names and interfaces may change in future versions.

At the moment, the implementation of the functions is very much geared towards
\code{RWeka}, an R package to interface with the Weka machine learning toolkit.
While in theory using other packages and implementations should be possible,
there may be problems in practice.

The model building functions are transparently parallelizable by loading a
suitable backend for parallel computation. Note that this does \emph{not} mean
that all machine learning algorithms used for building models can be
parallelized safely. In particular \code{RWeka} functions cannot be run in
parallel safely without loading the package in each executor independently.
}
\author{
Lars Kotthoff

contributions by Barry Hurley

Maintainer: Lars Kotthoff <larsko@4c.ucc.ie>
}
\references{
Kotthoff, L. (2013)
LLAMA: Leveraging Learning to Automatically Manage Algorithms.
\emph{arXiv:1306.1031}.

Kotthoff, L. (2012)
Algorithm Selection for Combinatorial Search Problems: A survey.
\emph{University College Cork (Technical Report)}.
}
\keyword{ package }
\examples{
\dontrun{
library(RWeka)

data(satsolvers)
trainTest = cvFolds(satsolvers)

res = classify(classifier=J48, data=trainTest)
# print the total number of successes
print(sum(unlist(successes(trainTest, res$predictions))))
# print the total misclassification penalty
print(sum(unlist(misclassificationPenalties(trainTest, res$predictions))))
# print the total PAR10 score
print(sum(unlist(parscores(trainTest, res$predictions))))

# number of total successes for virtual best solver for comparison
print(sum(successes(satsolvers, vbs(satsolvers))))

# print predictions on the entire data set
print(res$predictor(subset(satsolvers$data, TRUE, satsolvers$features)))

# filter features and train a regression model
library(FSelector)

filtered = featureFilter(cfs, satsolvers)
folds = cvFolds(filtered)
res = regression(regressor=LinearRegression, data=folds)
# print the total number of successes
print(sum(unlist(successes(folds, res$predictions))))
}
}
