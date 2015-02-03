\name{cluster}
\alias{cluster}
\title{
Cluster model
}
\description{
Build a cluster model that predicts the algorithm to use based on the features
of the problem.
}
\usage{
cluster(clusterer = NULL, data = NULL,
    bestBy = "performance",
    pre = function(x, y=NULL) { list(features=x) },
    save.models = NA)
}
\arguments{
  \item{clusterer}{
  the mlr clustering function to use. See examples.

  The argument can also be a list of such functions.
}
  \item{data}{
  the data to use with training and test sets. The structure returned by
  one of the partitioning functions.
}
  \item{bestBy}{
  the criteria by which to determine the best algorithm in a cluster. Can be one
  of "performance", "count", "successes". Optional. Defaults to "performance".
}
  \item{pre}{
  a function to preprocess the data. Currently only \code{normalize}.
  Optional. Does nothing by default.
}
  \item{save.models}{
  Whether to serialize and save the models trained during evaluation of the
  model. If not \code{NA}, will be used as a prefix for the file name.
}
}
\details{
\code{cluster} takes \code{data} and processes it using \code{pre} (if
supplied). \code{clusterer} is called to cluster the data. For each cluster, the
best algorithm is identified according to the criteria given in \code{bestBy}.
If \code{bestBy} is "performance", the best algorithm is the one with the best
overall performance across all instances in the cluster. If it is "count", the
best algorithm is the one that has the best performance most often. If it is
"successes", the best algorithm is the one with the highest number of successes
across all instances in the cluster. The learned model is used to cluster the
test data and predict algorithms accordingly.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.
The \code{parallelMap} level is "llama-fold".

If a list of clusterers is supplied in \code{clusterer}, ensemble
clustering is performed. That is, the models are trained and used to make
predictions independently. For each instance, the final prediction is determined
by majority vote of the predictions of the individual models -- the class that
occurs most often is chosen. If the list given as \code{clusterer} contains a
member \code{.combine} that is a function, it is assumed to be a classifier with
the same properties as classifiers given to \code{classify} and will be used to
combine the ensemble predictions instead of majority voting. This classifier is
passed the original features and the predictions of the classifiers in the
ensemble.

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
 cumulative performance value for the algorithm of the cluster the instance was
 assigned to. That is, if \code{bestBy} is "performance", it is the sum of the
 performance over all training instances. If \code{bestBy} is "count", the score
 corresponds to the number of training instances that the respective algorithm
 was the best on, and if it is "successes" it corresponds to the number of
 training instances where the algorithm was successful. If more than one
 clustering algorithm is used, the score corresponds to the sum of all instances
 across all clusterers. If stacking is used, the prediction is simply the best
 algorithm with a score of 1.}
 \item{predictor}{a function that encapsulates the model learned on the
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
\code{\link{classify}}, \code{\link{classifyPairs}}, \code{\link{regression}},
\code{\link{regressionPairs}}
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

res = cluster(clusterer=makeLearner("cluster.XMeans"), data=folds, pre=normalize)
# the total number of successes
sum(successes(folds, res))
# predictions on the entire data set
res$predictor(satsolvers$data[satsolvers$features])

# determine best by number of successes
res = cluster(clusterer=makeLearner("cluster.XMeans"), data=folds,
    bestBy="successes", pre=normalize)
sum(successes(folds, res))

# ensemble clustering
rese = cluster(clusterer=list(makeLearner("cluster.XMeans"),
    makeLearner("cluster.SimpleKMeans"), makeLearner("cluster.EM")),
    data=folds, pre=normalize)

# ensemble clustering with a classifier to combine predictions
rese = cluster(clusterer=list(makeLearner("cluster.XMeans"),
    makeLearner("cluster.SimpleKMeans"), makeLearner("cluster.EM"),
    .combine=makeLearner("classif.J48")), data=folds, pre=normalize)
}
}
\keyword{ cluster }
