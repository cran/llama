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
    pre = function(x, y=NULL) { list(features=x) })
}
\arguments{
  \item{clusterer}{
  the clustering function to use. Must accept a data frame with features. Return
  value should be a structure that can be given to \code{predict} along with new
  data. See examples.

  The argument can also be a list of such clusters.
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
\code{cluster} takes \code{data} and processes it using \code{pre} (if
supplied). \code{clusterer} is called to cluster the data. For each cluster, the
best algorithm is identified as the one with the best overall performance on the
problems in that cluster. The learned model is used to cluster the test data and
predict algorithms accordingly.

The evaluation across the training and test sets will be parallelized
automatically if a suitable backend for parallel computation is loaded.

If a list of clusterers is supplied in \code{clusterer}, ensemble
clustering is performed. That is, the models are trained and used to make
predictions independently. For each instance, the final prediction is determined
by majority vote of the predictions of the individual models -- the class that
occurs most often is chosen. If the list given as \code{clusterer} contains a
member \code{.combine} that is a function, it is assumed to be a classifier with
the same properties as classifiers given to \code{classify} and will be
used to combine the ensemble predictions instead of majority voting.

}
\value{
 \item{predictions}{a list of lists with the predictions for each test set.}
 \item{predictor}{a function that encapsulates the model learned on the
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
\code{\link{classify}}, \code{\link{regression}}
}
\examples{
\dontrun{
library(RWeka)

data(satsolvers)
trainTest = cvFolds(satsolvers)

res = cluster(clusterer=XMeans, data=trainTest, pre=normalize)
# the total number of successes
sum(unlist(successes(trainTest, res$predictions)))
# predictions on the entire data set
res$predictor(subset(satsolvers$data, TRUE, satsolvers$features))

library(flexclust)
res = cluster(clusterer=function(x) { kcca(x, length(satsolvers$performance)) },
    data=trainTest, pre=normalize)

# ensemble clustering
rese = cluster(clusterer=list(XMeans, make_Weka_clusterer("weka/clusterers/EM"),
    function(x) { kcca(x, length(satsolvers$performance)) }),
    data=trainTest, pre=normalize)

# ensemble clustering with a classifier to combine predictions
rese = cluster(clusterer=list(XMeans, make_Weka_clusterer("weka/clusterers/EM"),
    function(x) { kcca(x, length(satsolvers$performance)) }, .combine=J48),
    data=trainTest, pre=normalize)
}
}
\keyword{ ~cluster }
