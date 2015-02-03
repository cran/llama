\name{misclassificationPenalties}
\alias{misclassificationPenalties}
\title{
Misclassification penalty
}
\description{
Calculates the penalty incurred because of making incorrect decisions, i.e.
choosing suboptimal algorithms.
}
\usage{
misclassificationPenalties(data, model, addCosts = NULL)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{classifyPairs}, \code{cluster} or
      \code{regression}.}
  \item{model}{the algorithm selection model. Can be either a model
      returned by one of the model-building functions or a function that returns
      predictions such as \code{vbs} or the predictor function of a trained
      model.}
  \item{addCosts}{does nothing. Only here for compatibility with the other
      evaluation functions.}
}
\details{
Compares the performance of the respective chosen algorithm to the performance
of the best algorithm for each datum. Returns the absolute difference. This
denotes the penalty for choosing a suboptimal algorithm, e.g. the additional
time required to solve a problem or reduction in solution quality incurred. The
misclassification penalty of the virtual best is always zero.

If the model returns \code{NA} (e.g. because no algorithm solved the instance),
\code{0} is returned as misclassification penalty.

\code{data} may contain a train/test partition or not. This makes a difference
when computing the misclassification penalties for the single best algorithm.
If no train/test split is present, the single best algorithm is determined on
the entire data. If it is present, the single best algorithm is determined on
each test partition. That is, the single best is local to the partition and may
vary across partitions.
}
\value{
A list of the misclassification penalties.
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{parscores}}, \code{\link{successes}}
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)
folds = cvFolds(satsolvers)

model = classify(classifier=makeLearner("classif.J48"), data=folds)
sum(misclassificationPenalties(folds, model))
}
}
\keyword{ models }
