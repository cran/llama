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
misclassificationPenalties(data, model)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{classifyPairs}, \code{cluster} or
      \code{regression}.}
  \item{model}{the algorithm selection model. Can be either a model
      returned by one of the model-building functions or a function that returns
      predictions such as \code{vbs} or the predictor function of a trained
      model.}
}
\details{
Compares the performance of the respective chosen algorithm to the performance
of the best algorithm for each datum. Returns the absolute difference. This
denotes the penalty for choosing a suboptimal algorithm, e.g. the additional
time required to solve a problem or reduction in solution quality incurred. The
misclassification penalty of the virtual best is always zero.
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
\dontrun{
library(RWeka)

data(satsolvers)
trainTest = cvFolds(satsolvers)

model = classify(classifier=J48, data=trainTest)
sum(misclassificationPenalties(trainTest, model))
}
}
\keyword{ ~models }
