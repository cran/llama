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
misclassificationPenalties(data, predictions, minimize = T)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{cluster} or \code{regression}.}
  \item{predictions}{the predictions. Can be a list of lists as returned by
      \code{classify}, \code{cluster} or \code{regression} or a
      list as returned by the predictor return value of those functions.}
  \item{minimize}{is the minimum performance value the best or worst? Default T
      for best.}
}
\details{
Compares the performance of the respective chosen algorithm to the performance
of the best algorithm for each datum. Returns the absolute difference. This
denotes the penalty for choosing a suboptimal algorithm, e.g. the additional
time required to solve a problem or reduction in solution quality incurred.
}
\value{
A list (or list of lists, depending on the format of \code{predictions}) of the
misclassification penalties.
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

res = classify(classifier=J48, data=trainTest)
sum(unlist(misclassificationPenalties(trainTest, res$predictions)))

# maximum performance value is best
sum(unlist(misclassificationPenalties(trainTest, res$predictions, FALSE)))
}
}
\keyword{ ~models }
