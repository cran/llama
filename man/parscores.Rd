\name{parscores}
\alias{parscores}
\title{
Penalized average runtime score
}
\description{
Calculates the penalized average runtime score which is commonly used for
evaluating satisfiability solvers on a set of problems.
}
\usage{
parscores(data, predictions, factor = 10)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{cluster} or \code{regression}.}
  \item{predictions}{the predictions. Can be a list of lists as returned by
      \code{classify}, \code{cluster} or \code{regression} or a
      list as returned by the predictor return value of those functions.}
  \item{factor}{the penalization factor to use for non-successful choices.
      Default 10.}
}
\details{
Returns the performances of the respective chosen algorithm on each datum. If
the chosen algorithm is not able to solve the problem successfully, its
performance value is multiplied by the penalization factor. That is, it is
assumed that the performance value is the timeout.
}
\value{
A list (or list of lists, depending on the format of \code{predictions}) of the
penalized average runtimes.
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{misclassificationPenalties}}, \code{\link{successes}}
}
\examples{
\dontrun{
library(RWeka)

data(satsolvers)
trainTest = cvFolds(satsolvers)

res = classify(classifier=J48, data=trainTest)
sum(unlist(parscores(trainTest, res$predictions)))

# use factor of 5 instead of 10.
sum(unlist(parscores(trainTest, res$predictions, 5)))
}
}
\keyword{ ~models }
