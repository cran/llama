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
parscores(data, model, factor = 10, timeout)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{classifyPairs}, \code{cluster} or
      \code{regression}.}
  \item{model}{the algorithm selection model. Can be either a model
      returned by one of the model-building functions or a function that returns
      predictions such as \code{vbs} or the predictor function of a trained
      model.}
  \item{factor}{the penalization factor to use for non-successful choices.
      Default 10.}
  \item{timeout}{the timeout value to be multiplied by the penalization factor.
      If not specified, the maximum performance value of all algorithms on the
      entire data is used.}
}
\details{
Returns the penalized average runtime performances of the respective chosen
algorithm on each problem instance.

If feature costs have been given, the cost of the used features or feature
groups is added to the performance of the chosen algorithm. The used features
are determined by examining the the \code{features} member of \code{data}, not
the model. If after that the performance value is above the timeout value, the
timeout value multiplied by the factor is assumed.
}
\value{
A list of the penalized average runtimes.
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

model = classify(classifier=J48, data=trainTest)
sum(parscores(trainTest, model))

# use factor of 5 instead of 10.
sum(parscores(trainTest, model, 5))

# explicitly specify timeout.
sum(parscores(trainTest, model, timeout = 3600))
}
}
\keyword{ ~models }
