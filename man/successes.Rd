\name{successes}
\alias{successes}
\title{
Success
}
\description{
Was the problem solved successfully using the chosen algorithm?
}
\usage{
successes(data, model, timeout)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{classifyPairs}, \code{cluster} or
      \code{regression}.}
  \item{model}{the algorithm selection model. Can be either a model
      returned by one of the model-building functions or a function that returns
      predictions such as \code{vbs} or the predictor function of a trained
      model.}
  \item{timeout}{the timeout value to be multiplied by the penalization factor.
      If not specified, the maximum performance value of all algorithms on the
      entire data is used.}
}
\details{
Returns TRUE if the chosen algorithm successfully solved the problem instance,
FALSE otherwise for each problem instance.

If feature costs have been given, the cost of the used features or feature
groups is added to the performance of the chosen algorithm. The used features
are determined by examining the the \code{features} member of \code{data}, not
the model. If after that the performance value is above the timeout value,
\code{FALSE} is assumed. If whether an algorithm was successful is not
determined by performance and feature costs, don't pass costs when creating the
LLAMA data frame.
}
\value{
A list of the success values.
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{misclassificationPenalties}}, \code{\link{parscores}}
}
\examples{
\dontrun{
library(RWeka)

data(satsolvers)
trainTest = cvFolds(satsolvers)

model = classify(classifier=J48, data=trainTest)
sum(successes(trainTest, model))
}
}
\keyword{ ~models }
