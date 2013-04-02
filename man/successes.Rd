\name{successes}
\alias{successes}
\title{
Success
}
\description{
Was the problem solved successfully using the chosen algorithm?
}
\usage{
successes(data, predictions)
}
\arguments{
  \item{data}{the data used to induce the model. The same as given to
      \code{classify}, \code{cluster} or \code{regression}.}
  \item{predictions}{the predictions. Can be a list of lists as returned by
      \code{classify}, \code{cluster} or \code{regression} or a
      list as returned by the predictor return value of those functions.}
}
\details{
Returns TRUE if the chosen algorithm successfully solved the problem, FALSE
otherwise for each datum.
}
\value{
A list (or list of lists, depending on the format of \code{predictions}) of the
success values.
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

res = classify(classifier=J48, data=trainTest)
sum(unlist(successes(trainTest, res$predictions)))
}
}
\keyword{ ~models }
