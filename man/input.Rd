\name{input}
\alias{input}
\title{
Read data
}
\description{
Reads performance data that can be used to train and evaluate models.
}
\usage{
input(features, performances, successes = NULL, minimize = T)
}
\arguments{
  \item{features}{
  data frame that contains the feature values for each problem.
}
  \item{performances}{
  data frame that contains the performance values for each problem.
}
  \item{successes}{
  data frame that contains the success values (true/false) for each
  problem. Optional.
}
  \item{minimize}{
  whether the minimum performance value is best. Default true.
}
}
\details{
\code{input} takes a list of data frames and processes them as follows. The
feature and performance data are joined by looking for common column names in
the two files (usually an ID of the problem). For each problem, the best
algorithm according to the read performance data is computed.

The data frame that describes whether an algorithm was successful on a problem
is optional. If \code{parscores} or \code{successes} are to be used to
evaluate the learned models, this argument is required however and will lead to
error messages if not supplied.
}
\value{
\item{data}{the combined data (features, performance, successes).}
\item{features}{a list of names denoting problem features.}
\item{performance}{a list of names denoting algorithm performances.}
\item{success}{a list of names denoting algorithm successes.}
}
\author{
Lars Kotthoff
}
\examples{
# features.csv looks something like
# ID,width,height
# 0,1.2,3
# ...
# performance.csv
# ID,alg1,alg2
# 0,2,5
# ...
# success.csv
# ID,alg1,alg2
# 0,T,F
# ...
\dontrun{input(read.csv("features.csv"), read.csv("performance.csv"),
    read.csv("success.csv"))}
}
\keyword{ ~models }
