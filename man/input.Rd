\name{input}
\alias{input}
\title{
Read data
}
\description{
Reads performance data that can be used to train and evaluate models.
}
\usage{
input(features, performances, successes = NULL, costs = NULL, extra = NULL,
    minimize = T)
}
\arguments{
  \item{features}{
  data frame that contains the feature values for each problem instance and a
  non-empty set of ID columns.
}
  \item{performances}{
  data frame that contains the performance values for each problem instance and
  a non-empty set of ID columns.
}
  \item{successes}{
  data frame that contains the success values (\code{TRUE}/\code{FALSE}) for
  each algorithm on each problem instance and a non-empty set of ID columns. The
  names of the columns in this data set should be the same as the names of the
  columns in \code{performances}. Optional.
}
  \item{costs}{
  either a single number, a data frame or a list that specifies
  the cost of the features. If a number is specified, it is assumed to denote
  the cost for all problem instances (i.e. the cost is always the same). If a
  data frame is given, it is assumed to have one column for each feature with
  the same name as the feature where each value gives the cost and a non-empty
  set of ID columns. If a list is specified, it is assumed to have a member
  \code{groups} that specifies which features belong to which group and a member
  \code{values} that is a data frame in the same format as before. Optional.
}
  \item{extra}{
  data frame containing any extra information about the instances and a
  non-empty set of ID columns. This is not used in modelling, but can be used
  e.g. for visualisation. Optional.
}
  \item{minimize}{
  whether the minimum performance value is best. Default true.
}
}
\details{
\code{input} takes a list of data frames and processes them as follows. The
feature and performance data are joined by looking for common column names in
the two data frames (usually an ID of the problem instance). For each problem,
the best algorithm according to the given performance data is computed. If more
than one algorithm has the best performance, all of them are returned.

The data frame that describes whether an algorithm was successful on a problem
is optional. If \code{parscores} or \code{successes} are to be used to
evaluate the learned models, this argument is required however and will lead to
error messages if not supplied.

Similarly, feature costs are optional.

If \code{successes} is given, it is used to determine the best algorithm on each
problem instance. That is, an algorithm can only be best if it was successful.
If no algorithm was successful, the value will be \code{NA}. Special care should
be taken when preparing the performance values for unsuccessful algorithms. For
example, if the performance measure is runtime and success is determined by
whether the algorithm was able to find a solution within a timeout, the
performance value for unsuccessful algorithms should be the timeout value. If
the algorithm failed because of some other reason in a short amount of time,
specifying this small amount of time may confuse some of the algorithm selection
model learners.
}
\value{
\item{data}{the combined data (features, performance, successes).}
\item{best}{a list of the best algorithms.}
\item{ids}{a list of names denoting the instance ID columns.}
\item{features}{a list of names denoting problem features.}
\item{performance}{a list of names denoting algorithm performances.}
\item{success}{a list of names denoting algorithm successes.}
\item{minimize}{true if the smaller performance values are better, else false.}
\item{cost}{a list of names denoting feature costs.}
\item{costGroups}{a list of list of names denoting which features belong to
    which group. Only returned if cost groups are given as input.}
}
\author{
Lars Kotthoff
}
\examples{
# features.csv looks something like
# ID,width,height
# 0,1.2,3
# ...
# performance.csv:
# ID,alg1,alg2
# 0,2,5
# ...
# success.csv:
# ID,alg1,alg2
# 0,T,F
# ...
#input(read.csv("features.csv"), read.csv("performance.csv"),
#    read.csv("success.csv"), costs=10)

# costs.csv:
# ID,width,height
# 0,3,4.5
# ...
#input(read.csv("features.csv"), read.csv("performance.csv"),
#    read.csv("success.csv"), costs=read.csv("costs.csv"))

# costGroups.csv:
# ID,group1,group2
# 0,3,4.5
# ...
#input(read.csv("features.csv"), read.csv("performance.csv"),
#    read.csv("success.csv"),
#    costs=list(groups=list(group1=c("height"), group2=c("width")),
#               values=read.csv("costGroups.csv")))
}
\keyword{ models }
