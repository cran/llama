\name{misc}
\alias{vbs}
\alias{singleBest}
\alias{singleBestByCount}
\alias{singleBestByPar}
\alias{singleBestBySuccesses}
\title{
Convenience functions
}
\description{
Convenience functions for computing predictions based on input data.
}
\usage{
vbs(data = NULL)
singleBest(data = NULL)
singleBestByCount(data = NULL)
singleBestByPar(data = NULL, factor = 10)
singleBestBySuccesses(data = NULL)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
  }
  \item{factor}{
  the penalization factor to use for non-successful choices. Default 10.
  }
}
\details{
\code{vbs} and \code{singleBest} take a data frame of input data and return
predictions that correspond to the virtual best and the single best algorithm,
respectively. The virtual best picks the best algorithm for each instance. The
single best picks the algorithm that has the best cumulative performance over
the entire data set.

\code{singleBestByCount} returns the algorithm that has the best performance
the highest number of times over the entire data set. Only whether or not an
algorithm is the best matters for this, not the difference to other algorithms.

\code{singleBestByPar} aggregates the PAR score over the entire data set and
returns the algorithm with the lowest overall PAR score.
\code{singleBestBySuccesses} counts the number of successes over the data set
and returns the algorithm with the highest overall number of successes.
}
\value{
A list of predictions, that is, data frames with columns \code{algorithm} and
\code{score}. The data frames are sorted such that the most preferred algorithm
is first. The score is 1 if the respective algorithm is chosen for the instance,
0 otherwise.
}
\author{
Lars Kotthoff
}
\examples{
\dontrun{
data(satsolvers)

# number of total successes for virtual best solver
print(sum(successes(satsolvers, vbs)))
# number of total successes for single best solver by count
print(sum(successes(satsolvers, singleBestByCount)))

# sum of PAR10 scores for single best solver by PAR10 score
print(sum(parscores(satsolvers, singleBestByPar)))

# number of total successes for single best solver by successes
print(sum(successes(satsolvers, singleBestBySuccesses)))
}
}
\keyword{ ~models }
