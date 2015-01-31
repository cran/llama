\name{misc}
\alias{vbs}
\alias{singleBest}
\alias{singleBestByCount}
\alias{singleBestByPar}
\alias{singleBestBySuccesses}
\alias{predTable}
\title{
Convenience functions
}
\description{
Convenience functions for computing and working with predictions.
}
\usage{
vbs(data = NULL)
singleBest(data = NULL)
singleBestByCount(data = NULL)
singleBestByPar(data = NULL, factor = 10)
singleBestBySuccesses(data = NULL)
predTable(predictions = NULL, bestOnly = TRUE)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
  }
  \item{factor}{
  the penalization factor to use for non-successful choices. Default 10.
  }
  \item{predictions}{
  the list of predictions.
  }
  \item{bestOnly}{
  whether to tabulate only the respective best algorithm for each instance.
  Default \code{TRUE}.
  }
}
\details{
\code{vbs} and \code{singleBest} take a data frame of input data and return
predictions that correspond to the virtual best and the single best algorithm,
respectively. The virtual best picks the best algorithm for each instance. If no
algorithm solved in the instance, \code{NA} is returned. The single best picks
the algorithm that has the best cumulative performance over the entire data set.

\code{singleBestByCount} returns the algorithm that has the best performance
the highest number of times over the entire data set. Only whether or not an
algorithm is the best matters for this, not the difference to other algorithms.

\code{singleBestByPar} aggregates the PAR score over the entire data set and
returns the algorithm with the lowest overall PAR score.
\code{singleBestBySuccesses} counts the number of successes over the data set
and returns the algorithm with the highest overall number of successes.

\code{predTable} tabulates the predicted algorithms in the same way that
\code{table} does. If \code{bestOnly} is \code{FALSE}, all algorithms are
considered -- for example for regression models, predictions are made for all
algorithms, so the table will simply show the number of instances for each
algorithm. Set \code{bestOnly} to \code{TRUE} to tabulate only the best
algorithm for each instance.
}
\value{
A data frame with the predictions for each instance. The columns of the data
frame are the instance ID columns (as determined by \code{input}), the
algorithm, the score of the algorithm, and the iteration (always 1). The score
is 1 if the respective algorithm is chosen for the instance, 0 otherwise. More
than one prediction may be made for each instance and iteration.

For \code{predTable}, a table.
}
\author{
Lars Kotthoff
}
\examples{
if(Sys.getenv("NOT_CRAN") == "true") {
data(satsolvers)

# number of total successes for virtual best solver
print(sum(successes(satsolvers, vbs)))
# number of total successes for single best solver by count
print(sum(successes(satsolvers, singleBestByCount)))

# sum of PAR10 scores for single best solver by PAR10 score
print(sum(parscores(satsolvers, singleBestByPar)))

# number of total successes for single best solver by successes
print(sum(successes(satsolvers, singleBestBySuccesses)))

# print a table of the best solvers per instance
print(predTable(vbs(satsolvers)))
}
}
\keyword{ models }
