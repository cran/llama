\name{misc}
\alias{vbs}
\alias{singleBest}
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
single best picks the algorithm that is the best most often over the entire data
set, even if for a particular instance it is not, where best is defined as
having the best performance metric.

\code{singleBestByPar} and \code{singleBestBySuccesses} aggregate the PAR score
and number of successes, respectively, over the \emph{entire} data set. The
single algorithm with the lowest PAR score and the highest number of successes
on the data is chosen.
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
print(sum(successes(satsolvers, vbs(satsolvers))))
# number of total successes for single best solver
print(sum(successes(satsolvers, singleBest(satsolvers))))

# sum of PAR10 scores for single best solver by PAR10 score
print(sum(parscores(satsolvers, singleBestByPar(satsolvers))))

# number of total successes for single best solver by successes
print(sum(successes(satsolvers, singleBestBySuccesses(satsolvers))))
}
}
\keyword{ ~models }
