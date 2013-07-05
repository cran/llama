\name{misc}
\alias{vbs}
\alias{singleBest}
\title{
Convenience functions
}
\description{
Convenience functions for computing predictions based on input data.
}
\usage{
vbs(data = NULL)
singleBest(data = NULL)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
}
}
\details{
\code{vbs} and \code{singleBest} take a data frame of input data and return
predictions that correspond to the virtual best and the single best algorithm,
respectively. The virtual best picks the best algorithm for each instance. The
single best picks the algorithm that is the best most often over the entire data
set, even if for a particular instance it is not.
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
}
}
\keyword{ ~models }
