\name{analysis}
\alias{contributions}
\title{
Analysis functions
}
\description{
Functions for analysing portfolios.
}
\usage{
contributions(data = NULL)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
}
}
\details{
\code{contributions} analyses the marginal contributions of the algorithms in
the portfolio to its overall performance. More specifically, the Shapley value
for a specific algorithm is computed as the "value" of the portfolio with the
algorithm minus the "value" without the algorithm. This is done over all
possible portfolio compositions.

It is automatically determined whether the performance value is to be minimised
or maximised.
}
\value{
A table listing the Shapley values for each algorithm in the portfolio.
The higher the value, the more the respective algorithm contributes to the
overall performance of the portfolio.
}
\author{
Lars Kotthoff
}
\references{
Rahwan, T., Michalak, T. (2013)
A Game Theoretic Approach to Measure Contributions in Algorithm Portfolios.
\emph{Technical Report RR-13-11, University of Oxford}.
}
\examples{
if(Sys.getenv("RUN_EXPENSIVE") == "true") {
data(satsolvers)

contributions(satsolvers)
}
}
\keyword{ models }
