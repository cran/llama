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

This function is currently only implemented for cases where the objective is to
minimise the performance value, which is the case for runtime for example. The
function will still work in other cases, but the reported values will be
incorrect.
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
\dontrun{
data(satsolvers)

contributions(satsolvers)
}
}
\keyword{ ~models }
