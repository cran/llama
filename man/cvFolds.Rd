\name{cvFolds}
\alias{cvFolds}
\title{
Cross-validation folds
}
\description{
Take data produced by \code{input} and amend it with (optionally)
stratified folds for cross-validation.
}
\usage{
cvFolds(data, nfolds = 10, stratify = T)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
}
  \item{nfolds}{
  the number of folds. Defaults to 10. If -1 is given, leave-one-out
  cross-validation folds are produced.
}
  \item{stratify}{
  whether to stratify the folds. Defaults to TRUE.
}
}
\details{
Partitions the data set into folds. Stratification, if requested, is done by the
best algorithm, i.e. the one with the best performance. The distribution of the
best algorithms in each fold will be approximately the same. The folds are
assembled into training and test sets by combining $n-1$ folds for training and
using the remaining fold for testing. The sets are added to the original data
set and returned.
}
\value{
\item{train}{a list of data sets for training.}
\item{test}{a list of data sets for testing.}
\item{...}{the original members of \code{data}. See \code{\link{input}}.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{trainTest}}
}
\examples{
data(satsolvers)
folds = cvFolds(satsolvers)

# use 5 folds instead of the default 10
folds5 = cvFolds(satsolvers, 5)

# don't stratify
foldsU = cvFolds(satsolvers, stratify=FALSE)
}
\keyword{ ~models }
