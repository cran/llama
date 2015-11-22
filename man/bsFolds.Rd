\name{bsFolds}
\alias{bsFolds}
\title{
Bootstrapping folds
}
\description{
Take data produced by \code{input} and amend it with (optionally)
stratified folds determined through bootstrapping.
}
\usage{
bsFolds(data, nfolds = 10L, stratify = FALSE)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
}
  \item{nfolds}{
  the number of folds. Defaults to 10.
}
  \item{stratify}{
  whether to stratify the folds. Makes really only sense for classification
  models. Defaults to \code{FALSE}.
}
}
\details{
Partitions the data set into folds. Stratification, if requested, is done by the
best algorithm, i.e. the one with the best performance. The distribution of the
best algorithms in each fold will be approximately the same. For each fold, the
training index set is assembled through .632 bootstrap. The remaining indices
are used for testing. There is no guarantee on the sizes of either sets. The
sets of indices are added to the original data set and returned.

If the data set has train and test partitions already, they are overwritten.
}
\value{
\item{train}{a list of index sets for training.}
\item{test}{a list of index sets for testing.}
\item{\dots}{the original members of \code{data}. See \code{\link{input}}.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{cvFolds}}, \code{\link{trainTest}}
}
\examples{
data(satsolvers)
folds = bsFolds(satsolvers)

# use 5 folds instead of the default 10
folds5 = bsFolds(satsolvers, 5L)

# stratify
foldsU = bsFolds(satsolvers, stratify=TRUE)
}
\keyword{ models }
