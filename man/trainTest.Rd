\name{trainTest}
\alias{trainTest}
\title{
Train / test split
}
\description{
Split a data set into train and test set.
}
\usage{
trainTest(data, trainpart = 0.6, stratify = FALSE)
}
\arguments{
  \item{data}{
  the data to use. The structure returned by \code{input}.
}
  \item{trainpart}{
  the fraction of the data to use for training. Default 0.6.
}
  \item{stratify}{
  whether to stratify the folds. Makes really only sense for classification
  models. Defaults to \code{FALSE}.
}
}
\details{
Partitions the data set into training and test set according to the specified
fraction. The training and test index sets are added to the original data and
returned. If requested, the distribution of the best algorithms in training and
test set is approximately the same, i.e. the sets are stratified.

If the data set has train and test partitions already, they are overwritten.
}
\value{
\item{train}{a (one-element) list of index sets for training.}
\item{test}{a (one-element) list of index sets for testing.}
\item{\dots}{the original members of \code{data}. See \code{\link{input}}.}
}
\author{
Lars Kotthoff
}
\seealso{
\code{\link{bsFolds}}, \code{\link{cvFolds}}
}
\examples{
data(satsolvers)
trainTest = trainTest(satsolvers)

# use 50-50 split instead of 60-40
trainTest1 = trainTest(satsolvers, 0.5)

# stratify
trainTestU = trainTest(satsolvers, stratify=TRUE)
}
\keyword{ models }
