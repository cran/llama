\name{imputeCensored}
\alias{imputeCensored}
\title{
Impute censored values
}
\description{
Impute the performance values that are censored, i.e. for which the respective
algorithm was not successful.
}
\usage{
imputeCensored(data = NULL, estimator = makeLearner("regr.lm"),
    epsilon = 0.1, maxit = 1000)
}
\arguments{
  \item{data}{the data to check for censored values to impute. The structure
  returned by \code{input}.}
  \item{estimator}{the mlr regressor to use to impute the censored values.}
  \item{epsilon}{the convergence criterion. Default 0.1.}
  \item{maxit}{the maximum number of iterations. Default 1000.}
}
\details{
The function checks for each algorithm if there are censored values by checking
for which problem instances the algorithm was not successful. It trains a model
to predict the performance value for those instances using the given estimator
based on the performance values of the instances where the algorithm was
successful and the problem features. It then uses the results of this initial
prediction to train a new model on the entire data and predict the performance
values for those problems where the algorithm was successful again. This process
is repeated until the maximum difference between predictions in two successive
iterations is less than \code{epsilon} or more than \code{maxit} iterations have
been performed.

It is up to the user to check whether the imputed values make sense. In
particular, for solver runtime data and timeouts one would expect that the
imputed values are above the timeout threshold, indicating at what time the
algorithms that have timed out would have solved the problem. No effort is made
to enforce such application-specific constraints.
}
\value{
The data structure with imputed censored values. The original data is saved in
the \code{original_data} member.
}
\author{
Lars Kotthoff
}
\references{
Josef Schmee and Gerald J. Hahn (1979)
A Simple Method for Regression Analysis with Censored Data.
\emph{Technometrics} 21, no. 4, 417-432.
}
\examples{
if(Sys.getenv("NOT_CRAN") == "true") {
data(satsolvers)
imputed = imputeCensored(satsolvers)
}
}
\keyword{ imputation }
