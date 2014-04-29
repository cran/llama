\name{satsolvers}
\alias{satsolvers}
\docType{data}
\title{
Example data for Leveraging Learning to Automatically Manage Algorithms
}
\description{
Performance data for 19 SAT solvers on 2433 SAT instances.
}
\usage{data(satsolvers)}
\format{
\code{satsolvers} is a list in the format returned by \code{input} and
expected by the other functions of LLAMA. The list has the following components.

\describe{
 \item{data:}{The original input data merged. That is, the data frames processed
 by \code{input} in a single data frame with the following additional
 columns.
    \describe{
     \item{best:}{The algorithm(s) with the best performance for each row.}
     \item{*_success:}{For each algorithm whether it was successful on the
     respective row.}
    }}
 \item{features:}{The names of the columns that contain feature values.}
 \item{performance:}{The names of the columns that contain performance data.}
 \item{success:}{The names of the columns indicating whether an algorithm was
 successful.}
 \item{minimize:}{Whether the performance is to be minimized.}
 \item{cost:}{The names of the columns that contain the feature group
 computation cost for each instance.}
 \item{costGroups:}{A list the maps the names of the feature groups to the list
 of feature names that are contained in it.}
}
}
\details{
Performance data for 19 SAT solvers on 2433 SAT instances. For each instance, 36
features were measured. In addition to the performance (time) on each instance,
data on whether a solver timed out on an instance is included. The cost to
compute all features is included as well.
}
\source{
Hurley, B., Kotthoff, L., Malitsky, Y., O'Sullivan, B. (2014)
Proteus: A Hierarchical Portfolio of Solvers and Transformations.
\emph{Eleventh International Conference on Integration of Artificial Intelligence (AI) and Operations Research (OR) techniques in Constraint Programming}.
}
\seealso{
\code{\link{input}}
}
\examples{
data(satsolvers)
}
\keyword{datasets}
