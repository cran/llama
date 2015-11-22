\name{helpers}
\alias{print.llama.data}
\alias{print.llama.model}
\alias{makeRLearner.classif.constant}
\alias{predictLearner.classif.constant}
\alias{trainLearner.classif.constant}
\title{
Helpers
}
\usage{
\method{print}{llama.data}(x, \dots)
\method{print}{llama.model}(x, \dots)
\method{makeRLearner}{classif.constant}()
\method{predictLearner}{classif.constant}(.learner, .model, .newdata, \dots)
\method{trainLearner}{classif.constant}(.learner, .task, .subset, .weights, \dots)
}
\arguments{
  \item{x}{
  the object to print.
  }
  \item{.learner}{
  learner.
  }
  \item{.model}{
  model.
  }
  \item{.newdata}{
  new data.
  }
  \item{.task}{
  task.
  }
  \item{.subset}{
  subset.
  }
  \item{.weights}{
  weights.
  }
  \item{\dots}{
  ignored.
  }
}
\description{
S3 helper methods.
}
\author{
Lars Kotthoff
}
