% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/userPredictions.R
\name{predict.RFEAT}
\alias{predict.RFEAT}
\title{Model prediction for Random Forest + Efficiency Analysis Trees model.}
\usage{
\method{predict}{RFEAT}(object, newdata, x, ...)
}
\arguments{
\item{object}{A \code{RFEAT} object.}

\item{newdata}{\code{data.frame}. Set of input variables to predict on.}

\item{x}{Inputs index.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
\code{data.frame} with the original data and the predicted values.
}
\description{
This function predicts the expected output by a \code{RFEAT} object.
}
\examples{
\donttest{
simulated <- X2Y2.sim(N = 50, border = 0.2)
RFEAT_model <- RFEAT(data = simulated, x = c(1, 2), y = c(3, 4))

predict(object = RFEAT_model, newdata = simulated, x = c(1, 2))
}
}
