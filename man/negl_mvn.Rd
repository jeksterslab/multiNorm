% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiNorm-negl_mvn.R
\name{negl_mvn}
\alias{negl_mvn}
\title{Negative Log of the Likelihood of the Multivariate Normal Distribution}
\usage{
negl_mvn(theta, x)
}
\arguments{
\item{theta}{Numeric vector.
Parameter vector
\eqn{
    \boldsymbol{\theta}
    =
    \left\{
    \boldsymbol{\mu},
    \mathrm{vech}
    \left(
    \boldsymbol{\Sigma}
    \right)
    \right\}^{\prime}
}}

\item{x}{Numeric vector of length \code{k}.
The ith vector of observations.
Or numeric matrix of size \code{n} by \code{k}.}
}
\value{
A vector.
}
\description{
Calculates the negative log of the likelihood function
of the multivariate normal distribution.
}
\examples{
n <- 5
mu <- c(0, 0)
sigmacap <- matrix(
  data = c(
    1, 0.5, 0.5, 1
  ),
  nrow = 2
)

xcap <- as.data.frame(
  t(
    rmvn_chol(
      n = n,
      mu = mu,
      sigmacap = sigmacap
    )
  )
)

theta <- c(
  mu,
  linearAlgebra::vech(sigmacap)
)

lapply(
  X = xcap,
  FUN = negl_mvn,
  theta = theta
)
}
\seealso{
Other Multivariate Normal Distribution Functions: 
\code{\link{grad_l_mvn_generic}()},
\code{\link{grad_l_mvn}()},
\code{\link{hess_l_mvn_generic}()},
\code{\link{hess_l_mvn}()},
\code{\link{l_mvn_generic}()},
\code{\link{l_mvn}()},
\code{\link{mvn_theta_helper}()},
\code{\link{rmeans_mvn_chol_of_theta}()},
\code{\link{rmeans_mvn_chol}()},
\code{\link{rmvn_chol_of_rhocap}()},
\code{\link{rmvn_chol_of_theta}()},
\code{\link{rmvn_chol_of_vechsrhocap}()},
\code{\link{rmvn_chol}()},
\code{\link{rvcov_mvn_chol}()},
\code{\link{rvcov_wishart_of_vechsigmacap}()},
\code{\link{rvcov_wishart}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Multivariate Normal Distribution Functions}
\keyword{likelihood}
\keyword{multiNorm}
