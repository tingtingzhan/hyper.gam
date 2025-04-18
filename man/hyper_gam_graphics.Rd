% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{hyper_gam_graphics}
\alias{hyper_gam_graphics}
\alias{persp.hyper_gam}
\alias{contour.hyper_gam}
\title{Visualize \link{hyper_gam} object using R package \pkg{graphics}}
\usage{
\method{persp}{hyper_gam}(
  x,
  n = 31L,
  xlab = "Percentages",
  ylab = "Quantiles",
  zlab = "Integrand of FR-index",
  ...
)

\method{contour}{hyper_gam}(
  x,
  n = 501L,
  image_col = topo.colors(20L),
  xlab = "Percentages",
  ylab = "Quantiles",
  ...
)
}
\arguments{
\item{x}{\link{hyper_gam} object}

\item{n}{\link[base]{integer} scalar, fineness of visualization,
default \code{501L}. See parameter \code{n.grid} of function \link[mgcv]{vis.gam}.}

\item{xlab, ylab}{\link[base]{character} scalars}

\item{zlab}{\link[base]{character} scalar, for function \link{persp.hyper_gam}}

\item{...}{additional parameters, currently not in use}

\item{image_col}{argument \code{col} of \link[graphics]{image.default}}
}
\value{
Function \code{\link[=persp.hyper_gam]{persp.hyper_gam()}},
a method dispatch of S3 generic \link[graphics]{persp},
does not have a return value.

Function \code{\link[=contour.hyper_gam]{contour.hyper_gam()}},
a method dispatch of S3 generic \link[graphics]{contour},
does not have a return value
}
\description{
Create \link[graphics]{persp}ective and \link[graphics]{contour}
plots of FR-index integrand using R package \pkg{graphics}.

End users are encouraged to use function \code{\link[=integrandSurface]{integrandSurface()}}
with \CRANpkg{plotly} work horse.
}
\keyword{internal}
