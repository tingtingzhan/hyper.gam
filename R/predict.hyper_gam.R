

#' @title Prediction of [hyper_gam] Model
#' 
#' @description
#' Prediction of [hyper_gam] model.
#' 
#' @param object an [hyper_gam] model
#' 
#' @param newdata test \link[spatstat.geom]{hyperframe}, with at least 
#' the response \eqn{y^{\text{new}}} and
#' the \link[base]{double}-hypercolumn \eqn{X^{\text{new}}}
#' tabulated on the same grid as the training hypercolumn \eqn{X}.
#' If missing, the training data `object$data` will be used.
#' 
#' @param ... additional parameters, currently not in use.
#' 
#' @returns 
#' The `S3` method [predict.hyper_gam()] returns a 
#' \link[base]{double} \link[base]{vector}.
#' 
#' @keywords internal
#' @importFrom mgcv predict.gam
#' @export
predict.hyper_gam <- function(
    object, 
    newdata = object$data,
    ...
) {
  
  xname <- attr(object, which = 'xname', exact = TRUE)
  newdata <- augdata(data = newdata, xname = xname)
  
  # do we really need to check the `$x` and `$L` of `newdata` and `olddata` being the same???
  # from tzh's previous code, we do need to check '$L' are the same
  newL <- newdata$L
  oldL <- object$data$L
  if (length(newl <- unique.default(newL)) != 1L) stop()
  oldl <- unique.default(oldL)
  if (!all.equal.numeric(newl, oldl)) stop()
  # what about `$x` ?

  predict.gam(object = object, newdata = newdata) |> # mgcv::predict.gam returns 'array'
    as.double() # ?base::as.double much faster than ?base::c
    
}






