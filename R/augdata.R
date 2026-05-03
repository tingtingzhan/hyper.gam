

#' @title Augment \link[spatstat.geom]{hyperframe} for \link[mgcv]{gam}
#' 
#' @param data \link[spatstat.geom]{hyperframe}
#' 
#' @param formula one-sided \link[stats]{formula}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @author 
#' Tingting Zhan, Erjia Cui
#' 
#' @name augdata
#' @export
augdata <- function(data, formula, ...) UseMethod(generic = 'augdata')

#' @rdname augdata
#' @export
augdata.data.frame <- function(data, formula, ...) return(data)

#' @rdname augdata
#' @export
augdata.hyperframe <- function(data, formula, ...) {
  
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  xnm <- formula[[2L]] # right-hand-side
  if (!is.symbol(xnm)) stop('Right-hand-side ', xnm |> deparse1() |> col_magenta(), ' must be a symbol')
  
  X <- data[[xnm]]
  if (!inherits(X, what = 'listof')) stop(xnm |> col_blue(), ' in `data` must be a hypercolumn')
  
  X. <- X |> 
    do.call(what = rbind)
  
  cnm <- X. |> 
    colnames()
  x. <- if (all(grepl(pattern = '%$', x = cnm))) {
    # returned from ?stats:::quantile.default
    tmp <- cnm |>
      gsub(pattern = '%$', replacement = '') |>
      as.double()
    tmp / 1e2
  } else cnm |> as.double() 
  
  if (!length(x.) || !is.numeric(x.) || anyNA(x.) || is.unsorted(x., strictly = TRUE)) {
    stop(xnm |> col_blue(), ' must have names convertible to strictly-increasing numerics')
  }
  
  colnames(X.) <- x.
  
  dat <- unclass(data)$df
  dat[[xnm]] <- X.
  #dat$x <- tcrossprod(rep(1, times = length(X)), x.)
  dat[[paste(xnm, 'x', sep = '.')]] <- tcrossprod(rep(1, times = length(X)), x.)
  
  nx <- length(x.)
  # for numeric integration of the functional term - Erjia's comment
  #dat$L <- array(1/nx, dim = dim(X.))
  dat[[paste(xnm, 'L', sep = '.')]] <- array(1/nx, dim = dim(X.))
  
  return(dat)
  
}
