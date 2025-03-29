

#' @title Predict via Cross Validation
#' 
#' @param object a [hyper_gam] object
#' 
#' @param k \link[base]{integer} scalar
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [predict_k_fold.hyper_gam()] returns a \link[base]{list}.
#' 
#' @keywords internal
#' @importFrom caret createFolds
#' @importFrom parallel mclapply detectCores
#' @export
predict_k_fold.hyper_gam <- function(
    object, 
    k, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) { 
  
  if (!inherits(object, what = 'hyper_gam')) stop('input must be `hyper_gam`')
  
  data <- getData.gam(object)

  # ?caret::createFolds depends on ?base::set.seed
  nr <- .row_names_info(data, type = 2L)
  fld <- createFolds(y = seq_len(nr), k = k, list = TRUE, returnTrain = FALSE)
  
  ret <- list(
    fold = rep(NA_integer_, times = nr),
    est = rep(NA_real_, times = nr),
    est.k = rep(NA_real_, times = nr),
    est.global = rep(NA_real_, times = nr),
    signadj = rep(NA_integer_, times = k)
  )
  
  tmp <- mclapply(X = fld, mc.cores = mc.cores, FUN = function(id) {
  #tmp <- lapply(X = fld, FUN = function(id) {
    d0 <- data[-id, , drop = FALSE] # training set
    d1 <- data[id, , drop = FALSE] # test set
    m <- tryCatch(update.hyper_gam(object, data = d0), error = identity) # training model
    if (inherits(m, what = 'error')) return(invisible())
    signadj <- cor_xy.gam(m) |> sign()
    # predicted value on test set
    est <- predict.hyper_gam(m, newdata = d1, sgn = 1)
    est.k <- predict.hyper_gam(m, newdata = d1, sgn = signadj)
    return(list(signadj = signadj, est = est, est.k = est.k))
  })
  
  for (i in seq_along(fld)) {
    id <- fld[[i]]
    ret$fold[id] <- i
    ret$signadj[i] <- tmp[[i]]$signadj
    ret$est[id] <- tmp[[i]]$est
    ret$est.k[id] <- tmp[[i]]$est.k
  }
  
  ret$est.global <- ret$est * sign(cor_xy.gam(object))
  
  return(ret)
  
}

# https://www.geeksforgeeks.org/k-fold-cross-validation-in-r-programming/



