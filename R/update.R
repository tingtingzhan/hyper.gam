
#' @importFrom stats update update.default
#' @export
update.hyper_gam <- function(object, ...) {
  # we do not have any ?mgcv:::update.*
  # 'gam' object is typically c("gam", "glm", "lm")
  # we do not have many ?stats:::update.*
  
  # `update(gam)` invokes ?stats::update.default, as of 2025-11-06 packageDate('mgcv')
  ret <- update.default(object, ...)
  # ret <- NextMethod(generic = 'update') # error
  
  class(ret) <- c('hyper_gam', class(ret))
  return(ret)
}
