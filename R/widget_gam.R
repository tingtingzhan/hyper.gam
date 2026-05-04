
# read ?mgcv::vis.gam carefully
# why ?mgcv::vis.gam does not require (an equivalent of) the parameter `formula`


#' @title Integrand Surface(s) of \link[mgcv]{gam} Model(s)
#' 
#' @description
#' An interactive \CRANpkg{htmlwidgets} of the 
#' \link[graphics]{persp}ective plot for 
#' \link[mgcv]{gam} model(s)
#' using package \CRANpkg{plotly}.
#' 
#' @param ... one or more \link[mgcv]{gam} models
#' based on *a same data set*.
#' 
#' @param formula one-sided \link[stats]{formula}
#' 
#' @param newdata (for future expansion)
#' 
#' @param proj_xy \link[base]{logical} scalar, whether to show 
#' the projection of \eqn{\hat{S}\big(p, Q_i(p)\big)}
#' (see sections **Details** and **Value**)
#' to the \eqn{(p,q)}-plain, default `TRUE`
#' 
#' @param proj_xz \link[base]{logical} scalar, whether to show
#' the projection of \eqn{\hat{S}\big(p, Q_i(p)\big)} to the \eqn{(p,s)}-plain, default `TRUE`
#' 
#' @param proj_beta \link[base]{logical} scalar, whether to show
#' \eqn{\hat{\beta}(p)} on the \eqn{(p,s)}-plain when applicable, default `TRUE`
#' 
#' @param n \link[base]{integer} scalar, fineness of visualization,
#' default `501L`. See parameter `n.grid` of function \link[mgcv]{vis.gam}.
#' 
#' @param newid \link[base]{integer} scalar or \link[base]{vector},
#' row indices of `newdata` to be visualized. 
#' Default `1:2`, i.e., the first two test subjects.
#' Use `newid = NULL` to disable visualization of `newdata`.
#' 
#' @param qlim \link[base]{length}-2 \link[base]{double} \link[base]{vector},
#' range on \eqn{q}-axis. Default is the range of \eqn{X} and \eqn{X^{\text{new}}} combined.
#' 
# @param axis_col \link[base]{length}-3 \link[base]{character} \link[base]{vector},
# colors of the \eqn{(p,q,s)} axes
#' 
# @param axis_title description \link[base]{length}-3 \link[base]{character} \link[base]{vector},
# title of the \eqn{(p,q,s)} axes
#' 
#' @param beta_col \link[base]{character} scalar, color 
#' of \eqn{\hat{\beta(p)}}
#' 
#' @param surface_col \link[base]{length}-2 \link[base]{character} \link[base]{vector},
#' color of the integrand surface(s), for lowest and highest surface values
#' 
#' @section Integrand Surface:
#' 
# The quantile index (QI), 
# \deqn{\text{QI}=\displaystyle\int_0^1\beta(p)\cdot Q(p)\,dp}
# with a linear functional coefficient \eqn{\beta(p)}
# can be estimated by fitting a functional generalized linear model (FGLM, James, 2002) to exponential-family outcomes, 
# or by fitting a linear functional Cox model (LFCM, Gellar et al., 2015) to survival outcomes. 
# More flexible non-linear quantile index (nlQI)
# \deqn{
# \text{nlQI}=\displaystyle\int_0^1 F\big(p, Q(p)\big)\,dp
# }
# with a bivariate twice differentiable function \eqn{F(\cdot,\cdot)}
# can be estimated by fitting a functional generalized additive model (FGAM, McLean et al., 2014) to exponential-family outcomes, 
# or by fitting an additive functional Cox model (AFCM, Cui et al., 2021) to survival outcomes. 
#' 
#' The estimated **integrand surface** of quantile indices and non-linear quantile indices, defined on 
#' \eqn{p\in[0,1]} and 
#' \eqn{q\in\text{range}\big(Q_i(p)\big)} for all training subjects \eqn{i=1,\cdots,n}, 
#' is
#' \deqn{
#' \hat{S}_0(p,q) = 
#' \begin{cases}
#' \hat{\beta}(p)\cdot q & \text{for QI}\\
#' \hat{F}(p,q) & \text{for nlQI}
#' \end{cases}
#' }
#' 
#' @returns 
#' The function [widget_gam()] returns a pretty \CRANpkg{htmlwidgets} created by **R** package \CRANpkg{plotly}
#' to showcase the \link[graphics]{persp}ective plot of the
#' estimated integrand surface \eqn{\hat{S}(p,q)}.
#' 
#' If a set of training/test subjects is selected (via parameter `newid`), then 
#' \itemize{
#' \item {the estimated **line integrand curve** \eqn{\hat{S}\big(p, Q_i(p)\big)} 
#' of subject \eqn{i} 
#' is displayed on the surface \eqn{\hat{S}(p,q)};}
#' \item {the quantile curve \eqn{Q_i(p)} 
#' is projected on the \eqn{(p,q)}-plain of the 3-dimensional \eqn{(p,q,s)} cube, 
#' if `proj_xy=TRUE` (default);}
#' \item {the user-specified \eqn{\tilde{p}} is marked on the \eqn{(p,q)}-plain of the 3D cube, 
#' if `proj_xy=TRUE` (default);}
#' \item {\eqn{\hat{S}\big(p, Q_i(p)\big)}
#' is projected on the \eqn{(p,s)}-plain of the 3-dimensional \eqn{(p,q,s)} cube, 
#' if one and only one \link[mgcv]{gam} model is provided in in
#' put argument `...` and `proj_xz=TRUE` (default);}
#' \item {the estimated *linear functional coefficient* \eqn{\hat{\beta}(p)} is shown on the \eqn{(p,s)}-plain of the 3D cube, 
#' if one and only one *linear* \link[mgcv]{gam} model is provided in input argument `...` and `proj_beta=TRUE` (default).}
#' }
#' 
#' @note
#' The maintainer is not aware of any functionality of projection of arbitrary curves in package \CRANpkg{plotly}.
#' Currently, the projection to \eqn{(p,q)}-plain is hard coded on \eqn{(p,q,s=\text{min}(s))}-plain.
#' 
#' @keywords internal
#' @importFrom mgcv predict.gam
#' @importFrom plotly plot_ly add_paths add_surface
#' @importFrom stats setNames
#' @export
widget_gam <- function(
    ...,
    formula, 
    newdata = data,
    proj_xy = TRUE, 
    proj_xz = TRUE,
    proj_beta = FALSE, # bug with my latest hyperframe !!!
    n = 501L,
    newid = min(3L, nrow(newdata)) |> seq_len(), 
    qlim = range(X[is.finite(X)], newX[is.finite(newX)]), # removing NA, NaN, Inf
    #axis_col = c('dodgerblue', 'deeppink', 'darkolivegreen'),
    #axis_title = c('Probability (p)', 'Quantile (q)', 'Integrand (s)'),
    beta_col = 'purple',
    surface_col = 
      # c('lightyellow', 'lightpink') # nice
      # c('beige', 'lightpink') # nice
      # c('white', 'deeppink') # not good!
      # c('white', 'magenta') # not good!
      c('white', 'lightgreen') # nice
    # c('white', 'darkgreen') # not good!
    # c('white', 'lightgoldenrod') # my R do not recognize
    # c('white', 'lightslateblue') # my R do not recognize
    # c('white', 'yellow') # nice
) {
  
  dots <- list(...)
  if (!all(vapply(dots, FUN = inherits, what = 'gam', FUN.VALUE = NA))) stop('all input needs to be `gam.matrix`')
  
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  xnm <- formula[[2L]] # right-hand-side
  if (!is.symbol(xnm)) stop('Right-hand-side ', xnm |> deparse1() |> col_magenta(), ' must be a symbol')
  
  data_ <- dots |> lapply(FUN = \(i) i$data) |> unique()
  if (length(data_) > 1L) stop('data not same')
  data <- data_[[1L]]
  
  X <- data[[xnm]]
  x. <- as.double(colnames(X))
  nx <- length(x.)
  
  newX <- newdata[[xnm]]
  if (!is.matrix(newX)) stop('`newdata` does not contain a matrix column of functional predictor values')
  newx. <- newX |> colnames() |> as.double()
  if (!all.equal.numeric(newx., x.)) stop('grid of training and test data must be exactly the same')
  
  #l <- unique.default(data$L)
  l <- unique.default(data[[paste(xnm, 'L', sep = '.')]])
  if (length(l) != 1L) stop('wont happen')
  
  # plot!!
  # *surface* based on training model
  x_ <- seq.int(from = min(x.), to = max(x.), length.out = n)
  y_ <- seq.int(from = qlim[1L], to = qlim[2L], length.out = n)
  d_xy <- data.frame(
    expand.grid(x = x_, y_), # span `x_` first, then span `y_`
    L = l
  ) |> 
    setNames(nm = c(
      paste(xnm, 'x', sep = '.'),
      as.character(xnm),
      paste(xnm, 'L', sep = '.')
    ))
  
  zs <- mapply(FUN = \(x) { # (x = dots[[1L]])
    y0 <- predict.gam(x, newdata = d_xy, se.fit = FALSE, type = 'link')
    dim(y0) <- c(n, n)
    t.default(y0) # important!!!
    # plot_ly(, type = 'surface') lay out `z` differently from ?graphics::persp !!!
  }, x = dots, SIMPLIFY = FALSE)
  
  zmin <- zs |> unlist() |> min()
  zmax <- zs |> unlist() |> max()
  
  #p <- plot_ly(x = x_, y = y_)
  p <- plot_ly()
  
  for (z_ in zs) {
    p <- p |> 
      add_surface(
        x = x_, y = y_,
        z = z_, cmin = zmin, cmax = zmax, 
        contours = list(
          z = list(
            show = TRUE,
            start = zmin, end = zmax, size = (zmax - zmin)/21,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = TRUE)
          )
        ),
        colorscale = list(c(0, 1), surface_col), 
        showscale = FALSE
      )
  }
  
  #p <- p |> 
  #layout(scene = list(
  #  xaxis = list(title = axis_title[1L], tickformat = '.0%', color = axis_col[1L]), 
  #  yaxis = list(title = axis_title[2L], color = axis_col[2L]),
  #  zaxis = list(title = axis_title[3L], color = axis_col[3L])
  #))
  
  if (!length(newid)) return(p)
  
  if (!is.integer(newid) || anyNA(newid) || any(newid > nrow(newX))) stop('illegal `newid`')
  
  d <- data.frame(
    x = x.,
    y = newX[newid, , drop = FALSE] |> t.default() |> c(),
    id = rep(newid, each = nx),
    L = l
  )
  if (proj_xy) {
    p <- p |> 
      add_paths(
        x = d$x, y = d$y, z = zmin, name = d$id, color = d$id, 
        showlegend = FALSE,
        line = list(width = 4)
      )
  } # projection on x-y plain
  
  d_ <- d |>
    setNames(nm = c(
      paste(xnm, 'x', sep = '.'),
      as.character(xnm),
      'id',
      paste(xnm, 'L', sep = '.')
    ))
  z_subj <- mapply(FUN = \(x) {
    predict.gam(x, newdata = d_, se.fit = FALSE, type = 'link')
  }, x = dots, SIMPLIFY = FALSE)
  
  if (proj_xz) {
    # projection on x-z plain, F(p, Q(p)) curve
    # this is only done if (length(dots) == 1L); otherwise too messy
    if (length(dots) == 1L) {
      for (i in seq_along(dots)) {
        p <- p |> 
          add_paths(
            x = d$x, y = qlim[2L], z = z_subj[[i]], name = d$id, color = d$id,
            showlegend = FALSE,
            line = list(width = 4)
          )
      }
    }
  } # projection on x-z plain
  
  if (proj_beta) {
    if (length(dots) == 1L) { # will remove this bracket in future!!
      for (i in seq_along(dots)) {
        if (dots[[i]]$formula[[3L]][[1L]] == 's') {
          d_beta <- data.frame(
            x = x.,
            y = 1,
            L = l
          ) |>
            setNames(nm = c(
              paste(xnm, 'x', sep = '.'),
              as.character(xnm),
              paste(xnm, 'L', sep = '.')
            ))
          z_beta <- mapply(FUN = \(x) {
            predict.gam(x, newdata = d_beta, se.fit = FALSE, type = 'link')
          }, x = dots, SIMPLIFY = FALSE)
          p <- add_paths(
            p = p, data = d_beta, 
            x = ~ x, y = qlim[2L], z = z_beta[[i]], 
            showlegend = FALSE,
            line = list(
              color = beta_col,
              width = 4
            ))  
        }
      }
    }
  } # projection of beta(p), for linear models
  
  for (i in seq_along(dots)) {
    p <- p |> 
      add_paths(
        x = d$x, y = d$y, z = z_subj[[i]], name = d$id, color = d$id,
        showlegend = FALSE,
        line = list(width = 4)
      )
  }
  
  return(p)
  
}











if (FALSE) { # learn projection with plotly
  
    
  # python
  # https://plotly.com/python/3d-line-plots/ # no mention of projection
  # https://community.plotly.com/t/how-to-plot-a-2d-graph-on-the-background-side-wall-of-a-3d-plot/72874/5
  
  # R
  # https://plotly.com/r/3d-line-plots/ # no mention of projection
  # https://stackoverflow.com/questions/53182432/3d-surface-with-a-2d-projection-using-r
  
  plot_ly(z = ~volcano) |> 
    add_surface(
      contours = list(
        z = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "#ff0000",
          project = list(z=TRUE)
        ),
        y = list(
          show = TRUE,
          usecolormap = FALSE, # Projection without colormap
          highlightcolor = "#ff0000",
          project = list(y=TRUE)
        ),
        x = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "#ff0000",
          project = list(x=TRUE)
        )
      )
    )
  
  # ?plotly::add_trace
  # explanation for parameter `...`
  # Arguments (i.e., attributes) passed along to the trace type. See schema() for a list of acceptable attributes for a given trace type (by going to traces -> type -> attributes).
  
  # ?plotly::schema
}
