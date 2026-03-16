
#' @keywords internal
#' @import groupedHyperframe
#' 
#' @import cli
#' 
#' @import stats
#' 
'_PACKAGE'



# [~~Highlighted strike through test.~~]{style="background-color: #FFFF00"}
# [Highlighted test.]{style="background-color: #FFFF00"}



#' @importFrom utils citation
.onAttach <- function(libname, pkgname) {
  
  # .onAttach(libname = 'lib', pkgname = 'groupedHyperframe') # nah..
  
  'hyper.gam' |>
    citation() |>
    format(style = 'text') |> # utils:::format.citation
    col_green() |>
    style_bold() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}


