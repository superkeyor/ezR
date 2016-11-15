# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' @export
ez.show = function(...){
    # flush otherwise not print large text
    # show(...)
    print(Hmisc::describe(...))
    flush.console()
    cat('------------------------------\n')
    print(summary(...))
    flush.console()
    cat('------------------------------\n')
    str(...)
    flush.console()
    cat('------------------------------\n')
    ez.view(...)
}

#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' @export
ez.info = ez.show

#' show information about a data frame or similar object (like spss variable view)
#' @description wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param
#' @return
#' @examples
#' @export
ez.view = function(x, show.frq = T, show.prc = T, sort.by.name = F, ...){
    sjPlot::view_df(x, show.frq = show.frq, show.prc = show.prc, sort.by.name = sort.by.name, ...)
}
