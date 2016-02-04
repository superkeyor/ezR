# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' @export
z.show = function(...){
    # flush otherwise not print large text
    # show(...)
    flush.console()
    Hmisc::describe(...)
    flush.console()
    cat('------------------------------\n')
    flush.console()
    summary(...)
    flush.console()
    cat('------------------------------\n')
    flush.console()
    str(...)
    flush.console()
    cat('------------------------------\n')
    z.view(...)
}

#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' @export
z.info = z.show

#' show information about a data frame or similar object (like spss variable view)
#' @description wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param
#' @return
#' @examples
#' @export
z.view = function(x, showFreq = T, showPerc = T, sortByName = F, ...){
    sjPlot::view_df(x, showFreq = showFreq, showPerc = showPerc, sortByName = sortByName, ...)
}
