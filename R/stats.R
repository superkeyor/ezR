# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' @export
z.show = function(...){
    # show(...)
    z.view(...)
    cat('------------------------------\n')
    str(...)
    cat('------------------------------\n')
    summary(...)
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
