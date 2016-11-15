# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object, alias of \code{\link{ez.info}}
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @return
#' @examples
#' @seealso \code{\link{ez.view}}
#' @export
ez.show = function(x){
    if (!is.data.frame(x)) {(x = data.frame(x))}
    # flush otherwise not print large text
    # show(x)
    print(Hmisc::describe(x))
    flush.console()
    print(summary(x))
    flush.console()
    cat('--------------------------------------------------------------------------------------------\n')
    str(x)
    flush.console()
    cat('--------------------------------------------------------------------------------------------\n')
    ez.view(x)
}

#' show information about a data frame or other object, alias of \code{\link{ez.show}}
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @return
#' @examples
#' @seealso \code{\link{ez.view}}
#' @export
ez.info = ez.show

#' show information about a data frame or similar object (like spss variable view)
#' @description wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param
#' @return
#' @seealso \code{\link{ez.info}} or \code{\link{ez.show}}
#' @examples
#' @export
ez.view = function(x, show.frq = T, show.prc = T, sort.by.name = F, ...){
    sjPlot::view_df(x, show.frq = show.frq, show.prc = show.prc, sort.by.name = sort.by.name, ...)
}
