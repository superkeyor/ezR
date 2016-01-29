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
