# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' Compactly display the internal structure of an R object, a diagnostic function and an alternative to summary
#' (and to some extent, dput). Ideally, only one line for each ‘basic’ structure is displayed.
#' It is especially well suited to compactly display the (abbreviated) contents of (possibly nested) lists.
#' The idea is to give reasonable output for any R object. It calls args for (non-primitive) function objects.
#' @export
z.show = function(...){
    show(...)
    cat('------------------------------\n')
    str(...)
    cat('------------------------------\n')
    summary(...)
}

#' show information about a data frame or other object
#' @param
#' @return
#' @examples
#' Compactly display the internal structure of an R object, a diagnostic function and an alternative to summary
#' (and to some extent, dput). Ideally, only one line for each ‘basic’ structure is displayed.
#' It is especially well suited to compactly display the (abbreviated) contents of (possibly nested) lists.
#' The idea is to give reasonable output for any R object. It calls args for (non-primitive) function objects.
#' @export
z.info = z.show
