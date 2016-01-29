###**************************************************.
###*mainly data frame functions.
###**************************************************.
#' alias of data.frame
#' @export
#' @examples
#' sx = c("F", "F", "F", "M", "M", "M")
#' ht = c(69, 64, 67, 68, 72, 71)
#' wt = c(148, 132, 142, 149, 167, 165)
#' people = data.frame(sx, ht, wt)
z.frame = data.frame

#' all row names, alias of \code{\link{row.names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
z.rnames = row.names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
z.cnames = names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
z.names = names

#' reorder all cols
#' @param newColOrder c('','',''), number of cols must match
#' @return returns a new df, old one does not change
z.recols = function(df, newColOrder){
    return(df[newColOrder])
}

#' reorder a single col (sort of, see below)
#' @param movecommand sth like "v17, v18 before v3; v6, v16 last; v5 first", supports before/after, last/first
z.recol = function(df, movecommand) {
    # modified from http://stackoverflow.com/questions/12544888/
    invec = names(df)
    movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], ",|\\s+"),
                          function(x) x[x != ""])
    movelist <- lapply(movecommand, function(x) {
        Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
        ToMove <- setdiff(x, Where)
        list(ToMove, Where)
    })
    myVec <- invec
    for (i in seq_along(movelist)) {
        temp <- setdiff(myVec, movelist[[i]][[1]])
        A <- movelist[[i]][[2]][1]
        if (A %in% c("before", "after")) {
            ba <- movelist[[i]][[2]][2]
            if (A == "before") {
                after <- match(ba, temp)-1
            } else if (A == "after") {
                after <- match(ba, temp)
            }
        } else if (A == "first") {
            after <- 0
        } else if (A == "last") {
            after <- length(myVec)
        }
        myVec <- append(temp, values = movelist[[i]][[1]], after = after)
    }
    df[myVec]
}

#' rename all cols
#' @param newColName c('','',''), number of cols must match
#' @return returns a new df, old one does not change
#' @examples
#' @export
z.rncols = function(df,newColNames){
    names(df) = newColNames
    return(df)
}

#' rename a single col
#' @description alias of \code{\link[reshape]{rename}}
#' @param ... c("oldColName"="newColName")) or c(oldColName="newColName"))
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
z.rncol = function(df, ...){
    df = reshape::rename(df, ...)
    return(df)
}

#' create a new col, may use \code{\link[dplyr]{mutate}} instead
#' @param newColName ''
#' @param defaultVal NA (default)
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
z.newcol = function(df, newColName, defaultVal=NA){
    df[newColName] = defaultVal
    return(df)
}

#' delete one or many cols, may use \code{\link[dplyr]{select}} instead
#' @param del sth like 'Month' or c('Month','Day')
#' @return returns a new df, old one does not change
#' @examples
#' @export
z.del = function(df,del){
    df[del] = NULL
    return(df)
}
