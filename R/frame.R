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



#' length of an object
#' @export
z.len = function(x) {
    if (class(x) == 'character') {
        # require('stringi')
        # return(stri_length(x))  # in case of stri_length(NA) = NA
        return(nchar(x))  # nchar(NA) = 2
    }
    else if (class(x) == 'data.frame') {
        return(nrow(x))
    }
    else if (is.na(x) || is.nan(x)) {
        return(0)
    }
    else {
        return(length(x))
    }
}

#' size of an object
#' @param x data.frame
#' @param dimension 0=both, 1=row, 2=col
#' @export
z.size = function(x,dimension=0) {
    if (dimension == 0) {
        return(dim(x))
    } else if (dimension == 1) {
        return(nrow(x))
    } else if (dimension == 2) {
        return(ncol(x))
    }
}

#' all row names, alias of \code{\link{row.names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{z.len}}, \code{\link{z.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{z.rnames}}, \code{\link{z.cnames}}, \code{\link{z.names}}
z.rnames = row.names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{z.len}}, \code{\link{z.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{z.rnames}}, \code{\link{z.cnames}}, \code{\link{z.names}}
z.cnames = names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{z.len}}, \code{\link{z.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{z.rnames}}, \code{\link{z.cnames}}, \code{\link{z.names}}
z.names = names



#' reorder all cols
#' @param newColOrder c('','',''), number of cols must match
#' @return returns a new df, old one does not change
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.recols = function(df, newColOrder){
    return(df[newColOrder])
}

#' reorder a single col (sort of, see below), alias of \code{\link{z.move}}
#' @param movecommand sth like "v17, v18 before v3; v6, v16 last; v5 first", supports before/after, last/first
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
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

#' reorder a single col (sort of, see below), alias of \code{\link{z.recol}}
#' @param movecommand sth like "v17, v18 before v3; v6, v16 last; v5 first", supports before/after, last/first
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.move = z.recol

#' rename all cols
#' @param newColName c('','',''), number of cols must match
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.rncols = function(df,newColNames){
    names(df) = newColNames
    return(df)
}

#' rename a single or many col
#' @description alias of \code{\link[reshape]{rename}} \code{\link{z.rename}}
#' @param replace c("oldColName"="newColName")) or c(oldColName="newColName"))
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.rncol = reshape::rename

#' rename a single or many col
#' @description alias of \code{\link[reshape]{rename}} \code{\link{z.rncol}}
#' @param replace c("oldColName"="newColName")) or c(oldColName="newColName"))
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.rename = reshape::rename

#' create a new col, may use \code{\link[dplyr]{mutate}} instead
#' @param newColName ''
#' @param defaultVal NA (default)
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.newcol = function(df, newColName, defaultVal=NA){
    df[newColName] = defaultVal
    return(df)
}

#' alias of \code{\link[dplyr]{mutate}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.compute = dplyr::mutate

#' alias of \code{\link[dplyr]{filter}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.select = dplyr::filter

#' alias of \code{\link[dplyr]{arrange}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.sort = dplyr::arrange

#' alias of \code{\link[dplyr]{distinct}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.unique = dplyr::distinct

#' alias of \code{\link[dplyr]{group_by}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.split = dplyr::group_by

#' alias of \code{\link[dplyr]{left_join}}
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.leftjoin = dplyr::left_join

#' delete one or many cols, may use \code{\link[dplyr]{select}} instead
#' @description alias of \code{\link{z.delete}}
#' @param del sth like 'Month' or c('Month','Day')
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.del = function(df,del){
    df[del] = NULL
    return(df)
}

#' delete one or many cols, may use \code{\link[dplyr]{select}} instead
#' @description alias of \code{\link{z.del}}
#' @param del sth like 'Month' or c('Month','Day')
#' @return returns a new df, old one does not change
#' @examples
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.delete = z.del
