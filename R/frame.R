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
ez.frame = data.frame

#' length of an object
#' @return if a character vector with only one element, returns the number of characters in that item
#' \cr if a character vector with more than one element, returns the number of elements
#' \cr data frame, returns the number of rows
#' \cr NA returns 0
#' \cr everything else, call length()
#' @examples
#' # x <- c("Hello", "world!")
#' # > length(x)
#' # [1] 2
#' # > nchar(x)
#' # [1] 5 6
#' @export
ez.len = function(x) {
    if (class(x) == 'character') {
        # require('stringi')
        # return(stri_length(x))  # in case of stri_length(NA) = NA

        # if character vector with only one element, returns the number of characters in that item
        if (length(x)==1) {
            return(nchar(x))  # nchar(NA) = 2
        } 
        # else a character vector with more than one element, returns the number of elements
        else {
            return(length(x))
        }
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
#' @param dim 1=row, 2=col, 3=both
#' @export
ez.size = function(x,dim=3) {
    if (dim == 3) {
        return(dim(x))
    } else if (dim == 1) {
        return(nrow(x))
    } else if (dim == 2) {
        return(ncol(x))
    }
}

#' all row names, alias of \code{\link{row.names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{ez.len}}, \code{\link{ez.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{ez.rnames}}, \code{\link{ez.cnames}}, \code{\link{ez.names}} \code{\link{ez.reindex}}
ez.rnames = row.names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{ez.len}}, \code{\link{ez.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{ez.rnames}}, \code{\link{ez.cnames}}, \code{\link{ez.names}}
ez.cnames = colnames

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{ez.len}}, \code{\link{ez.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{ez.rnames}}, \code{\link{ez.cnames}}, \code{\link{ez.names}}
ez.names = colnames

#' reconstruct to long format, wrapper of \code{\link[stats]{reshape}}
#' @description can handle one (similar to gather(), which does not require index input) or two repetitions
#' @param id unique identification variable, or variable combination
#' @param indexname variable name (column to be created) for timing/repetition/index variable, such as "session"
#' @param index level name (value labels to be created) for each timing/repetition/index point, such as c("1,2"), c("Pre, Post")
#' @param measurename variable name (column/columns to be created) for the measurement, such as "BDI"
#' @param measure column names (existing) that are the repeated measures, such as c("BDI_Pre","BDI_Post")
#' @param drop variables to drop before reshaping
#' @details
#' @note refer to my spss syntax 'Time(2) | Measure1(Pre1 Post1) | Measure2(Pre2 Post2) +/- Subject'
#' \cr if index=c("Pre","Post"), then the character would not be viewed by ez.view; index=1:2 will be int and fine.
#' @examples
#' df <- data.frame(
#'     id = 1:10,
#'     time = as.Date('2009-01-01') + 0:9,
#'     Q3.2.1. = rnorm(10, 0, 1),
#'     Q3.2.2. = rnorm(10, 0, 1),
#'     Q3.2.3. = rnorm(10, 0, 1),
#'     Q3.3.1. = rnorm(10, 0, 1),
#'     Q3.3.2. = rnorm(10, 0, 1),
#'     Q3.3.3. = rnorm(10, 0, 1)
#' )
#'ez.2long(df, "id",
#'        "loop_number",c(1:3),
#'        c('Q3.2','Q3.3'),
#'        list(c("Q3.2.1.","Q3.2.2.","Q3.2.3."),c("Q3.3.1.","Q3.3.2.","Q3.3.3.")))
#' @return returns a new df, label attributes seem to be intact.
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2long = function(df, id, indexname, index, measurename=NULL, measure=NULL, drop=NULL,...){
    # 'Time(2) | Measure1(Pre1 Post1) | Measure2(Pre2 Post2) +/- Subject'
    # timevar: variable name for timing/repetition variable, such as "session"
    # times: level name for each timing/repetition point, such as c("1,2"), c("Pre, Post")
    # v.names: variable name for the measurement, such as "BDI"
    # varying: column names that are the repeated measures, such as c("BDI_Pre","BDI_Post")
    # drop: variables to drop before reshaping
    # sep: separator in the variable names

    # re-assure df is a data frame, rather than tbl or data.table types, which reshape would not work well
    df = data.frame(df)
    result = stats::reshape(df, idvar=id,
                            timevar=indexname, times=index,
                            v.names=measurename,varying=measure,
                            direction="long",
                            drop=drop,
                            sep="_",...)
    row.names(result) <- NULL
    return(result)
}

#' reconstruct to wide format, wrapper of \code{\link[stats]{reshape}}
#' @description
#' @param id unique identification variable, or variable combination
#' @param indexname variable name for timing/repetition/index variable, such as "session"
#' @param measure column names that are the repeated measures, such as c("BDI_Pre","BDI_Post")
#' @param drop variables to drop before reshaping
#' @details
#' @note refer to my spss syntax 'SUBJID * Time [School] - Measure2'
#' @examples
#' set.seed(10)
#' df <- data_frame(
#'     Person = rep(c("greg", "sally", "sue"), each=2),
#'     Time = rep(c("Pre", "Post"), 3),
#'     Score1 = round(rnorm(6, mean = 80, sd=4), 0),
#'     Score2 = round(jitter(Score1, 15), 0),
#'     Score3 = 5 + (Score1 + Score2)/2
#' )
#' df = data.frame(df)
#' ez.2wide(df,
#'         "Person",
#'         "Time",
#'         c("Score1","Score2","Score3"))
#' @return returns a new df. Because of the structural change, lable attributes would be lost and can NOT be copied with \code{\link[sjmisc]{copy_labels}}--not a subset.
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2wide = function(df, id, indexname, measure=NULL, drop=NULL,...){
    # 'SUBJID * Time [School] - Measure2'
    # note: v.names not varying
    df = data.frame(df)
    result = stats::reshape(df,
                            idvar=id,
                            timevar=indexname,
                            v.names=measure,
                            direction="wide",
                            drop=drop,
                            sep="_",...)
    row.names(result) <- NULL
    return(result)
}

#' get value labels, wrapper of \code{\link[sjmisc]{get_labels}}
#' @description
#' @param
#' @details see also \code{\link[sjmisc]{get_values}}
#' @examples
#'
#' @return returns a list $varname
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.values.get = function(x, include.values=NULL, attr.only=T, include.non.labelled=F, ...){
    result=sjmisc::get_labels(x, include.values=include.values, attr.only=attr.only, include.non.labelled=include.non.labelled, ...)
    return(result)
}

#' set value labels, wrapper of \code{\link[sjmisc]{set_labels}}
#' @description
#' @param
#' @details
#' @examples
#' # 1 4 5 9 do not have to all appear in x
#' # notice the particular order and symbol: "strongly agree" <- 1
#' set_labels(x, c("strongly agree"=1,
#'                "totally disagree"=4,
#'                "refused"=5,
#'                "missing"=9))
#' @return returns a new changed df
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.values.set = function(x, valuelabels, force.labels=FALSE, force.values=FALSE, ...){
    result=sjmisc::set_labels(x, valuelabels, force.labels=force.labels, force.values=force.values, ...)
    return(result)
}

#' get variable label, wrapper of \code{\link[sjmisc]{get_label}}
#' @description
#' @param ... var1, var2,  one or many
#' @details
#' @examples
#'
#' @return returns character
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.label.get = function(...){
    result=sjmisc::get_label(list(...))
    return(result)
}

#' set variable label, wrapper of \code{\link[sjmisc]{set_label}}
#' @description
#' @param df data frame
#' @param varname variable name with quote ""
#' @param label explanatory string
#' @details
#' @examples
#'
#' @return returns a new changed df
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.label.set = function(df,varname,label){
    df[varname] <- sjmisc::set_label(df[varname],label)
    return(df)
}

#' convert a factor column (or all factor columns) in a data frame into character type
#' @param x a data frame or a vector/col
#' @param col if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @details Both value and variable label attributes will be removed when converting variables to characters.
#' @examples
#'
#' @return returns a character vector or a data frame with changed col(s)
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2character = function(x, col=NULL){
    if (is.data.frame(x) & is.null(col)){
        result = dplyr::mutate_if(x, is.factor, as.character)
    } else if (is.data.frame(x) & !is.null(col)) {
        x[[col]] = as.character(x[[col]])
        result=x
    } else {
        result = as.character(x)
    }
    return(result)
}

#' wrapper of \code{\link[sjmisc]{to_label}}
#' @description continous/factorial number-->factorial level string, say, gender=0/1-->male/female
#' \cr more "agressive" than \code{\link{ez.2factor}}; opposite of \code{\link{ez.2value}}
#' @param x a data frame or a vector/col
#' @param col if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param drop.is_na ignore is_na attr, if yes, treat as NA
#' @details Both value and variable label attributes will be removed when converting variables to factors.
#' @examples
#'
#' @return returns a factor with string as its levels or a data frame with changed col(s)
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2label = function(x, col=NULL, add.non.labelled=TRUE, drop.is_na=FALSE,...){
    if (is.data.frame(x) & !is.null(col)){
        x[col]=sjmisc::to_label(x[col], add.non.labelled=add.non.labelled, drop.na=drop.is_na)
        result=x
    } else {
        result=sjmisc::to_label(x, add.non.labelled=add.non.labelled, drop.na=drop.is_na)
    }
    return(result)
}

#' wrapper of \code{\link[sjmisc]{to_factor}}
#' @description continous number-->categorical number
#' \cr converts a variable into a factor, but preserves variable and value label attributes.
#' \cr more "gentle" than \code{\link{ez.2label}}; opposite of \code{\link{ez.2value}}
#' @param x a data frame or a vector/col
#' @param col if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @details
#' @examples
#'
#' @return returns a factor with number as its levels or a data frame with changed col(s)
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2factor = function(x, col=NULL, add.non.labelled=TRUE, drop.na=FALSE, ref.lvl=NULL,...){
    if (is.data.frame(x) & !is.null(col)){
        x[col]=sjmisc::to_factor(x[col], add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
        result=x
    } else {
        result=sjmisc::to_factor(x, add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
    }
    return(result)
}

#' wrapper of \code{\link[sjmisc]{to_value}}; see also \code{\link{ez.num}}
#' @description continous number<--categorical string/number
#' @param x a data frame or a vector/col
#' @param col if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param start.at starting index, i.e. the lowest numeric value of the variable's value range. By default, this argument is NULL, hence the lowest value of the returned numeric variable corresponds to the lowest factor level (if factor is numeric) or to 1 (if factor levels are not numeric).
#' @details opposite of \code{\link{ez.2factor}}, \code{\link{ez.2label}}
#' @examples
#' # starting at 1
#' dummy <- factor(c("D", "F", "H"))
#' to_value(dummy)
#' # [1] 1 2 3
#' # attr(,"labels")
#' # D F H
#' # 1 2 3
#'
#' dummy <- factor(c("6", "4", "2"))
#' to_value(dummy)
#' # [1] 6 2 4
#' # attr(,"labels")
#' # 2 4 6 
#' # 2 4 6 
#' @return returns a numeric variable or a data frame with changed col(s)
#' \cr if x is a factor with chars, will be converted to 1 2 3 etc, see the example
#' \cr if x, however, is a factor with chars of numbers "2","4","6", will be converted to 2 4 6 etc, see the example
#' \cr \code{\link{ez.num}} keeps the same char as is
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2value = function(x, col=NULL, start.at=NULL, keep.labels=TRUE,...){
    if (is.data.frame(x) & !is.null(col)){
        # reset factor levels in a df after its levels have been modified, relevel a factor in order to reflect its new levels
        if (length(levels(x[[col]])) > nrow(x)){
            cat('Seems you have changed the factor levels of the column in data frame, resetting levels...\n')
            x[[col]] = factor(x[[col]], unique(as.character(x[[col]])))
        }
        x[col]=sjmisc::to_value(x[col], start.at=start.at, keep.labels=keep.labels,...)
        result=x
    } else {    
        result=sjmisc::to_value(x, start.at=start.at, keep.labels=keep.labels,...)
    }
    return(result)
}



# copied from https://cran.r-project.org/web/packages/Deducer/Deducer.pdf
# though the function can accepts a data frame with multiple columns
# when adapted, restrict the use to be with a data frame with only one column
.recode_helper<-function(data,recodes){
    recode.other<-function(var){
        if(is.factor(var)) stop("use recode.factor to recode factors")
        warning.flag<-TRUE
        result <- var
        else.target<-""
        if(else.term!=""){
            else.target <- eval(parse(text = strsplit(else.term, "->")[[1]][2]))
            result[1:length(var)] <- else.target
        }
        if(is.numeric(var)){
            Lo <- min(var, na.rm = TRUE)
            Hi <- max(var, na.rm = TRUE)
        }else{
            Lo <-""
            Hi <-max(var, na.rm = TRUE)
        }
        for(term in recode.list){
            if(0 < length(grep(":", term))){
                if(is.character(var) && warning.flag){
                    warning("Recoding a range of characters may not do what you think it does.\n Example: '15' is less than '9'.")
                    warning.flag<-FALSE
                }
                range <- strsplit(strsplit(term, "->")[[1]][1], ":")
                low <- eval(parse(text = range[[1]][1]))
                high <- eval(parse(text = range[[1]][2]))
                if(high<low) next
                target <- eval(parse(text = strsplit(term, "->")[[1]][2]))
                result[(var >= low) & (var <= high)] <- target
            }else{
                set <- eval(parse(text = strsplit(term, "->")[[1]][1]))
                target <- eval(parse(text = strsplit(term, "->")[[1]][2]))
                for (val in set) {
                    if (is.na(val))
                        result[is.na(var)] <- target
                    else{
                        result[var == val] <- target
                    }
                }
            }
        }
        return(result)
    }

    recode.factor<-function(var){
        if(!is.factor(var)) stop("var must be a factor")
        result<-var
        else.target<-""
        if(else.term!=""){
            else.target <- eval(parse(text = strsplit(else.term, "->")[[1]][2]))
            if(!(else.target %in% levels(result))){
                levels(result)<-c(levels(result),else.target)
            }
            result<-factor(rep(else.target,length(var)),levels=else.target)
        }

        for(term in recode.list){
            Lo<-levels(var)[1]
            Hi<-levels(var)[length(levels(var))]
            if(0 < length(grep(":", term))){
                range <- strsplit(strsplit(term, "->")[[1]][1], ":")
                low <- eval(parse(text = range[[1]][1]))
                low<-which(levels(var)==low)[1]
                if(is.na(low)) stop(paste("Lower value in range not a valid factor level.",term))
                high <- eval(parse(text = range[[1]][2]))
                high <- which(levels(var)==high)[1]
                if(is.na(high)) stop(paste("upper value in range not a valid factor level.",term))
                if(high<low) stop(paste("Upper value must be ordered after lower value in the factor ordering.",term))

                target <- eval(parse(text = strsplit(term, "->")[[1]][2]))
                set<-levels(var)[low:high]
                if(!(target %in% levels(result))){
                    levels(result)<-c(levels(result),target)
                }
                result[var %in% set] <- target
                set<-setdiff(set,target)
                levels(result)<-ifelse(levels(result) %in% set,NA,levels(result))
            }else{
                set <- eval(parse(text = strsplit(term, "->")[[1]][1]))
                target <- eval(parse(text = strsplit(term, "->")[[1]][2]))
                for (val in set) {
                    if(!(target %in% levels(result))){
                        levels(result)<-c(levels(result),target)
                    }
                    if (is.na(val))
                        result[is.na(var)] <- target
                    else{
                        result[var == val] <- target
                        if (!is.na(val) && !is.na(target) && val != target){
                            levels(result)<-ifelse(levels(result)==val,NA,levels(result))
                        }
                    }
                }
            }
        }
        return(result)
    }

    if(!is.data.frame(data)) data<-as.data.frame(data)
    recode.list <- strsplit(recodes, ";")[[1]]
    else.term<-""
    else.ind<-c()
    for(i in 1:length(recode.list)){
        first.part<-strsplit(recode.list[[i]],"->")[[1]][1]
        if(length(grep("else",first.part))>0 && length(grep("'",first.part))<1){
            else.term<-recode.list[[i]]
            else.ind<-c(else.ind,-i)
        }
    }
    if(length(else.ind)>0) recode.list<-recode.list[else.ind]
    result.data<-data.frame(1:dim(data)[1])
    for(variable in data){
        if(is.factor(variable)){
            result.data<-data.frame(result.data,recode.factor(variable),stringsAsFactors=FALSE)
        }else result.data<-data.frame(result.data,recode.other(variable),stringsAsFactors=FALSE)
    }
    return(result.data[-1])
}
#' recode
#' @description Recodes one single according to a set of rules
#' \cr\cr ez.recode replaces the original var with recoded var;
#' \cr ez.recode2 saves orignal var as var_ori, and then recodes var
#' \cr see also \code{\link{ez.replace}}
#' @param df data.frame to be recoded
#' @param varName the name of var to be recoded, must be a string in quotes ""
#' @param recodes Definition of the recoding rules. See details
#' @details recodes contains a set of recoding rules separated by ";". There are three different types of recoding rules:
#' \itemize{
#'  \item{}{The simplest codes one value to another. If we wish to recode 1 into 2, we could use the rule "1=2;".}
#'  \item{}{A range of values can be coded to a single value using "1:3=4;". This rule would code all values between 1 and 3 inclusive into 4. For factors, a value is between two levels if it is between them in the factor ordering. One sided ranges can be specified using the lo and hi key words (e.g."lo:3=0; 4:hi=1")}
#'  \item{}{Default conditions can be coded using "else." For example, if we wish to recode all values >=0 to 1 and all values <0 to missing, we could use ("0:hi=1; else=NA")}
#' }
#' \cr Works with characters/factors as well e.g., ('Gr',"'U1'='U';'U2'='U';'R1'='R';'R2'='R'")
#' \cr characters to number does not work directly e.g., ('Gr',"'U1'=2;'U2'=3")  --> 2, 3 are converted to "2", "3" (char of number)
#' \cr but number to character works directly, char->char, factor->factor
#' \cr for factors, no need to reset levels (auto reset)
#' \cr The conclusion is: numeric<->numeric without quote
#' \cr but if newval is quoted character, then numeric->char, char->char, factor->factor
#' \cr See the example section for more detail.
#'
#' @note Please note following behaviours of the function:
#'       \itemize{
#'         \item the \code{"else"}-token should be the last argument in the \code{recodes}-string.
#'         \item the \code{"else"}-token is optional. if not specified, simply copy over else.
#'         \item if multiple ranges overlap, the latter one prevails. 1:3=1;3:5=2 (3->2 finally).
#'         \item hi=Hi=HI=max, lo=Lo=LI=min, :=thru=Thru=THRU (mimic SPSS recode syntax)  -> can replace = as well
#'         \item Variable label attributes (see, for instance, \code{\link{get_label}}) are preserved if exists, however, value label attributes are removed (makes sense, right)
#'         \item the \code{\link[sjmisc]{rec}} function in sjmisc does not work well with double numbers (eg, 3.59)
#' }
#'
#' @author Jerry Zhu modified from Ian Fellows (pkg Deducer) adapted from code by John Fox (car)
#' @examples
#' data<-data.frame(a=rnorm(100),b=rnorm(100),male=rnorm(100)>0)
#' ez.recode(data, "a", "lo:0 = 0;0:hi = 1;")
#' ez.recode(data, "b", "lo:0 = 0;0:hi = 1;")
#' ez.recode(data, "a", "lo:0 = 'low';0:hi = 1;")  
#'          #a was numeric type, now is character type 
#'          #note: for hi=1, the 1 is not even quoted
#'          #can be quoted hi='1', but it does not matter here
#' data <- ez.recode(data,"male", "1 = 'Male';FALSE = 'Female';else = NA;")
#'          #both 1 and TRUE = 'Male' work
#'          #the last semicolon; after NA is not necessary
#'          #male was initially a logic type, now is a character type
#'
#' data=data.frame(a=c('r1','r2'))
#' ez.recode(data,'a','"r1"="3"')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","r2")
#' ez.recode(data,'a','"r1"=3')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","r2")
#' ez.recode(data,'a','"r1"=3;"r2"=4')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","4")
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
ez.recode = function(df, varName, recodes){
    recodes = gsub("min","Lo",recodes,fixed=True)
    recodes = gsub("max","Hi",recodes,fixed=True)
    recodes = gsub("Min","Lo",recodes,fixed=True)
    recodes = gsub("Max","Hi",recodes,fixed=True)
    recodes = gsub("MIN","Lo",recodes,fixed=True)
    recodes = gsub("MAX","Hi",recodes,fixed=True)
    recodes = gsub("LO","Lo",recodes,fixed=True)
    recodes = gsub("HI","Hi",recodes,fixed=True)
    recodes = gsub("lo","Lo",recodes,fixed=True)
    recodes = gsub("hi","Hi",recodes,fixed=True)
    recodes = gsub("=","->",recodes,fixed=True)
    recodes = gsub(" thru ",":",recodes,fixed=True)
    recodes = gsub(" THRU ",":",recodes,fixed=True)
    recodes = gsub(" Thru ",":",recodes,fixed=True)

    newVar = .recode_helper(df[varName],recodes)
    # the helper function changes column name to sth like recode.other.variable.
    # now change it back
    names(newVar) = varName
    # remove all value labels attr, with graceful failure
    newVar=tryCatch(sjmisc::set_labels(newVar,""), error=function(e) newVar, warning = function(w) newVar, finally=newVar)
    cmd = sprintf("reshape::rename(df,c(%s='%s'))",varName,paste0(varName,'_ori'))
    # parse: http://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    df = eval(parse(text=cmd))
    df = dplyr::bind_cols(df,newVar)
    cmd = sprintf("ez.move(df,'%s before %s')",varName, paste0(varName,'_ori'))
    df = eval(parse(text=cmd))

    cmd = sprintf("dplyr::select(df,-%s)",paste0(varName,'_ori'))
    df = eval(parse(text=cmd))
    return(df)
}

#' recode
#' @description Recodes one single according to a set of rules
#' \cr\cr ez.recode replaces the original var with recoded var;
#' \cr ez.recode2 saves orignal var as var_ori, and then recodes var
#' \cr see also \code{\link{ez.replace}}
#' @param df data.frame to be recoded
#' @param varName the name of var to be recoded, must be a string in quotes ""
#' @param recodes Definition of the recoding rules. See details
#' @details recodes contains a set of recoding rules separated by ";". There are three different types of recoding rules:
#' \itemize{
#'  \item{}{The simplest codes one value to another. If we wish to recode 1 into 2, we could use the rule "1=2;".}
#'  \item{}{A range of values can be coded to a single value using "1:3=4;". This rule would code all values between 1 and 3 inclusive into 4. For factors, a value is between two levels if it is between them in the factor ordering. One sided ranges can be specified using the lo and hi key words (e.g."lo:3=0; 4:hi=1")}
#'  \item{}{Default conditions can be coded using "else." For example, if we wish to recode all values >=0 to 1 and all values <0 to missing, we could use ("0:hi=1; else=NA")}
#' }
#' \cr Works with characters/factors as well e.g., ('Gr',"'U1'='U';'U2'='U';'R1'='R';'R2'='R'")
#' \cr characters to number does not work directly e.g., ('Gr',"'U1'=2;'U2'=3")  --> 2, 3 are converted to "2", "3" (char of number)
#' \cr but number to character works directly, char->char, factor->factor
#' \cr for factors, no need to reset levels (auto reset)
#' \cr The conclusion is: numeric<->numeric without quote
#' \cr but if newval is quoted character, then numeric->char, char->char, factor->factor
#' \cr See the example section for more detail.
#'
#' @note Please note following behaviours of the function:
#'       \itemize{
#'         \item the \code{"else"}-token should be the last argument in the \code{recodes}-string.
#'         \item the \code{"else"}-token is optional. if not specified, simply copy over else.
#'         \item if multiple ranges overlap, the latter one prevails. 1:3=1;3:5=2 (3->2 finally).
#'         \item hi=Hi=HI=max, lo=Lo=LI=min, :=thru=Thru=THRU (mimic SPSS recode syntax)  -> can replace = as well
#'         \item Variable label attributes (see, for instance, \code{\link{get_label}}) are preserved if exists, however, value label attributes are removed (makes sense, right)
#'         \item the \code{\link[sjmisc]{rec}} function in sjmisc does not work well with double numbers (eg, 3.59)
#' }
#'
#' @author Jerry Zhu modified from Ian Fellows (pkg Deducer) adapted from code by John Fox (car)
#' @examples
#' data<-data.frame(a=rnorm(100),b=rnorm(100),male=rnorm(100)>0)
#' ez.recode(data, "a", "lo:0 = 0;0:hi = 1;")
#' ez.recode(data, "b", "lo:0 = 0;0:hi = 1;")
#' ez.recode(data, "a", "lo:0 = 'low';0:hi = 1;")  
#'          #a was numeric type, now is character type 
#'          #note: for hi=1, the 1 is not even quoted
#'          #can be quoted hi='1', but it does not matter here
#' data <- ez.recode(data,"male", "1 = 'Male';FALSE = 'Female';else = NA;")
#'          #both 1 and TRUE = 'Male' work
#'          #the last semicolon; after NA is not necessary
#'          #male was initially a logic type, now is a character type
#'
#' data=data.frame(a=c('r1','r2'))
#' ez.recode(data,'a','"r1"="3"')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","r2")
#' ez.recode(data,'a','"r1"=3')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","r2")
#' ez.recode(data,'a','"r1"=3;"r2"=4')
#'          # a was factor wih level ("r1","r2"), now still a factor, with level ("3","4")
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
ez.recode2 = function(df, varName, recodes){
    recodes = gsub("min","Lo",recodes,fixed=True)
    recodes = gsub("max","Hi",recodes,fixed=True)
    recodes = gsub("Min","Lo",recodes,fixed=True)
    recodes = gsub("Max","Hi",recodes,fixed=True)
    recodes = gsub("MIN","Lo",recodes,fixed=True)
    recodes = gsub("MAX","Hi",recodes,fixed=True)
    recodes = gsub("LO","Lo",recodes,fixed=True)
    recodes = gsub("HI","Hi",recodes,fixed=True)
    recodes = gsub("lo","Lo",recodes,fixed=True)
    recodes = gsub("hi","Hi",recodes,fixed=True)
    recodes = gsub("=","->",recodes,fixed=True)
    recodes = gsub(" thru ",":",recodes,fixed=True)
    recodes = gsub(" Thru ",":",recodes,fixed=True)
    recodes = gsub(" THRU ",":",recodes,fixed=True)

    newVar = .recode_helper(df[varName],recodes)
    # the helper function changes column name to sth like recode.other.variable.
    # now change it back
    names(newVar) = varName
    # remove all value labels attr, with graceful failure
    newVar=tryCatch(sjmisc::set_labels(newVar,""), error=function(e) newVar, warning = function(w) newVar, finally=newVar)
    cmd = sprintf("reshape::rename(df,c(%s='%s'))",varName,paste0(varName,'_ori'))
    # parse: http://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    df = eval(parse(text=cmd))
    df = dplyr::bind_cols(df,newVar)
    cmd = sprintf("ez.move(df,'%s before %s')",varName, paste0(varName,'_ori'))
    df = eval(parse(text=cmd))
    return(df)
}

#' replace a single value in data frame with another value
#' @description replace within one or more than one columns, or entire data frame (ie, all columns)
#' \cr smilar to \code{\link{ez.recode}} numeric->char, char->char, factor->factor
#' \cr wrapper of df[[col]][which(df[[col]]==oldval)] <- newval
#' \cr the "==" syntax within "which()" could be modified 
#' \cr
#' \cr when col not provided (see param, note, example below), internally calling dplyr::mutate_all(ifelse()), therefore the change of data type follows mutate()
#' @param df data frame
#' @param col column name in string (not numbers), can be single, or multiple/vector eg, c('col1','col2'). If skipped (not provided), all columns used
#' @param oldval old value (e.g., -Inf, NA), can only be single, not multiple/vector. Note would not differentiate 5.0 and 5
#' @param newval new value (e.g., NA), can only be single, not multiple/vector
#' @return returns a new df, old one does not change
#' @family data transformation functions
#' @export
#' @note when 4 parameters provided, it is recognized as (df,col,oldval,newval) 
#' \cr when 3 parameters provided, it is recognized as (df,oldval,newval) 
#' see example
#' @seealso \code{\link{ez.strreplace}} \code{\link{ez.recode}} \code{\link{ez.recode2}} 
#' @examples
#' data=data.frame(a=factor(c(1,2)))
#' ez.replace(data,'a',1,3) %>% .$a
#' ez.replace(data,'a',1,'abc') %>% .$a
#'           # a was factor with level (1,2), now is factor with level (2,3), (2 abc)
#' 
#' data=data.frame(a=c('r1','r2'))
#' ez.replace(data,'a','r1',3) %>% .$a
#'           # a was factor with level ('r1','r2'), now is factor with level ('3','r2')
#' 
#' data=data.frame(a=c('r1','r2'),stringsAsFactors = F)
#' ez.replace(data,'a','r1',3) %>% .$a
#' ez.replace(data,'a','r1',NA) %>% .$a 
#'           # a was char, now is still char, (NA counted here as char)
#' 
#' data=data.frame(a=c(1,2))
#' ez.replace(data,'a',1,111) %>% .$a
#' ez.replace(data,'a',1,NA) %>% .$a
#'           # a was numeric, now is still numeric, (NA counted here as numeric)
#' 
#' ez.replace(data,'a',1,'111') %>% .$a
#' ez.replace(data,'a',1,'abc') %>% .$a
#'           # a was numeric, now is character
#' 
#' data=iris[1:10,]; data[1,2]=NA; data[2,5]=NA; data['TV']='COBY'
#' ez.replace(data,c('Sepal.Width','Petal.Length','Petal.Width','Species'),NA,3.1415)
#' ez.replace(data,NA,3.1415)
#'           # Species was factor, now is numeric (factor->numeric)
#' ez.replace(data,NA,'replaced')
#'           # Sepal.Width was numeric, now is char
#'           # Species was factor, now is char (factor->char of num)
#' ez.replace(data,5.1,3.14)
#'           # Sepal.Length was numeric, now is still numeric
#' ez.replace(data,'COBY','Mac')
#'           # TV was char, now is still char
ez.replace = function(df, col, oldval, newval=NULL){
    # four parameters passed
    if (!is.null(newval)) {
        cols = col
        for (col in cols) {
            # for factor, you cannot directly assign, otherwise get "invalid factor level, NA generated"
            if (is.factor(df[[col]])) {
                df[[col]]=as.character(df[[col]])
                if (is.na(oldval)) {
                    cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', sum(is.na(df[[col]])), col, as.character(oldval), as.character(newval)))
                    df[[col]][which(is.na(df[[col]]))] <- newval
                } else {
                    cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', length(which(df[[col]]==oldval)), col, as.character(oldval), as.character(newval)))
                    df[[col]][which(df[[col]]==oldval)] <- newval
                }
                df[[col]]=as.factor(df[[col]])
            } else {
                if (is.na(oldval)) {
                    cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', sum(is.na(df[[col]])), col, as.character(oldval), as.character(newval)))
                    df[[col]][which(is.na(df[[col]]))] <- newval
                } else {
                    cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', length(which(df[[col]]==oldval)), col, as.character(oldval), as.character(newval)))
                    df[[col]][which(df[[col]]==oldval)] <- newval
                }
            }
        }

    # three parameters passed        
    } else {
        # trick to recognize parameters
        newval=oldval;oldval=col
        if (is.na(oldval)) {
            # the dot here, I think, refers to each column, not related to . for %>%
            # mutate() will somehow auto convert columns of factor
            cat(sprintf('%5.0f values replaced in data frame (%s -> %s)\n', sum(colSums(is.na(df))), as.character(oldval), as.character(newval)))
            df = dplyr::mutate_all(df,funs(ifelse(is.na(.),newval,.)))
        } else {
            cat(sprintf('%5.0f values replaced in data frame (%s -> %s)\n', sum(colSums(df==oldval,na.rm=TRUE)), as.character(oldval), as.character(newval)))
            df = dplyr::mutate_all(df,funs(ifelse(.==oldval,newval,.)))
        }
    }
    return(df)
}

#' Count the occurrence of a single value in data frame columnwise, or rowwise, or both
#' @description count within one or more than one columns/rows, or entire data frame (ie, all columns/rows)
#' @param x data frame or vector, if vector, parameters col, dim are ignored
#' @param val value to be counted, could be NA. Note would not differentiate 5.0 and 5
#' @param col column in string (not number), single or vector. If NULL, all columns used
#' @param dim 1=along row (rowwise), 2=along col (colwse), 3=area, both, grand total (within specified cols/rows)
#' @return returns a data frame, if dim=1/2; a single value if dim=3. 
#' \cr vector input x always outputs a single value.
#' @examples
#' sx = c("F", "F", "F", "M", "M", "M")
#' ht = c(69, 64, 67, 68, 72, 71)
#' wt = c(148, 132, 142, 149, 167, 165)
#' people = data.frame(sx, ht, wt)
#' @export
ez.count = function(x, val, col=NULL, dim=3) {
    # assume a 1d vector
    if (!is.data.frame(x)) return(ifelse(is.na(val),sum(is.na(x)),sum(x==val,na.rm=TRUE)))

    df=if (!is.null(col)) x[col] else x

    # https://stackoverflow.com/a/40340152/2292993
    # do not use the protected list() option as in the link, it returns a list
    # ifelse would not return a matrix
    tmpMatrix=if (is.na(val)) is.na(df) else df==val
    if (dim==3) {
        return(sum(rowSums(tmpMatrix,na.rm=TRUE)))
    } else if (dim==1) {
        sumNamedVector=rowSums(tmpMatrix,na.rm=TRUE)
        return(data.frame(count=sumNamedVector))
    } else if (dim==2) {
        sumNamedVector=colSums(tmpMatrix,na.rm=TRUE)
        return(data.frame(as.list(sumNamedVector)))
    }
}

#' reorder all cols, or sort all cols alphabetically
#' @param newColOrder c('','',''), number of cols must match. 
#' or, newColOrder='az' or 'za', sort all cols alphabetically
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
ez.recols = function(df, newColOrder){
    # use all(), because newColOrder might be a vector
    # na.last=FALSE, makes na appears first, here it does not matter, because col names should not be NA
    if(all(newColOrder=='az')) newColOrder=order(colnames(df), na.last = FALSE, decreasing = FALSE)
    if(all(newColOrder=='za')) newColOrder=order(colnames(df), na.last = FALSE, decreasing = TRUE)
    if (length(newColOrder)!=length(colnames(df))) stop('new col names length mismatches old one')
    return(df[newColOrder])
}

#' reorder a single col (sort of, see below), alias of \code{\link{ez.move}}
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
ez.recol = function(df, movecommand) {
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

#' reorder a single col (sort of, see below), alias of \code{\link{ez.recol}}
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
ez.move = ez.recol

#' clean col names
#' @description replace certain characters (all occurrence) in all column names, using regular expression and gsub()
#' @param df df
#' @param pattern search
#' @param replacement replacement
#' @param fixed FALSE=regex mode on, TRUE=regex mode off
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param perl Perl-compatible regexps be used, without perl, [[:space:][:punct:]] works, but not [\\s[:punct:]]  
#' so seems always a good idea to turn on perl compatible. see \code{\link{gsub}}. 
#' ignored when fixed=TRUE
#' @return returns a new df with column names cleaned, old df does not change
#' @examples
#' all upper to lower using regex (ignore.case=FALSE or TRUE does not matter)
#' ez.clcols(iris,pattern='([[:upper:]])', replacement = '\\L\\1', perl = TRUE, ignore.case=FALSE)
#' @export
ez.clcols <- function(df,pattern='[[:space:][:punct:]]',replacement='_',fixed=FALSE,ignore.case=FALSE,perl=TRUE) { 
    # ignore perl when fixed is true, otherwise issuing a warning
    if (fixed) perl=FALSE
    colnames(df) <- gsub(pattern, replacement, colnames(df), fixed=fixed, ignore.case=ignore.case, perl=perl)
    return(df)
} 

#' rename all cols, see also \code{\link{ez.rncol}}
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
ez.rncols = function(df,newColNames){
    names(df) = newColNames
    return(df)
}

#' convert all column names to lower case
#' @param df a data frame
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
ez.2lower = function(df){
    result = df
    names(result) = tolower(names(result))
    return(result)
}

#' rename a single or many col, see also \code{\link{ez.rncols}}
#' @description alias of \code{\link[reshape]{rename}} \code{\link{ez.rename}}
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName"), c(wt = "weight", cyl = "cylinders")
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
ez.rncol = reshape::rename

#' rename a single or many col
#' @description alias of \code{\link[reshape]{rename}} \code{\link{ez.rncol}}
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName"), c(wt = "weight", cyl = "cylinders")
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
ez.rename = reshape::rename

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
ez.newcol = function(df, newColName, defaultVal=NA){
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
ez.compute = dplyr::mutate

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
ez.select = dplyr::filter

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
ez.sort = dplyr::arrange

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
ez.unique = dplyr::distinct

#' find the duplicated rows in a data frame or duplicated elements in a vector
#' @param x a data frame or a vector/col
#' @param col restrict to the columns where you would like to search for duplicates; e.g., 3, c(3), 2:5, "place", c("place","age")
#' \cr if x is a data frame, col is specified (e.g., "cond"), check that col only
#' \cr if x is a data frame, col is unspecified (i.e., NULL default), check all cols in x
#' \cr if x is not a data frame, col is ignored
#' @param vec TRUE/FALSE, if TRUE, returns a vector of TRUE/FALSE indicating duplicates; 
#' \cr if FALSE, returns a df with one column 'Duplicated' of TRUE/FALSE
#' @return return depends, see vec above
#' \cr this is different from the built-in R \code{\link{duplicated}}
#' \cr x <- c(1, 1, 4, 5, 4, 6)  duplicated(x) returns [1] FALSE TRUE FALSE FALSE TRUE FALSE
#' \cr but ez.duplicated(x) returns [1] TRUE TRUE TRUE FALSE TRUE FALSE
#' @export
ez.duplicated = function(x, col=NULL, vec=TRUE){
    if (is.data.frame(x) & !is.null(col)) {
        # R converts a single row/col to a vector if the parameter col has only one col
        # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
        x = x[,col,drop=FALSE]
    } else {
        x = x
    }
    
    # https://stackoverflow.com/a/7854620/2292993
    result = duplicated(x) | duplicated(x, fromLast=TRUE)
    if (!vec) {result = data.frame(Duplicated=result)}
    return(result)
}

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
ez.split = dplyr::group_by

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
ez.leftjoin = dplyr::left_join

#' delete/remove one or many cols, may use \code{\link[dplyr]{select}} instead, alias of \code{\link{ez.del}} \code{\link{ez.delete}} \code{\link{ez.rmcol}} \code{\link{ez.rmcols}}
#' @param cols sth like 'Month' or c('Month','Day')
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
ez.del = function(df,cols){
    df[cols] = NULL
    return(df)
}

#' @rdname ez.del
#' @export
ez.delete = ez.del

#' @rdname ez.del
#' @export
ez.rmcols = ez.del

#' @rdname ez.del
#' @export
ez.rmcol = ez.del

#' keep rows that have a certain number (range) of NAs anywhere/somewhere and delete others
#' @param df a data frame
#' @param col restrict to the columns where you would like to search for NA; eg, 3, c(3), 2:5, "place", c("place","age")
#' \cr default is NULL, search for all columns
#' @param n integer or vector, 0, c(3,5), number/range of NAs allowed.
#' \cr If a number, the exact number of NAs kept
#' \cr Range includes both ends 3<=n<=5
#' \cr Range could be -Inf, Inf
#' @param reindex whether to keep original row index or reindex
#' \cr eg, original row.names() is 1, 2, 3, then drop row 2
#' \cr if not reindex, new index is 1, 3
#' \cr if reindex, new index is 1, 2
#' @return returns a new df with rows that have NA(s) removed
#' @rdname ez.dropna
#' @export
ez.na.keep = function(df, col=NULL, n=0, reindex=TRUE){
    nbefore = nrow(df)
    if (!is.null(col)) {
        # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
        # R converts a single col to a vector if the parameter col has only one col, e.g., iris[,'Sepal.Length',drop=F]
        # but iris['Sepal.Length'] will not! (iris['Sepal.Length',drop=F] actually gives a warning!)
        # will not convert a single row, but it does not hurt to add drop=F, e.g., iris[1,,drop=F] OK
        df.temp = df[,col,drop=FALSE]
    } else {
        df.temp = df
    }

    if (length(n)==1){
        if (n==0) {
            # simply call complete.cases which might be faster
            result = df[complete.cases(df.temp),,drop=FALSE]
        } else {
            # credit: http://stackoverflow.com/a/30461945/2292993
            log <- apply(df.temp, 2, is.na)
            logindex <- apply(log, 1, function(x) sum(x) == n)
            result = df[logindex,,drop=FALSE]
        }
    }

    if (length(n)==2){
        min = n[1]; max = n[2]
        log <- apply(df.temp, 2, is.na)
        logindex <- apply(log, 1, function(x) {sum(x) >= min && sum(x) <= max})
        result = df[logindex,,drop=FALSE]
    }

    # https://stackoverflow.com/a/7570677/2292993
    if (reindex) {row.names(result) <- NULL}

    nafter=nrow(result)
    cat(sprintf('%d rows dropped (In: %d -> Out: %d).\n', nbefore-nafter, nbefore, nafter))

    return(result)
}

#' alias ez.na.keep, ez.dropna
#' @rdname ez.dropna
#' @export
ez.dropna = ez.na.keep

#' reset the index of a data frame from 1...N
#' @description internally call row.names(df) <- NULL
#' @param df a data frame
#' @return returns a new df
#' @examples
#' occasionally, the index of a data frame could be broken, eg, after removing a row: 
#' original row.names() is 1, 2, 3, then drop row 2
#' if not reindex, new index is 1, 3
#' if reindex, new index is 1, 2
#' @seealso \code{\link{ez.rnames}}
#' @export
ez.reindex = function(df){
    # https://stackoverflow.com/a/7570677/2292993
    row.names(df) <- NULL
    return(df)
}

#' create a header for a data frame; also create the data frame
#' @description wrapper of data.frame(), commonly use together with \code{\link{ez.append}}
#' @param ... e.g., col1=character(n), 'col2'=numeric(n) where n defaults to 0, represents n of rows in the data frame, '' around col name is optional
#' @param cols optional (default=NULL), specify only if massively assign col names (i.e. header), see example
#' @param stringsAsFactors defaults to FALSE (data.frame() defaults to TRUE)
#' @return returns an (empty) df with specified col names
#' @examples
#' postdoc=ez.header(name=character(),'salary'=numeric())
#' postdoc=ez.header(cols=c('name','salary'))  # <--note that 'name','salary' data type is logical in this usage
#' @export
ez.header = function(..., cols=NULL, stringsAsFactors=FALSE){
    if (is.null(cols)) {
        return(data.frame(...,stringsAsFactors=stringsAsFactors))
    } else {
        # http://stackoverflow.com/questions/9917545/r-define-dimensions-of-empty-data-frame
        df = data.frame(matrix(NA, nrow = 0, ncol = length(cols)))
        colnames(df) = cols
        return(df)
    }
}

#' append a row to an exisiting data frame
#' @description could be slow, commonly use together with \code{\link{ez.header}}
#' @param df df to be appended
#' @param newrow a vector, e.g.,  c("Ted", 25)
#' @param print2screen whether to print the new row to string (auto separated by tab), default TRUE
#' @return returns a new df, old passed df does not change
#' @examples
#' typically within a loop:  results = ez.append(results,c(var,p,R,U))
#' @export
ez.append = function(df, newrow, print2screen=TRUE){
    # http://vitalflux.com/learn-r-append-rows-data-frame/
    df[nrow(df)+1,] <- newrow
    if (print2screen) {cat(newrow, '\n', sep='\t')}
    return(df)
}