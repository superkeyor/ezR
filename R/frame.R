###**************************************************.
###*mainly data frame functions.
###**************************************************.
#' alias of data.frame
#' @description alias of data.frame
#' @export
#' @examples
#' sx = c("F", "F", "F", "M", "M", "M")
#' ht = c(69, 64, 67, 68, 72, 71)
#' wt = c(148, 132, 142, 149, 167, 165)
#' people = data.frame(sx, ht, wt)
ez.frame = data.frame

#' alias of \code{\link[data.table]{transpose}}
#' @description alias of \code{\link[data.table]{transpose}}
#' @export
ez.transpose = data.table::transpose

#' length of an object
#' @description length of an object
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
#' @description size of an object
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
#' @description all row names, alias of \code{\link{row.names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{ez.len}}, \code{\link{ez.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{ez.rnames}}, \code{\link{ez.cnames}}, \code{\link{ez.names}} \code{\link{ez.reindex}}
ez.rnames = row.names

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @description all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @export
#' @seealso \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{dim}}, \code{\link{length}}
#' \cr \code{\link{ez.len}}, \code{\link{ez.size}}
#' \cr \code{\link{names}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{row.names}},
#' \cr \code{\link{ez.rnames}}, \code{\link{ez.cnames}}, \code{\link{ez.names}}
ez.cnames = colnames

#' all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
#' @description all column names, alias of \code{\link{names}}, there are also names(), colnames(), rownames(), row.names() but no col.names()
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
#' @note refer to my spss syntax 'Time(2) | Measure1(Pre1 Post1) | Measure2(Pre2 Post2) +/- Subject'
#' \cr if index=c("Pre","Post"), then the character would not be viewed by ez.vx; index=1:2 will be int and fine.
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
ez.2long = function(df, id, indexname, index, measurename=NULL, measure=NULL, drop=NULL, ...){
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
                            sep="_", ...)
    row.names(result) <- NULL
    return(result)
}

#' reconstruct to wide format, wrapper of \code{\link[stats]{reshape}}
#' @description reconstruct to wide format, wrapper of \code{\link[stats]{reshape}}
#' @param id unique identification variable, or variable combination
#' @param indexname variable name for timing/repetition/index variable, such as "session"
#' @param measure column names that are the repeated measures, such as c("BDI_Pre","BDI_Post")
#' @param drop variables to drop before reshaping
#' @param sep could be '_acc_'
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
#' @return returns a new df. Because of the structural change, lable attributes would be lost and can NOT be copied with \code{\link{sjmisc_copy_labels}}--not a subset.
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.2wide = function(df, id, indexname, measure=NULL, drop=NULL, sep='_', ...){
    # 'SUBJID * Time [School] - Measure2'
    # note: v.names not varying
    df = data.frame(df)
    result = stats::reshape(df,
                            idvar=id,
                            timevar=indexname,
                            v.names=measure,
                            direction="wide",
                            drop=drop,
                            sep=sep, ...)
    row.names(result) <- NULL
    return(result)
}

#' factor 2 char
#' @description convert a column of factor type (or all factor columns) in a data frame into character type as.character(). Check with is.factor(). If not is.factor(), remain untouched.
#' @param x a data frame or a vector/col
#' @param col internally evaluated by eval('dplyr::select()')
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only.
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible factor cols in x
#' \cr        if x is not a data frame, col is ignored
#' @details Both value and variable label attributes will be kept when converting variables to characters.
#' @seealso \code{\link{ez.str}}
#' @return returns a character vector or a data frame with changed col(s)
#' @family data transformation functions
#' @note ez.2char only coverts factor, ez.str coverts all types, both use as.character
#' @export
ez.2char = function(x, col=NULL){
    char_if_fac = function (e) {
        if (is.factor(e)) {
            labels <- sjmisc_get_labels(e, attr.only = T, include.values = "n", include.non.labelled=F)
            varlab <- sjmisc_get_label(e)
            new_value = as.character(e)
            new_value <- sjmisc_set_labels(new_value, labels, force.labels = T)
            new_value <- sjmisc_set_label(new_value, varlab)
        } else {
            new_value = e
        }
        return(new_value)
    }

    result=x
    if (is.data.frame(x) & is.null(col)){
        # result = dplyr::mutate_if(x, is.factor, as.character)
        x[] = lapply(x,char_if_fac)
        result = x
    } else if (is.data.frame(x) & !is.null(col)) {
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     if (is.factor(x[[col]])) {
        #         x[[col]] = as.character(x[[col]])
        #         result=x
        #     }
        # }
        x[cols] = lapply(x[cols],char_if_fac)
        result = x
    } else {
        result = char_if_fac(x)
    }
    return(result)
}

#' e=e, f=f, g/h/i->i, j=j, k=k
#' @description e=e, f=f, g/h/i->i, j=j, k=k
#' \cr
#' \cr number ef[(0/1)]|   {attr number g(0/1) / factor attr number h[0/1]}-->factor char i[girl/boy]   |char jk[(girl/boy)]
#' @param x a data frame or a vector
#' @param col internally evaluated by eval('dplyr::select()')
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param add.non.labelled  logical, if TRUE, values without associated value label (ie, more values than "labels", hence not all values are labelled.) will also be converted to labels (as is). If FALSE, non-labelled values are converted to NA
#' @param drop.missing.value (jerry: do not use) if TRUE, all types of missing value codes are converted (is_na attr) into NA before x is converted as factor. If FALSE, ignore is_na attr, missing values will be left as their original codes.
#' @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#'          will be prefixed with their associated values.
#' @details Variable label will be kept, but value labels will be removed to avoid confusion after converting to factor.
#'\cr wrapper of \code{\link{sjmisc_to_label}}
#' @note See also \code{\link{ez.factorname}}
#' @examples
#' e=c(1,2); f=factor(1:2)
#' g=c(0,0,1,1,1,0); attr(g,'value.labels') <- c(boy=1,girl=0)
#' h=factor(c(0,0,1,1,1,0)); attr(h,'labels') <- c(boy=1,girl=0)
#' i=factor(c('girl','girl','boy','boy','boy','girl'))
#' j=c('x','y'); k=factor(c('x','y'))
#'
#' ez.2label: e=e, f=f, g/h/i->i, j=j, k=k
#' ez.2value: e=e, f(0,1)<-f[1,2], g<-g/h/i/j/k
#' ez.2factor: ef->f, g/h->h, i=i, factor[x,y,z]<j/k
#' @return returns a factor with string as its levels or a data frame with changed col(s)
#' @family data transformation functions
#' @export
ez.2label = function(x, col=NULL, add.non.labelled=TRUE, drop.missing.value=FALSE, prefix=FALSE, ...){
    if (is.data.frame(x) & !is.null(col)){
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     x[[col]] = ez.factorelevel(x[[col]])
        #     x[col]=sjmisc_to_label(x[col], add.non.labelled=add.non.labelled, drop.na=drop.missing.value)
        #     result=x
        # }
        x[cols] = lapply(x[cols],function(e,add.non.labelled,drop.na,prefix){sjmisc_to_label(ez.factorelevel(e), add.non.labelled=add.non.labelled, drop.na=drop.na, prefix=prefix)}, add.non.labelled=add.non.labelled, drop.na=drop.missing.value, prefix=prefix)
        result = x
    } else {
        x=ez.factorelevel(x)
        result=sjmisc_to_label(x, add.non.labelled=add.non.labelled, drop.na=drop.missing.value, prefix=prefix, ...)
    }
    return(result)
}

#' ef->f, g/h->h, i=i, factor[x,y,z]<j/k
#' @description ef->f, g/h->h, i=i, factor[x,y,z]<j/k
#' \cr number e->f[(0/1)]|   attr number g(0/1)-->factor attr number h[0/1]   |factor char i[girl/boy]   |char j->k[(girl/boy)]
#' @param x a data frame or a vector/col
#' @param col internally evaluated by eval('dplyr::select()')
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param add.non.labelled    Logical, if TRUE, non-labelled values also get value labels.
#' @param drop.na Logical, if TRUE, all types of missing value codes are converted into NA before x is converted as factor. If FALSE, missing values will be left as their original codes. See 'Examples' and get_na.
#' @param ref.lvl Numeric, specifies the reference level for the new factor. Use this parameter if a different factor level than the lowest value should be used as reference level. If NULL, lowest value will become the reference level. See ref_lvl for details.
#' @details Variable label will be kept. wrapper of \code{\link{sjmisc_to_factor}}
#' @examples
#' e=c(1,2); f=factor(1:2)
#' g=c(0,0,1,1,1,0); attr(g,'value.labels') <- c(boy=1,girl=0)
#' h=factor(c(0,0,1,1,1,0)); attr(h,'labels') <- c(boy=1,girl=0)
#' i=factor(c('girl','girl','boy','boy','boy','girl'))
#' j=c('x','y'); k=factor(c('x','y'))
#'
#' ez.2label: e=e, f=f, g/h/i->i, j=j, k=k
#' ez.2value: e=e, f(0,1)<-f[1,2], g<-g/h/i/j/k
#' ez.2factor: ef->f, g/h->h, i=i, factor[x,y,z]<j/k
#' @return returns a factor with number as its levels or a data frame with changed col(s)
#' @family data transformation functions
#' @export
ez.2factor = function(x, col=NULL, add.non.labelled=TRUE, drop.na=FALSE, ref.lvl=NULL, ...){
    if (is.data.frame(x) & !is.null(col)){
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     x[[col]] = ez.factorelevel(x[[col]])
        #     x[col]=sjmisc_to_factor(x[col], add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
        #     result=x
        # }
        x[cols] = lapply(x[cols],function(e,add.non.labelled,drop.na,ref.lvl){sjmisc_to_factor(ez.factorelevel(e), add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)}, add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
        result = x
    } else {
        x=ez.factorelevel(x)
        result=sjmisc_to_factor(x, add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
    }
    return(result)
}

#' e=e, f(0,1)<-f[1,2], g<-g/h/i/j/k
#' @description e=e, f(0,1)<-f[1,2], g<-g/h/i/j/k
#' \cr number e<-f[(0/1)]|   attr number g(0/1)<--factor attr number h[0/1] / factor char i[girl/boy] / char j/k[(girl/boy)]
#' @param x a data frame or a vector/col
#' @param col internally evaluated by eval('dplyr::select()')
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all possible cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param start.at starting index, i.e. the lowest numeric value of the variable's value range.
#' \cr If NULL, the lowest value of the returned numeric variable corresponds to the lowest factor level (if factor is numeric, eg, factor(1:2)->c(1,2))
#' \cr or to 1 (if factor levels are not numeric, factor(c('girl','boy'))->c(2,1)).
#' \cr
#' \cr Attention: ez.2value(c('','2','1'),start.at = NULL) -> c(1,3,2),  ez.2value(c('','2','1'),start.at = 0) -> c(0,2,1)
#' \cr
#' \cr To keep consistent with other R functions (eg, lm which converts numeric/non-numeric factor to values starting from 0), set start.at=0 in ez.2value(), then factor(1:2)->c(0,1), factor(c('girl','boy'))->c(1,0)
#' \cr in lm() the coding (0,1) vs.(1,2) does not affect slope, but changes intercept (but a coding from 1,2->1,3 would change slope--interval difference matters)
#' @details Variable label will be kept. opposite of \code{\link{ez.2factor}}, \code{\link{ez.2label}}  wrapper of \code{\link{sjmisc_to_value}}
#' @examples
#' e=c(1,2); f=factor(1:2)
#' g=c(0,0,1,1,1,0); attr(g,'value.labels') <- c(boy=1,girl=0)
#' h=factor(c(0,0,1,1,1,0)); attr(h,'labels') <- c(boy=1,girl=0)
#' i=factor(c('girl','girl','boy','boy','boy','girl'))
#' j=c('x','y'); k=factor(c('x','y'))
#'
#' ez.2label: e=e, f=f, g/h/i->i, j=j, k=k
#' ez.2value: e=e, f(0,1)<-f[1,2], g<-g/h/i/j/k
#' ez.2factor: ef->f, g/h->h, i=i, factor[x,y,z]<j/k
#'
#' ez.2value(c('','2','1'),start.at = NULL) # -> c(1,3,2)
#' ez.2value(c('','2','1'),start.at = 0) # -> c(0,2,1)
#' ez.num(c('','2','1')) # -> c(NA,2,1)
#' ez.2value(c("2","4","6.3"),start.at=NULL)  # -> (2.0 4.0 6.3)
#' ez.2value(factor(c("2","4","7.3")),start.at=0)  # -> (0.0 2.0 5.3)
#' for consistent and easy convertion, consider to use ez.num(c("2","4","7.3")), ez.num(factor(c("2","4","7.3")), force=T) in this case
#' @return returns a numeric variable or a data frame with changed col(s)
#' \cr if x is a factor with normal chars, will be converted to 1 2 3 etc, see the example
#' \cr if x, however, is a factor with chars of numbers ez.2value(c("2","4","6.3"),start.at=NULL), will be converted to (2 4 6.3) etc, see the example
#' @family data transformation functions
#' @export
#' @seealso \code{\link{ez.num}}
ez.2value = function(x, col=NULL, start.at=0, keep.labels=TRUE, ...){
    if (is.data.frame(x) & !is.null(col)){
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     x[[col]] = ez.factorelevel(x[[col]])
        #     x[col]=sjmisc_to_value(x[col], start.at=start.at, keep.labels=keep.labels, ...)
        #     result=x
        # }
        x[cols] = lapply(x[cols],function(e,start.at,keep.labels, ...){sjmisc_to_value(ez.factorelevel(e), start.at=start.at, keep.labels=keep.labels, ...)}, start.at=start.at, keep.labels=keep.labels, ...)
        result = x
    } else {
        x=ez.factorelevel(x)
        result=sjmisc_to_value(x, start.at=start.at, keep.labels=keep.labels, ...)
    }
    return(result)
}

#' change factor level order in a df
#' @description does not change factor label; only changes the order of printing out
#' @param x data frame or vector, factor or non-factor
#' @param col a single column name, quoted string, ignored when x is not a data frame
#' @param ord "az","za"--alphabetic;
#' \cr "as"--as is, appearance;
#' \cr c("small","medium","large")--specified level order
#' \cr "col2"    --another column in az    (ignored if x is not a data frame)
#' \cr "col2:az" --another column in az    (ignored if x is not a data frame)
#' \cr "col2:za" --another column in za    (ignored if x is not a data frame)
#' @return returns a new df, factor (non-factor->factor)
#' @note if x df, pass (x,col,ord);  if x not df, pass (x,ord), or (x,ord=)
#' @export
ez.factorder = function(x, col, ord=NULL, print2screen=F){
    if (is.data.frame(x)) {
        df = x
        if (length(col)!=1 | !is.element(col,colnames(df)) | !is.character(col)) stop('Is your col single exisiting character?')
        # [[]] is the programmable form of $
        labels <- sjmisc_get_labels(df[[col]], attr.only = T, include.values = "n")
        varlab <- attr(df[[col]],'label',exact=T)
        if (length(ord)==1) {
            if (!is.factor(df[[col]])) {
                if (print2screen) cat(sprintf('converting %s to factor via factor()...\n',class(df[[col]])))
                df[[col]] = factor(df[[col]])
            }
            if (!is.factor(df[[col]])) {df[[col]] = factor(df[[col]])}
            if (ord=="as"){
                df[[col]] = factor(df[[col]], unique(as.character(df[[col]])))
            } else if (ord=="az") {
                df[[col]] = factor(df[[col]], levels(factor(df[[col]])))
            } else if (ord=="za") {
                df[[col]] = factor(df[[col]], rev(levels(factor(df[[col]]))))
            } else if (ord %in% names(df)) {
                df[[col]] = factor(df[[col]], unique(df[order(df[[ord]]),col]))
            } else if (grepl(":",ord,fixed=TRUE)) {
                # remove spaces
                ord = gsub(" ","",ord,fixed=TRUE)
                decreasing = strsplit(ord,":")[[1]][2]
                ord = strsplit(ord,":")[[1]][1]
                if (decreasing=="az") {
                    decreasing=FALSE
                } else if (decreasing=="za") {
                    decreasing=TRUE
                }
                df[[col]] = factor(df[[col]], unique(df[order(df[[ord]],decreasing=decreasing),col]))
            }
        } else {
            df[[col]]= factor(df[[col]],ord)
        }
        lvls <- levels(df[[col]])
        labels <- labels[lvls]
        df[[col]] <- sjmisc_set_labels(df[[col]], labels, force.labels = T)
        attr(df[[col]],'label') <- varlab
        return(df)

    } else {
        if (is.null(ord)) ord = col
        labels <- sjmisc_get_labels(x, attr.only = T, include.values = "n")
        varlab <- attr(x,'label',exact=T)
        if (!is.factor(x)) {
            if (print2screen) cat(sprintf('converting %s to factor via factor()...\n',class(x)))
            x = factor(x)
        }
        if (length(ord)==1) {
            if (ord=="as"){
                x = factor(x, unique(as.character(x)))
            } else if (ord=="az") {
                x = factor(x, levels(factor(x)))
            } else if (ord=="za") {
                x = factor(x, rev(levels(factor(x))))
            }
        } else {
            x= factor(x,ord)
        }
        lvls <- levels(x)
        labels <- labels[lvls]
        x <- sjmisc_set_labels(x, labels, force.labels = T)
        attr(x,'label') <- varlab
        return(x)
    }
}

#' change factor level names in a df
#' @param x data frame or vector, factor or non-factor
#' @param col a single column name, quoted string, ignored when x is not a data frame
#' @param orn new level names coresponding to levels(x), eg, c("one","two","three")
#' @return returns a new df, factor (non-factor->factor)
#' @note if x df, pass (x,col,orn);  if x not df, pass (x,orn), or (x,orn=). Because of the change, will remove value labels attr
#' @references \href{http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/}{Cookbook R: Renaming levels of a factor}
#' @seealso \code{\link{ez.recode}}, \code{\link{ez.recode2}}, \code{\link{ez.replace}}, \code{\link{ez.replacewhen}}, \code{\link{ez.2label}}, \code{\link{ez.factorname}}, \code{\link{ez.strreplace}}, \code{\link{ez.strrev}}, \code{\link{ez.regexprep}}, \code{\link{ez.regexprepi}}
#' @export
ez.factorname = function(x, col, orn=NULL, print2screen=T){
    if (is.data.frame(x)) {
        df = x
        if (length(col)!=1 | !is.element(col,colnames(df)) | !is.character(col)) stop('Is your col single exisiting character?')

        varlab <- attr(df[[col]],'label',exact=T); labs=ez.getlabels(df[[col]]); firstTwo=df[[col]][1:2]
        if (!is.factor(df[[col]])){
            if (print2screen) cat(sprintf('converting %s to factor via factor()...\n',class(df[[col]])))
            df[[col]] = factor(df[[col]])
        }

        labs=labs[levels(df[[col]])]
        df[[col]] = factor(df[[col]])  # factor() removes value labels, if exist
        levels(df[[col]]) = orn
        if (print2screen) {
            labs = paste0(names(labs), '[', labs, ']')
            labs = sprintf('%-36s',labs)
            cat('Renamed level names: \n')
            cat(paste0(labs, ' --> ', orn, collapse = '\n'), '\n')
            cat('\nFirst two items:\n')
            firstTwo=sprintf('%-36s',paste0(firstTwo,collapse=', '))
            cat(paste0(firstTwo, ' --> ', paste0(df[[col]][1:2], collapse = ', '), '\n'))
        }
        attr(df[[col]],'label') <- varlab
        return(df)

    } else {
        if (is.null(orn)) orn = col
        varlab <- attr(x,'label',exact=T); labs=ez.getlabels(x); firstTwo=x[1:2]
        if (!is.factor(x)){
            if (print2screen) cat(sprintf('converting %s to factor via factor()...\n',class(x)))
            x = factor(x)
        }

        labs=labs[levels(x)]
        x = factor(x)   # factor() removes value labels, if exist
        levels(x) = orn
        if (print2screen) {
            labs = paste0(names(labs), '[', labs, ']')
            labs = sprintf('%-36s',labs)
            cat('Renamed level names: \n')
            cat(paste0(labs, ' --> ', orn, collapse = '\n'), '\n')
            cat('\nFirst two items:\n')
            firstTwo=sprintf('%-36s',paste0(firstTwo,collapse=', '))
            cat(paste0(firstTwo, ' --> ', paste0(x[1:2], collapse = ', '), '\n'))
        }
        attr(x,'label') <- varlab
        return(x)
    }
}

#' reset factor levels
#' @description reset factor levels in a factor, df (all) cols after its levels have been modified (eg, after using dplyr::filter)
#' relevel a factor in order to reflect its new levels
#' does not change factor label (set factor order as is)
#' has not effect on (ie, make no change to) a non-factor object
#' @param x data frame or vector, factor
#' @param cols column name(s) to eval('dplyr::select()'); ignored when x is not a data frame. NULL=all cols
#' @return returns a new df, factor, vector (has no effect on (ie, make no change to) a non-factor object)
#' @export
ez.factorelevel = function(x, cols=NULL, print2screen=F) {
    if (is.factor(x)) {
        # for nonfactor, length(levels(x)) returns 0
        if (length(levels(x))!=length(levels(factor(x,unique(as.character(x)))))) {
            if (print2screen) cat(sprintf('resetting factor levels for factor %s...\n',deparse(substitute(x))))
            varlab <- attr(x,'label',exact=T)
            labs = ez.getlabels(x)
            x = factor(x, unique(as.character(x)))
            attr(x,'label') <- varlab
            x = ez.setlabels(x,labs)
        }
    } else if (is.data.frame(x) & !is.null(cols)) {
        cols=ez.selcol(x,cols)
        # for (col in cols) { x[[col]] = ez.factorelevel(x[[col]]) }
        x[cols] = lapply(x[cols],function(e,print2screen){ez.factorelevel(e,print2screen=print2screen)},print2screen=print2screen)
    } else if (is.data.frame(x) & is.null(cols)) {
        # x = dplyr::mutate_all(x,ez.factorelevel)
        # mutate gets rid of variable labels
        x[] = lapply(x,ez.factorelevel,print2screen=print2screen)
    }
    return(x)
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
#' @description Recodes one single according to a set of rules. Recommends for numeric (single value or range change)
#' \cr\cr ez.recode replaces the original var with recoded var;
#' \cr ez.recode2 saves orignal var as var_ori, and then recodes var
#' \cr see also \code{\link{ez.replace}}, recommends for numeric (single value change), characters, factors
#' \cr keep data type whenever possible, remove value labels attr of col (otherwise could be inconsistent), but variable label is kept for numeric, character, factors etc.
#' @param df data.frame to be recoded
#' @param col the name of var to be recoded, must be a string in quotes ""
#' @param recodes Definition of the recoding rules. See details
#' @details recodes contains a set of recoding rules separated by ";". There are several different types of recoding rules:
#' \itemize{
#'  \item The simplest codes one value to another. If we wish to recode 1 into 2, we could use the rule "1=2;".
#'  \item A range of values can be coded to a single value using "1:3=4;". This rule would code all values between 1 and 3 inclusive into 4. For factors, a value is between two levels if it is between them in the factor ordering. One sided ranges can be specified using the lo and hi key words (e.g."lo:3=0; 4:hi=1"). hi=Hi=HI=max, lo=Lo=LI=min, :=thru=Thru=THRU (mimic SPSS recode syntax)  -> can replace = as well. if multiple ranges overlap, the latter one prevails. 1:3=1;3:5=2 (3->2 finally).
#'  \item Default conditions can be coded using "else." For example, if we wish to recode all values >=0 to 1 and all values <0 to missing, we could use ("0:hi=1; else=NA"). the \code{"else"}-token should be the last argument in the \code{recodes}-string.
#'   \item Variable label attributes (see, for instance, \code{\link{get_label}}) are preserved if exists, however, value label attributes are removed (makes sense, right)
#'   \item the \code{\link{sjmisc_rec}} function in sjmisc does not work well with double numbers (eg, 3.59)
#' }
#' \cr
#' \cr ====================================================================================
#' \cr recommends \code{\link{ez.replace}} to change characters, factors
#' \cr ====================================================================================
#' \cr Works with characters/factors as well e.g., ('Gr',"'U1'='U';'U2'='U';'R1'='R';'R2'='R'")
#' \cr characters to number does not work directly e.g., ('Gr',"'U1'=2;'U2'=3")  --> 2, 3 are converted to "2", "3" (char of number)
#' \cr but number to character works directly, char->char, factor->factor
#' \cr for factors, no need to reset levels (auto reset)
#' \cr The conclusion is: numeric<->numeric without quote
#' \cr but if newval is quoted character, then numeric->char, char->char, factor->factor
#' \cr See the example section for more detail.
#'
#' @author Jerry Zhu modified from Ian Fellows (pkg Deducer) adapted from code by John Fox (car)
#' @examples
#' data<-data.frame(a=rnorm(100),b=rnorm(100),male=rnorm(100)>0)
#' ez.recode(data, "a", "hi = 1")
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
ez.recode = function(df, col, recodes){
    if (length(col)!=1 | !is.element(col,colnames(df)) | !is.character(col)) stop('col not valid!')
    varName=col

    recodes = gsub("min","Lo",recodes,fixed=TRUE)
    recodes = gsub("max","Hi",recodes,fixed=TRUE)
    recodes = gsub("Min","Lo",recodes,fixed=TRUE)
    recodes = gsub("Max","Hi",recodes,fixed=TRUE)
    recodes = gsub("MIN","Lo",recodes,fixed=TRUE)
    recodes = gsub("MAX","Hi",recodes,fixed=TRUE)
    recodes = gsub("LO","Lo",recodes,fixed=TRUE)
    recodes = gsub("HI","Hi",recodes,fixed=TRUE)
    recodes = gsub("lo","Lo",recodes,fixed=TRUE)
    recodes = gsub("hi","Hi",recodes,fixed=TRUE)
    recodes = gsub("=","->",recodes,fixed=TRUE)
    recodes = gsub(" thru ",":",recodes,fixed=TRUE)
    recodes = gsub(" THRU ",":",recodes,fixed=TRUE)
    recodes = gsub(" Thru ",":",recodes,fixed=TRUE)

    newVar = .recode_helper(df[varName],recodes)
    # the helper function changes column name to sth like recode.other.variable.
    # now change it back
    names(newVar) = varName

    # remove value labels attr, with graceful failure
    newVar=tryCatch(sjmisc_set_labels(newVar,""), error=function(e) newVar, warning = function(w) newVar, finally=newVar)

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

#' @rdname ez.recode
#' @export
ez.recode2 = function(df, col, recodes){
    if (length(col)!=1 | !is.element(col,colnames(df)) | !is.character(col)) stop('col not valid!')
    varName=col

    recodes = gsub("min","Lo",recodes,fixed=TRUE)
    recodes = gsub("max","Hi",recodes,fixed=TRUE)
    recodes = gsub("Min","Lo",recodes,fixed=TRUE)
    recodes = gsub("Max","Hi",recodes,fixed=TRUE)
    recodes = gsub("MIN","Lo",recodes,fixed=TRUE)
    recodes = gsub("MAX","Hi",recodes,fixed=TRUE)
    recodes = gsub("LO","Lo",recodes,fixed=TRUE)
    recodes = gsub("HI","Hi",recodes,fixed=TRUE)
    recodes = gsub("lo","Lo",recodes,fixed=TRUE)
    recodes = gsub("hi","Hi",recodes,fixed=TRUE)
    recodes = gsub("=","->",recodes,fixed=TRUE)
    recodes = gsub(" thru ",":",recodes,fixed=TRUE)
    recodes = gsub(" Thru ",":",recodes,fixed=TRUE)
    recodes = gsub(" THRU ",":",recodes,fixed=TRUE)

    newVar = .recode_helper(df[varName],recodes)
    # the helper function changes column name to sth like recode.other.variable.
    # now change it back
    names(newVar) = varName
    # remove all value labels attr, with graceful failure
    newVar=tryCatch(sjmisc_set_labels(newVar,""), error=function(e) newVar, warning = function(w) newVar, finally=newVar)
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
#' \cr keep data type whenever possible, var label and value labels are also kept, but notice that value labels might be incomplete because of the insertion of new unlabelled value
#' @details smilar to \code{\link{ez.recode}}num->num (if get replaced with another num), numeric->char (if get replaced with a char), char->char, factor->factor (factor internally converted to char then back to factor)
#' \cr wrapper of df[[col]][which(df[[col]]==oldval)] <- newval
#' \cr a=c(1,2,3); a[which(a=='usb')] <-'cup'; a    # the assign of a char (even though no match) will change a to char of "1" "2" "3"!
#' \cr a=c(1,2,3); a[which(a==4)] <-'cup'; a        # a changes a to char as well
#' \cr a=c(1,2,3); a[which(a==4)] <-3.1; a          # a changes a to double
#' \cr a=c(1,'2',3); a[which(a==1)] <-4; a          # here a=c(1,'2',3)->c('1','2','3'), then '1'==1 TRUE, because 1 converted to '1', finally 4 converted to '4' assigned
#' \cr a=c(1,2,3); a[which(a=='1')] <-4; a          # same logic, but the returned a is still numeric
#' \cr Thus, the conclusion is: alawys better to not quote numbers. It is not compatible, could auto convert.
#' \cr bottom line: the new val determines outcome even without match
#' \cr But my script protects that; data type remains unchanged if there is no match
#' \cr logic is tricky TRUE=='TRUE', FALSE=='FALSE' return TRUE; always convert them first to num, eg, mutate(preterm=as.integer(delivery_ega<37.0)) and then start from there.
#' @param df data frame
#' @param col column name evaluated by eval('dplyr::select()'), can be single, or multiple/vector eg, c('col1','col2'). If skipped (not provided), all columns used
#' @param oldval old value (e.g., -Inf, NA), can only be single, not multiple/vector. Note would not differentiate 5.0 and 5
#' @param newval new value (e.g., NA), can only be single, not multiple/vector
#' @return returns a new df, old one does not change
#' @family data transformation functions
#' @export
#' @note when 4 parameters provided, it is recognized as (df,col,oldval,newval)
#' \cr when 3 parameters provided, it is recognized as (df,oldval,newval)
#' see example
#' @seealso \code{\link{ez.recode}}, \code{\link{ez.recode2}}, \code{\link{ez.replace}}, \code{\link{ez.replacewhen}}, \code{\link{ez.2label}}, \code{\link{ez.factorname}}, \code{\link{ez.strreplace}}, \code{\link{ez.strrev}}, \code{\link{ez.regexprep}}, \code{\link{ez.regexprepi}}
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
#' data=iris[1:10,]; data[1,2]=NA; data[2,5]=NA; data[4,'Species']='versicolor'; data[6,'Species']='virginica'; data['TV']='COBY'
#' ez.replace(data,c('Sepal.Width','Petal.Length','Petal.Width','Species'),NA,3.1415)
#' ez.replace(data,NA,3.1415)
#'           # Species was factor, now is numeric (factor->numeric)
#' ez.replace(data,NA,'replaced')
#'           # Sepal.Width was numeric, now is char
#'           # Species was factor, now is char (factor->char of num)
#' ez.replace(data,5.1,3.14)
#'           # Sepal.Length was numeric, now is still numeric
#' ez.replace(data,'TV','COBY','Mac')
#'           # TV was char, now is still char
#' ez.replace(data,'COBY','Mac')
#'           # TV was char, now is still char
#' ez.replace(data,'setosa','flower')
#'           # Species was factor, now is still factor (notice, levels changed)
ez.replace = function(df, col, oldval, newval=NULL, print2screen=T){
    # four parameters passed
    if (!is.null(newval)) {
        col=(ez.selcol(df,col))
        cols = col
        # for (col in cols) {
        #     # for factor, you cannot directly assign if the new val is not alredy in the levels, otherwise get "invalid factor level, NA generated"
        #     factored = ifelse(is.factor(df[[col]]), TRUE, FALSE)
        #     if (is.factor(df[[col]])) {df[[col]]=as.character(df[[col]])}

        #     if (is.na(oldval)) {
        #         if (length(which(is.na(df[[col]]))) > 0) {
        #             if (print2screen) cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', sum(is.na(df[[col]])), col, as.character(oldval), as.character(newval)))
        #             df[[col]][which(is.na(df[[col]]))] <- newval
        #         }
        #     } else {
        #         if (length(which(df[[col]]==oldval)) > 0) {
        #             if (print2screen) cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', length(which(df[[col]]==oldval)), col, as.character(oldval), as.character(newval)))
        #             df[[col]][which(df[[col]]==oldval)] <- newval
        #         }
        #     }

        #     if (factored) {df[[col]]=as.factor(df[[col]])}
        #     # remove all value labels attr, with graceful failure
        #     df[[col]]=tryCatch(sjmisc_set_labels(df[[col]],""), error=function(e) df[[col]], warning = function(w) df[[col]], finally=df[[col]])
        # }

        thefun = function(dfcol,oldval,newval) {
            lab = ez.getlabel(dfcol); labs = ez.getlabels(dfcol)
            # for factor, you cannot directly assign if the new val is not alredy in the levels, otherwise get "invalid factor level, NA generated"
            factored = ifelse(is.factor(dfcol), TRUE, FALSE)
            if (is.factor(dfcol)) {dfcol=as.character(dfcol)}

            if (is.na(oldval)) {
                if (length(which(is.na(dfcol))) > 0) {
                    if (print2screen) cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', sum(is.na(dfcol)), col, as.character(oldval), as.character(newval)))
                    dfcol[which(is.na(dfcol))] <- newval
                }
            } else {
                if (length(which(dfcol==oldval)) > 0) {
                    if (print2screen) cat(sprintf('%5.0f values replaced in column %s (%s -> %s)\n', length(which(dfcol==oldval)), col, as.character(oldval), as.character(newval)))
                    dfcol[which(dfcol==oldval)] <- newval
                }
            }

            if (factored) {dfcol=as.factor(dfcol)}
            dfcol = ez.setlabel(dfcol,lab); dfcol = ez.setlabels(dfcol,labs)
            return(dfcol)
        }
        df[cols] = lapply(df[cols],thefun,oldval=oldval,newval=newval)

    # three parameters passed
    } else {
        # trick to recognize parameters
        newval=oldval;oldval=col
        if (is.na(oldval)) {
            if (print2screen) cat(sprintf('%5.0f values replaced in data frame (%s -> %s)\n', sum(colSums(is.na(df))), as.character(oldval), as.character(newval)))
            # the dot here, I think, refers to each column, not related to . for %>%
            # mutate() will somehow auto convert columns of factor but in a bad way. Use my own function to convert factor to char
            # df = dplyr::mutate_all(df,funs(ifelse(is.na(.),newval,.)))
        } else {
            if (print2screen) cat(sprintf('%5.0f values replaced in data frame (%s -> %s)\n', sum(colSums(df==oldval,na.rm=TRUE)), as.character(oldval), as.character(newval)))
            # df = dplyr::mutate_all(df,funs(ifelse(.==oldval,newval,.)))
        }
        # recursive call, but suppress output
        # https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r
        # notice the invisible(capture.output(expr)) does not work within a function which returns some value and you want to use this value
        allcols=colnames(df)
        sink("/dev/null")
        df = ez.replace(df,allcols,oldval,newval)
        sink()
    }
    return(df)
}

#' replace when
#' @description replace a df: eg, when pt_num=1220, let baby_num=3,baby_name='Bennnnnnn'
#' \cr keep data type whenever possible, var label and value labels are also kept, but notice that value labels might be incomplete because of the insertion of new unlabelled value
#' @details smilar to \code{\link{ez.recode}}num->num (if get replaced with another num), numeric->char (if get replaced with a char), char->char, factor->factor (factor internally converted to char then back to factor)
#' \cr wrapper of df[[col]][theRow] <- newval
#' \cr df[theRow,col]=newval  # this syntax works also, but df[145:146,2,drop=F]=4 says unused arg drop=F
#' \cr whether the number in pt_num=1220 should be quoted or not, see \code{\link{ez.replace}} for comparison logic details. The conclusion is: alawys better to not quote numbers.
#' @param df df
#' @param ... pt_num=1220,baby_num=3,baby_name='Bennnnnnn',the first element is used as condition to pinpoint the row(s) (multiple matched rows allowed), the rest as cols to be replaced. Quotes around col names are optional, 'pt_num'=1220.
#' @return returns a new df
#' @seealso \code{\link{ez.replace}}
#' @examples df=ez.replacewhen(nicu,pt_num=1220,baby_num=3,baby_name='Ben')
#' @export
ez.replacewhen = function(df, print2screen=T, ...) {
    theList = list(...)
    theCols = names(theList)
    theID = theCols[1]; theValue = theList[[1]]
    theRow = which(df[theID]==theValue)
    # replace
    if (length(theRow) > 0) {
        for (i in 2:length(theCols)) {
            col=theCols[i]; newval=theList[[i]]; oldval=df[[col]][theRow]

            # prepare
            toReplace=FALSE
            if (is.na(newval)) {
                if (length(which(!is.na(oldval))) > 0) {
                    toReplace = TRUE
                    theString = sprintf('%d row(s) replaced when %s=%s in column %s (%s -> %s)',length(which(!is.na(oldval))),theID,toString(theValue),col,toString(oldval[which(!is.na(oldval))]),toString(newval))
                    theRow2 = which( df[theID]==theValue & !is.na(df[[col]]) )
                }
            } else {
                # when oldval = NA, != returns NA
                # https://stackoverflow.com/questions/16822426/r-dealing-with-true-false-na-and-nan
                if (length(which( (oldval!=newval) %in% c(TRUE,NA) )) > 0) {
                    toReplace = TRUE
                    theString = sprintf('%d row(s) replaced when %s=%s in column %s (%s -> %s)',length(which((oldval!=newval) %in% c(TRUE,NA))),theID,toString(theValue),col,toString(oldval[which((oldval!=newval) %in% c(TRUE,NA))]),toString(newval))
                    theRow2 = which( df[theID]==theValue & ((df[[col]]!=newval) %in% c(TRUE,NA)) )
                }
            }

            if (toReplace) {
                if (length(theRow2)==1) {
                    if (print2screen) ez.print(theString)
                } else if (length(theRow2)>1) {
                    if (print2screen) ez.pprint(theString)
                }

                lab = ez.getlabel(df[[col]]); labs = ez.getlabels(df[[col]])
                factored = ifelse(is.factor(df[[col]]), TRUE, FALSE)
                if (is.factor(df[[col]])) {df[[col]]=as.character(df[[col]])}
                # df[theRow,col]=newval  # this syntax works also, but df[145:146,2,drop=F]=4 says unused arg drop=F
                df[[col]][theRow2] <- newval
                if (factored) {df[[col]]=as.factor(df[[col]])}
                df[[col]] = ez.setlabel(df[[col]],lab); df[[col]] = ez.setlabels(df[[col]],labs)
            }
        } # end for
    } # end if
    return(df)
}

#' Count the occurrence of a single value in data frame columnwise, or rowwise, or both
#' @description count within one or more than one columns/rows, or entire data frame (ie, all columns/rows)
#' @param x data frame or vector, if vector, parameters col, dim are ignored
#' @param val value to be counted, could be NA. Note, may not differentiate 5.0 and 5, ie. 5.0==5 TRUE
#' @param col column evaluated by eval('dplyr::select()'), single or vector. If NULL, all columns used
#' @param dim 1=along row (rowwise), 2=along col (colwse), 3=area, both, grand total (within specified cols/rows)
#' @return returns a data frame, if dim=1/2; a single value if dim=3.
#' \cr vector input x always outputs a single value.
#' @seealso \code{\link{ez.countif}}
#' @examples
#' sx = c("F", "F", "F", "M", "M", "M")
#' ht = c(69, 64, 67, 68, 72, 71)
#' wt = c(148, 132, 142, 149, 167, 165)
#' people = data.frame(sx, ht, wt)
#' ez.count(people,'M',dim=2)
#' @export
ez.count = function(x, val=NA, col=NULL, dim=3) {
    # assume a 1d vector
    if (is.list(x) & !is.data.frame(x)) x=unlist(x,recursive=T)
    # trimws trick to convert to corresponding character vector
    # https://stackoverflow.com/a/53272969/2292993
    if (is.factor(x)) x=trimws(x)
    # note: is.vector does not test if is vector as one would think
    # is.atomic(factor(1:3)) also TRUE
    if (is.atomic(x)) return(ifelse(is.na(val),sum(is.na(x)),sum(x==val,na.rm=TRUE)))

    if (is.data.frame(x) & !is.null(col)) {
        col = (ez.selcol(x,col))
        df = x[col]
    } else {
        df = x
    }

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

#' Conditionally count the occurrence of a single value in data frame columnwise, or rowwise, or both
#' @description count within one or more than one columns/rows, or entire data frame (ie, all columns/rows)
#' @param x data frame or vector, if vector, parameters col, dim are ignored
#' @param cond a string like '.>=3', '.=="M"', 'is.na(.)', 'ifelse()'
#' \cr must use . to refer to each column of data frame (vectorized)
#' \cr must be able to be evaluated as TRUE/FALSE (essentially sum up TRUE)
#' \cr wrapper of mutate_all(funs(cnd)), so the same syntax
#' \cr Note, may not differentiate 5.0 and 5, ie. 5.0==5 TRUE
#' \cr > < etc, not meaningful for factors, return NA
#' \cr na.rm=FALSE to catch NA returned above
#' @param col column evaluated by eval('dplyr::select()'), single or vector. If NULL, all columns used
#' @param dim 1=along row (rowwise), 2=along col (colwse), 3=area, both, grand total (within specified cols/rows)
#' @return returns a data frame, if dim=1/2; a single value if dim=3.
#' \cr vector input x always outputs a single value.
#' @seealso \code{\link{ez.countif}}
#' @examples
#' sx = c("F", "F", "F", "M", "M", "M")
#' ht = c(69, 64, 67, 68, 72, 71)
#' wt = c(148, 132, 142, 149, 167, 165)
#' people = data.frame(sx, ht, wt)
#' ez.countif(people,'.=="M"','wt', dim=2)  # wt 0
#' ez.countif(people,'.>150', dim=2)   # sx ht wt   NA  0  2
#' ez.countif(people$wt, '.==165')
#' @export
ez.countif = function(x, cnd, col=NULL, dim=3, na.rm=FALSE) {
    # assume a 1d vector, convert to data frame for easy processing
    if (!is.data.frame(x)) {
        x=data.frame(x)
        col=NULL
        dim=3
    }

    if (is.data.frame(x) & !is.null(col)) {
        col = (ez.selcol(x,col))
        df = x[col]
    } else {
        df = x
    }

    # warning for factor > < etc, not meaningful for factors, return NA
    # na.rm=FALSE to catch NA returned above
    cmd=sprintf('tmpMatrix=suppressWarnings(dplyr::mutate_all(x, funs(%s)))',cnd)
    eval(parse(text = cmd))
    if (dim==3) {
        return(sum(rowSums(tmpMatrix,na.rm=na.rm)))
    } else if (dim==1) {
        sumNamedVector=rowSums(tmpMatrix,na.rm=na.rm)
        return(data.frame(count=sumNamedVector))
    } else if (dim==2) {
        sumNamedVector=colSums(tmpMatrix,na.rm=na.rm)
        return(data.frame(as.list(sumNamedVector)))
    }
}

#' reorder all cols, or sort all cols alphabetically
#' @description reorder all cols, or sort all cols alphabetically
#' @param newColOrder c('','',''), number of cols must match that of all cols (when para col=NULL) or specified by para col.
#' or, newColOrder='az' or 'za', sort all cols alphabetically
#' @param col NULL=all columns, otherwise restricted to specified cols, eg, ( internally evaluated by eval('dplyr::select()') )
#' \cr 'c(sample_num,mother_num)' (quoted) or c("sample_num","mother_num") (not quoted)
#' \cr 1:4 (not quoted)
#' \cr 'col1:col3' (quoted)
#' \cr '-(ABCB1_c1236t:pgp_rs2032582)', '-c(neonate_admit_NICU,BDNF)' (quoted)
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
ez.recols = function(df,newColOrder,col=NULL){
    # http://rrubyperlundich.blogspot.com/2011/06/r-sorting-vectors-sort-vs-order.html
    # the result of sort() is a vector consisting of elements of the original (unsorted) vector
    # order(), we get a vector with the ordered indices of the original (unsorted) vector
    if (is.null(col)) {
        # use all(), because newColOrder might be a vector
        # na.last=FALSE, makes na appears first, here it does not matter, because col names should not be NA
        if(all(newColOrder=='az')) newColOrder=order(colnames(df), na.last = FALSE, decreasing = FALSE)
        if(all(newColOrder=='za')) newColOrder=order(colnames(df), na.last = FALSE, decreasing = TRUE)
        if (length(newColOrder)!=length(colnames(df))) stop('new col names length mismatches old one')
    } else {
        names_all = colnames(df)
        names_sel = (ez.selcol(df,col))
        selected = is.element(names_all,names_sel)
        names_not_sel = names_all[!selected]

        if(all(newColOrder=='az')) newColOrder=sort(names_sel, na.last = FALSE, decreasing = FALSE)
        if(all(newColOrder=='za')) newColOrder=sort(names_sel, na.last = FALSE, decreasing = TRUE)
        if (length(newColOrder)!=length(names_sel)) stop('new col names length mismatches old one')
        newColOrder = c(names_not_sel,newColOrder)
    }
    return(df[newColOrder])
}

#' reorder a single col (sort of, see below), alias of \code{\link{ez.move}}
#' @description reorder a single col (sort of, see below), alias of \code{\link{ez.move}}
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
#' @description reorder a single col (sort of, see below), alias of \code{\link{ez.recol}}
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

#' rename all cols, see also \code{\link{ez.rncol}}
#' @description rename all cols, see also \code{\link{ez.rncol}}
#' @param newColName c('','',''), number of cols must match
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
ez.rncols = function(df,newColNames){
    if (length(colnames(df))!=length(newColNames)) stop('col length not match')
    names(df) = newColNames
    return(df)
}

#' convert all column names to lower case
#' @description convert all column names to lower case
#' @param df a data frame
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
ez.2lower = function(df){
    result = df
    names(result) = tolower(names(result))
    return(result)
}

#' rename a single or many col, see also \code{\link{ez.rncols}}
#' @description alias of \code{\link[reshape]{rename}} \code{\link{ez.rename}}
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName"), c(wt = "weight", cyl = "cylinders")
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
ez.rncol = reshape::rename

#' rename a single or many col
#' @description alias of \code{\link[reshape]{rename}} \code{\link{ez.rncol}}
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName"), c(wt = "weight", cyl = "cylinders")
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
ez.rename = reshape::rename

#' create a new col, may use \code{\link[dplyr]{mutate}} instead
#' @param newColName ''
#' @param defaultVal NA (default)
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
ez.newcol = function(df, newColName, defaultVal=NA){
    df[newColName] = defaultVal
    return(df)
}

#' alias of \code{\link[dplyr]{mutate}}
#' @description alias of \code{\link[dplyr]{mutate}}
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
#' @description alias of \code{\link[dplyr]{filter}}
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
#' @description alias of \code{\link[dplyr]{arrange}}
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
#' @description alias of \code{\link[dplyr]{distinct}}
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

#' duplicated
#' @description find the duplicated rows/cols in a data frame or duplicated elements in a vector of any data type (factor, char, numeric)
#' \cr ez.notduplicated is not the same as unique, but unique/distinct minus any of the duplicated
#' @param x a data frame or a vector/col of any data type (factor, char, numeric)
#' @param col restrict to the columns where you would like to search for duplicates, evaluated by eval('dplyr::select()'); e.g., 3, c(3), 2:5, "place", c("place","age")
#' \cr if x is a data frame, col is specified (e.g., "cond"), check that col only
#' \cr if x is a data frame, col is unspecified (i.e., NULL default), check all cols in x
#' \cr if x is not a data frame, col is ignored
#' @param vec TRUE/FALSE, if TRUE, returns a vector of TRUE/FALSE indicating duplicates;
#' \cr if FALSE, returns a df with one column 'Duplicated' of TRUE/FALSE
#' \cr This is useful for binding with other data frames
#' \cr T/F could be replaced by 0,1,2, see vecgroup
#' @param vecgroup TRUE/FALSE, if TRUE, returns a vector of 0,1,2 indicating duplicates, where
#' \cr 0=F, no duplicates; 1=duplicates group 1; 2=duplicates group 2, etc
#' @param value TRUE/FALSE, if TRUE, returns actual duplicated values, instead of logicals.
#' The returned data type is the same as the original (data frame->data frame, factor->factor, etc, because only slicing based on logicals).
#' Ignore/Overwrite vec, vecgroup.
#' @param keepall TRUE/FALSE, only applicable when value=T (otherwise ignored). When col is specified, value only returns for that col. Use keepall=T to return all cols in input df
#' @param dim 1=find duplicated rows, 2=find duplicated cols. dim has no effect when x is a vector
#' @param incomparables a vector of values that cannot be compared. FALSE is a special value, meaning that all values can be compared,
#' \cr and may be the only value accepted for methods other than the default. It will be coerced internally to the same type as x.
#' \cr not applicable to data.frame x (see https://stackoverflow.com/a/29730485/2292993), but ok for vector x
#' @return return depends, see vec above (By default, missing values are regarded as equal, to avoid that, pass incomparables=NA)
#' \cr this is different from the built-in R \code{\link{duplicated}}
#' \cr x <- c(1, 1, 4, 5, 4, 6)  duplicated(x) returns [1] FALSE TRUE FALSE FALSE TRUE FALSE
#' \cr but ez.duplicated(x) returns [1] TRUE TRUE TRUE FALSE TRUE FALSE
#' \cr Also, the function has a trick, so that duplicated cols could be checked, while the native duplicated cannot directly apply to cols. See https://stackoverflow.com/questions/9818125/
#' @examples
#' c(2,2,3) %>% data.frame(col=.) %>% ez.duplicated(incomparables = 4)  # error
#' c(2,2,3) %>% ez.duplicated(incomparables = 4)  # OK  note that 4 is not even an element of the vector
#' @export
ez.duplicated = function(x, col=NULL, vec=TRUE, vecgroup=FALSE, dim=1, incomparables=FALSE, value=FALSE, keepall=TRUE, ...){
    xinput = x

    if (is.data.frame(x) & !is.null(col)) {
        # R converts a single row/col to a vector if the parameter col has only one col
        # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
        col=(ez.selcol(x,col))
        x = x[,col,drop=FALSE]
    }

    if (is.data.frame(x) & dim==1){
        x = x
    } else if (is.data.frame(x) & dim==2) {
        # trick from https://stackoverflow.com/a/33552742/2292993
        xx=x
        x = as.list(x)
        # as.list applicable when x input is a vector as well, but vector x will not go through here
        # as.list() not the same as list()
    }

    # # https://stackoverflow.com/a/29730485/2292993
    # # potential hack for incomparables for data frame, not gonna use, better to have error
    # if (is.data.frame(x)) incomparables=FALSE
    # https://stackoverflow.com/a/7854620/2292993
    result = duplicated(x,incomparables=incomparables, ...) | duplicated(x, fromLast=TRUE, incomparables=incomparables, ...)

    # hack to group duplicates somehow, such that ('a','b','c','b','d','a') -> (1,2,0,2,0,1)
    if (vecgroup) {
        result = rep(0,length(result))
        # get d, duplicated elements
        d = duplicated(x, fromLast=TRUE, incomparables=incomparables, ...)
        d = base::unique(x[which(d)])

        if (length(d)>0) {
            # convert x to character vector for easy manipulation
            if (is.data.frame(x) & dim==1) {
                y = sapply(data.table::transpose(x),paste,collapse='')
                d = sapply(data.table::transpose(d),paste,collapse='')
            } else {
                # sapply works for vector, list, data frame colwise
                y = sapply(x,paste,collapse='')
                d = sapply(d,paste,collapse='')
            }

            for (i in 1:length(d)) {
                result[which(y==d[[i]])] = i
            }
        }
    }

    if (value==TRUE) {
        if (keepall) {xx=xinput;x=xinput}

        if (is.list(x) & dim==2) {
            result=xx[which(as.logical(result))]
        } else if (is.data.frame(x) & dim==1) {
            result=x[which(as.logical(result)),,drop=FALSE]
        } else {
            result=x[which(as.logical(result))]
        }
    } else if (!vec) {
        result = data.frame('Duplicated'=result)
    }

    return(result)
}

#' @rdname ez.duplicated
#' @export
ez.notduplicated = function(x, col=NULL, vec=TRUE, dim=1, incomparables=FALSE, value=FALSE, keepall=TRUE, ...){
    xinput = x

    if (is.data.frame(x) & !is.null(col)) {
        # R converts a single row/col to a vector if the parameter col has only one col
        # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
        col=(ez.selcol(x,col))
        x = x[,col,drop=FALSE]
    }

    if (is.data.frame(x) & dim==1){
        x = x
    } else if (is.data.frame(x) & dim==2) {
        # trick from https://stackoverflow.com/a/33552742/2292993
        xx=x
        x = as.list(x) # as.list applicable when x input is a vector as well, but vector x will not go through here
    }

    # # https://stackoverflow.com/a/29730485/2292993
    # # potential hack for incomparables for data frame, not gonna use, better to have error
    # if (is.data.frame(x)) incomparables=FALSE
    # https://stackoverflow.com/a/7854620/2292993
    result = duplicated(x,incomparables=incomparables, ...) | duplicated(x, fromLast=TRUE, incomparables=incomparables, ...)
    result = !result

    if (value==TRUE) {
        if (keepall) {xx=xinput;x=xinput}

        if (is.list(x) & dim==2) {
            result=xx[which(result)]
        } else if (is.data.frame(x) & dim==1) {
            result=x[which(result),,drop=FALSE]
        } else {
            result=x[which(result)]
        }
    } else if (!vec) {
        result = data.frame('NotDuplicated'=result)
    }

    return(result)
}

#' alias of \code{\link[dplyr]{group_by}}
#' @description alias of \code{\link[dplyr]{group_by}}
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
#' @description alias of \code{\link[dplyr]{left_join}}
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
#' @description delete/remove one or many cols, may use \code{\link[dplyr]{select}} instead, alias of \code{\link{ez.del}} \code{\link{ez.delete}} \code{\link{ez.rmcol}} \code{\link{ez.rmcols}}
#' @param cols evaluated by eval('dplyr::select()'), sth like 'Month' or c('Month','Day'). If not existing in df, nothing happens. Special situations: If NULL, auto delete/remove cols that are all empty or NAs. If a single decimal number less than 1 (NOT 1), any column whose non-missing rate less than the specified number will be removed (eg, cols=0.85, keep >=0.85; or cols=0.9999, remove <0.9999, essentially keep only completed cols)
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
ez.del = function(df,cols=NULL,print2screen=T){
    if (is.null(cols)) {
        # cols all empty
        # convert  all cols to string first, in order to compare with ""
        tmp = data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
        colNumsAllEmpty = as.vector(which(colSums(tmp=="") == nrow(tmp)))
        # https://stackoverflow.com/a/29269139/2292993
        colNumsAllNAs = as.vector(which(colSums(is.na(df)) == nrow(df)))

        colNumsAllTogether = dplyr::union(colNumsAllNAs,colNumsAllEmpty)
        if (length(colNumsAllTogether)>0) {
            if (print2screen) {
                cat(sprintf( '%s\n\n%d cols removed that contain all empty or NAs\n', toString(colnames(df)[colNumsAllTogether]), length(colNumsAllTogether) ))
            }
            df = dplyr::select(df,-colNumsAllTogether)
        }
    } else if (length(cols)==1 && is.numeric(cols) && !is.integer(cols) && cols<1) {
        nonMissingRate = colSums(!is.na(df))/nrow(df)
        colNumsLessThan = which(nonMissingRate<cols)

        if (length(colNumsLessThan)>0) {
            if (print2screen) {
                cat(sprintf( '%s\n\n%d cols removed less than the least non-missing rate\n', toString(colnames(df)[colNumsLessThan]), length(colNumsLessThan) ))
            }
            df = dplyr::select(df,-colNumsLessThan)
        }
    } else {
        # col not exist, cannot be selected, just skip
        tryCatch({
            cols=ez.selcol(df,cols)
        },error = function(e) {})
        existCols = cols[(cols %in% colnames(df))]
        if (length(existCols)>0) {
            if (print2screen) {
                cat(sprintf( '%s\n\n%d columns deleted\n',toString(existCols),length(existCols) ))
            }
            df[existCols] = NULL
        }
    }
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
#' @description could also accept a vector/factor as input, if so, then col,n,reindex ignored (factor->char->factor)
#' @param x a data frame, or a vector
#' @param col internally evaluated by eval('dplyr::select()'), restrict to the columns where you would like to search for NA; eg, 3, c(3), 2:5, "place", c("place","age")
#' \cr default is NULL, search for all columns.
#' @param n integer or vector, 0, c(3,5), number/range of NAs allowed.
#' \cr If a number, the exact number of NAs kept
#' \cr Range includes both ends 3<=n<=5
#' \cr Range could be -Inf, Inf
#' @param reindex whether to keep original row index or reindex
#' \cr eg, original row.names() is 1, 2, 3, then drop row 2
#' \cr if not reindex, new index is 1, 3
#' \cr if reindex, new index is 1, 2
#' @param print2screen T/F. if T, print out rows containing NAs dropped (In: -> Out: )
#' @return returns a new df with rows that have NA(s) removed, or a new vector/factor without NAs
#' @export
ez.dropna = function(x, col=NULL, n=0, reindex=TRUE, print2screen=TRUE){
    df=x
    if (is.factor(x)) x=as.character(x)
    if (is.vector(x)) {
        x=x[!is.na(x)]
        if ((length(df)-length(x)>0) & print2screen) cat(sprintf('%d NAs dropped (In: %d -> Out: %d).\n', length(df)-length(x), length(df), length(x)))
        if (is.factor(df)) x=as.factor(x)
        return(x)
    }

    df=x
    nbefore = nrow(df)
    if (!is.null(col)) {
        # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
        # R converts a single col to a vector if the parameter col has only one col, e.g., iris[,'Sepal.Length',drop=F]
        # but iris['Sepal.Length'] will not! (iris['Sepal.Length',drop=F] actually gives a warning!)
        # will not convert a single row, but it does not hurt to add drop=F, e.g., iris[1,,drop=F] OK
        col=(ez.selcol(x,col))
        # df.temp = df[,col,drop=FALSE]
        # slicing with [] would cause to lose attributes information, which in some case are not desired
        df.temp = dplyr::select(df,col)
    } else {
        df.temp = df
    }

    if (length(n)==1){
        if (n==0) {
            # simply call complete.cases which might be faster
            # result = df[complete.cases(df.temp),,drop=FALSE]
            result = dplyr::filter(df,complete.cases(df.temp))
        } else {
            # credit: http://stackoverflow.com/a/30461945/2292993
            log <- apply(df.temp, 2, is.na)
            logindex <- apply(log, 1, function(x) sum(x) == n)
            # result = df[logindex,,drop=FALSE]
            result = dplyr::filter(df,logindex)
        }
    }

    if (length(n)==2){
        min = n[1]; max = n[2]
        log <- apply(df.temp, 2, is.na)
        logindex <- apply(log, 1, function(x) {sum(x) >= min && sum(x) <= max})
        # result = df[logindex,,drop=FALSE]
        result = dplyr::filter(df,logindex)
    }

    # https://stackoverflow.com/a/7570677/2292993
    if (reindex) {row.names(result) <- NULL}

    nafter=nrow(result)
    if ((nbefore-nafter>0) & print2screen) cat(sprintf('%d rows containing NAs dropped (In: %d -> Out: %d).\n', nbefore-nafter, nbefore, nafter))

    return(result)
}

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
#' @seealso \code{\link{ez.append}}
#' @examples
#' postdoc=ez.header(name=character(),'salary'=numeric())
#' postdoc=ez.header(cols=c('name','salary'))  # <--note that 'name','salary' data type is logical in this usage
#'
#' # typical use:
#' results = ez.header(variable=character(),class=character(),n=numeric())
#' vars=colnames(x)
#' for (var in vars) {
#'     var=
#'     cls=
#'     n=
#'     results = ez.append(results,list(var,cls,n))
#' }
#' ez.savex(results,'x.xlsx')
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
#' @param newrow a list, e.g.,  list("Ted", 25)  <-- use list can preserve element data type
#' @param print2screen whether to print the new row to string (auto separated by tab), default TRUE
#' @return returns a new df, old passed df does not change
#' @seealso \code{\link{ez.header}}
#' @note although passing in newrow as a vector is fine, c(char,numeric) converts everything to char, bit when saving in excel, num formated as text
#' @examples
#' # typical use:
#' results = ez.header(variable=character(),class=character(),n=numeric())
#' vars=colnames(x)
#' for (var in vars) {
#'     var=
#'     cls=
#'     n=
#'     results = ez.append(results,list(var,cls,n))
#' }
#' ez.savex(results,'x.xlsx')
#' @export
ez.append = function(df, newrow, print2screen=TRUE){
    # http://vitalflux.com/learn-r-append-rows-data-frame/
    df[nrow(df)+1,] <- newrow
    if (print2screen) {cat(unlist(newrow), '\n', sep='\t')}
    return(df)
}

#' coalesce values in a vector
#' @description see example for more details, dplyr::coalesce do columnwise, only replace NA, but do not detect conflicts if not NA
#' @param vec a single vector, c(NA,3,3), c(NA,3,4), c(NA,NA)
#' @return return depends, see example
#' @examples
#' ez.coalesce(c(NA,3,3))  # 3
#' ez.coalesce(c(NA,NA))  # NA
#' ez.coalesce(c(NA,3,4))  # c(3,4)
#'
#' # typical use for coalesce by rows, see https://stackoverflow.com/q/45515218/2292993
#' df <- data.frame(A=c(1,1,2,2,2),B=c(NA,2,NA,4,4),
#'                  C=c(3,NA,NA,5,NA),D=c(NA,2,3,NA,NA),E=c(5,NA,NA,4,4))
#' df %>% group_by(A) %>% summarise_all(funs( ez.coalesce(.) ))
#'
#' df <- data.frame(A=c(1,1,2,2,2),B=c(NA,2,NA,4,5),
#'                  C=c(3,NA,NA,5,NA),D=c(NA,2,3,NA,NA),E=c(5,NA,NA,4,4))
#' df %>% group_by(A) %>% summarise_all(funs( ez.coalesce(.) ))
#' # ->default give summarise_all an error, but I hack to give '4 | 5' as a string
#' print/show all values, store all unique values
#' @seealso \code{\link[dplyr]{coalesce}}
#' @export
ez.coalesce = function(vec){
    uniVals = unique(vec)
    # if contains only NA
    if (all(is.na(uniVals))) {
        cmd = sprintf('NAOfSameType=as.%s(NA)',class(vec))
        eval(parse(text = cmd))
        return(NAOfSameType)
    } else {
        uniVal = na.omit(uniVals)
        # https://stackoverflow.com/a/45201734/2292993
        if (length(uniVal)>1) {
            ez.pprint(sprintf('multiple unique values found: %s\t%s',deparse(substitute(vec)),paste(vec,collapse = ' | ')),color='red')
            return(paste(uniVal,collapse = ' | '))
        } else {
            return(uniVal)
        }
    }
}

#' ppq
#' @description Pin Pin Qi, input (named) vectors of different length and output a data frame with NA appended to the same length
#' @param ... internal for list(...), see example
#' @return returns a data frame
#' @examples
#' ez.ppq(a=1:4,b=2:4)
#' # a  b
#' # 1  2
#' # 2  3
#' # 3  4
#' # 4 NA
#' @export
ez.ppq = function(...) {
    # https://stackoverflow.com/questions/35215027/adding-nas-to-a-vector
    result = list(...)
    # x=1:3; length(x) <- 4 will make the length of x be 4, or length(x) <- 2 will truncate
    result = data.frame(lapply(result, `length<-`, max(lengths(result))))
    return(result)
}

#' select col names in a df
#' @description select col names in a df, evaluated by sprintf('dplyr::select(df,%s, ...)',toString(col)
#' @param df df
#' @param col a string or a vector or NULL (NULL=all cols)
#' \cr recommended usage:
#' \cr c("sample_num","mother_num") (not quoted)
#' \cr 1:4 (not quoted), c(2,1,3) --> the returned order will be 2,1,3 (not in original df col order)
#' \cr 'col1:col3', 'col3:col5, col7:col8', (quoted)
#' \cr '-(ABCB1_c1236t:pgp_rs2032582)', '-c(neonate_admit_NICU,BDNF)' (quoted)
#' \cr
#' \cr OK usage:
#' \cr 'c(sample_num,mother_num)', 'sample_num,mother_num', '1, col3:col5, col7:col8'
#' @return returns vector of col names (if no col matched, resturn NULL, for easy later use with c(NULL,'a col') -> c('a col'))
#' @export
ez.selcol = function(df,col=NULL, ...) {
    # optimize for big df or long col
    if (is.null(col)) {
        result = colnames(df)
    } else if (all(col %in% colnames(df))) {
        result = col
    } else if (is.numeric(col)) {
        result = names(df)[col]
    } else {
        df = df[1,,drop=F]
        cmd=sprintf('dplyr::select(df,%s, ...)',toString(col))
        cols=ez.eval(cmd)
        result=colnames(cols)
    }

    # note length(NULL) is 0
    if (length(result)==0) result=NULL
    return(result)
}

#' sanitize col names
#' @description replace certain characters (all occurrence) in all column names, using regular expression and gsub(). see also \code{\link{ez.clcoldata}}
#' @param df df
#' @param pattern search
#' @param replacement replacement
#' @param fixed FALSE=regex mode on, TRUE=regex mode off
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param perl Perl-compatible regexps be used, without perl, [[:space:][:punct:]] works, but not [\\s[:punct:]]
#' so seems always a good idea to turn on perl compatible. see \code{\link{gsub}}.
#' ignored when fixed=TRUE
#' @param col NULL=all columns, otherwise restricted to specified cols, eg, ( internally evaluated by eval('dplyr::select()') )
#' \cr 'c(sample_num,mother_num)' (quoted) or c("sample_num","mother_num") (not quoted)
#' \cr 1:4 (not quoted)
#' \cr 'col1:col3' (quoted)
#' \cr '-(ABCB1_c1236t:pgp_rs2032582)', '-c(neonate_admit_NICU,BDNF)' (quoted)
#' @return returns a new df with column names cleaned, old df does not change
#' @examples
#' all upper to lower using regex (ignore.case=FALSE or TRUE does not matter)
#' ez.clcolnames(iris,pattern='([[:upper:]])', replacement = '\\L\\1', perl = TRUE, ignore.case=FALSE)
#' @seealso see also \code{\link{ez.clcoldata}}
#' @export
ez.clcolnames <- function(df,pattern='[[:space:][:punct:]]',replacement='.',fixed=FALSE,ignore.case=FALSE,perl=TRUE,col=NULL) {
    # ignore perl when fixed is true, otherwise issuing a warning
    if (fixed) perl=FALSE
    if (is.null(col)){
        colnames(df) <- gsub(pattern, replacement, colnames(df), fixed=fixed, ignore.case=ignore.case, perl=perl)
        # R cannot have var starting with _
        colnames(df) <- gsub('^_', 'X_', colnames(df), fixed=FALSE, perl=TRUE)
    } else {
        names_all = colnames(df)
        names_sel = (ez.selcol(df,col))
        selected = is.element(names_all,names_sel)
        names_new = dplyr::if_else(selected,gsub(pattern, replacement, names_all, fixed=fixed, ignore.case=ignore.case, perl=perl),names_all)
        colnames(df) <- names_new
        colnames(df) <- gsub('^_', 'X_', colnames(df), fixed=FALSE, perl=TRUE)
    }
    return(df)
}

#' sanitize column-wise char data
#' @description sanitize column-wise char data (if not numeric,logical,date), see also \code{\link{ez.clcolnames}}
#' @param x a data frame or a vector
#' @param col evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols.
#' @param procedures c('toupper','removeleading0') or 'tolower'
#' @return returns a new data frame or vector
#' @seealso \code{\link{ez.clcolnames}}
#' @export
ez.clcoldata = function(x, col=NULL, procedures=c('toupper','removeleading0')) {
    if (!is.data.frame(x)) {
        if (!is.numeric(x) && !is.logical(x) && !ez.is.date(x)) {
            factored = ifelse(is.factor(x), TRUE, FALSE)
            if (factored) {x=as.character(x)}

            if ('toupper' %in% procedures) x = toupper(x)
            if ('tolower' %in% procedures) x = tolower(x)
            if ('removeleading0' %in% procedures) x = sub('^0+','',x)

            if (factored) {x=as.factor(x)}
            x=tryCatch(sjmisc_set_labels(x,""), error=function(e) x, warning = function(w) x, finally=x)
        }
    } else if (is.data.frame(x) & is.null(col)) {
        # x = dplyr::mutate_all(x, funs(ez.clcoldata(.,procedures=procedures)))
        x[] = lapply(x,function(e,procedures){ez.clcoldata(e,procedures=procedures)},procedures=procedures)
    } else if (is.data.frame(x) & !is.null(col)) {
        col = ez.selcol(x,col)
        cols = col
        # for (col in cols) {
        #     x[[col]] = ez.clcoldata(x[[col]],procedures=procedures)
        # }
        x[cols] = lapply(x[cols],function(e,procedures){ez.clcoldata(e,procedures=procedures)},procedures=procedures)
    }
    return(x)
}

#' remove specified attributes
#' @description remove specified attributes
#' @param x a data frame or a vector
#' @param col evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols.
#' @param attrs variable label: c('variable.labels', 'label'); value labels: c('value.labels', 'labels'). run names(attributes(x)) to see all attributes
#' @return returns a new data frame or vector
#' @note this function uses a different mechanism from sjmisc_set_labels(x,"") which works only for value labels: haven style ("labels") or foreign style ("value.labels")
#' @export
ez.clattr = function(x, col=NULL, attrs=c('variable.labels', 'label','value.labels','labels'), ...) {
    if (!is.data.frame(x)) {
        # set_labels only for value labels
        # x=tryCatch(sjmisc_set_labels(x,""), error=function(e) x, warning = function(w) x, finally=x)
        # attributes(x) <- NULL  # this is not desirable because some attributes are needed, eg, $class, $level, $names
        for (a in attrs) {attr(x,a) <- NULL}
    } else if (is.data.frame(x) & is.null(col)) {
        # x = dplyr::mutate_all(x, funs(ez.clattr(.,attrs=attrs)))
        x[] = lapply(x,function(e,attrs){ez.clattr(e,attrs=attrs)},attrs=attrs)
    } else if (is.data.frame(x) & !is.null(col)) {
        col = ez.selcol(x,col)
        cols = col
        # for (col in cols) {
        #     x[[col]] = ez.clattr(x[[col]],attrs=attrs)
        # }
        x[cols] = lapply(x[cols],function(e,attrs){ez.clattr(e,attrs=attrs)},attrs=attrs)
    }
    return(x)
}

#' copy attr of a df or vector, save as a list of list, or list respectively
#' @description copy attr of a df or vector, save as a list of list, or list respectively
#' @param x a data frame or a vector
#' @param col evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols.
#' @param attrs variable label: c('variable.labels', 'label'); value labels: c('value.labels', 'labels'). run names(attributes(x)) to see all attributes
#' @return returns a list of list (x is df), or list (x is vector). Works fine even if x, its col, does not have attrs.
#' @export
ez.copyattr = function(x, col=NULL, attrs=c('label', 'labels'), ...) {
    if (!is.data.frame(x)) {
        result = list()
        for (a in attrs) { result[[a]] = attr(x,a,exact=T) }
    } else if (is.data.frame(x) & is.null(col)) {
        result = lapply(x,function(e,attrs){ez.copyattr(e,attrs=attrs)},attrs=attrs)
    } else if (is.data.frame(x) & !is.null(col)) {
        col = ez.selcol(x,col)
        cols = col
        result = lapply(x[cols],function(e,attrs){ez.copyattr(e,attrs=attrs)},attrs=attrs)
    }
    return(result)
}

#' paste attr to a df or vector, from a list of list, or list respectively
#' @description paste attr to a df or vector, from a list of list, or list respectively
#' @param x a data frame or a vector
#' @param col evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols.
#' @param attrs a list of list ($sex $sex$label $sex$labels for df) or list ($label $labels for vector). Works fine even if the list or list of list is empty, or if list has $sex $race but df has 'sex', no 'race'
#' \cr when only two parameters passed in, will be interpreted as (x,attrs) with col=NULL
#' @return returns a new data frame or vector
#' @export
ez.pasteattr = function(x, col=NULL, attrs=NULL, ...) {
    # when col names is not in names(attrs), ez.pasteattr(vector,NULL,NULL) essentially returns vector itself
    # if names(attrs) has more than col names, because of looping over each col, this is fine.
    if (!is.data.frame(x)) {
        for (a in names(attrs)) { attr(x,a) = attrs[[a]] }
    } else if (is.data.frame(x) & is.null(col)) {
        x[] = lapply(names(x),function(e,attrs){ez.pasteattr(x[[e]],attrs=attrs[[e]])},attrs=attrs)
    } else if (is.data.frame(x) & !is.null(col)) {
        if (is.null(attrs)) {
            # only two parameters passed in
            attrs=col; col=NULL
            x[] = lapply(names(x),function(e,attrs){ez.pasteattr(x[[e]],attrs=attrs[[e]])},attrs=attrs)
        } else {
            col = ez.selcol(x,col)
            cols = col
            x[cols] = lapply(names(x[cols]),function(e,attrs){ez.pasteattr(x[[e]],attrs=attrs[[e]])},attrs=attrs)
        }
    }
    return(x)
}

#' kinda like filter(), but the order of rows are in the order of vec
#' @description kinda like filter(), but the order of rows are in the order of vec
#' @param col a single col name
#' @param vec elements; see note
#' @param nomatch if 0, not return a row for the nomatch; if NA/NULL, return NA.
#' @return returns a new data frame with rows in df[[col]] matching the vec elements. row names are normal (1:nrow)
#' @note works best if your vec contains exactly the same elements as df[[col]], and neither contain duplicate values
#' \cr Actually, duplicated elements in vec OK, will be mathched multiple times. Duplicates in df[[col]] will be picked using first match.
#' @export
ez.match = function(df, col, vec, nomatch=0) {
    if (length(col)!=1 | !is.element(col,colnames(df)) | !is.character(col)) stop('Is your col single exisiting character?')
    # https://stackoverflow.com/a/11977256/2292993
    df[match(vec, df[[col]], nomatch=nomatch),,drop=F]
}
