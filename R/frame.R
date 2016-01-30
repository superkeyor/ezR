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
z.values.get = function(x, include.values=NULL, attr.only=T, include.non.labelled=F, ...){
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
z.values.set = function(x, valuelabels, force.labels=FALSE, force.values=FALSE, ...){
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
z.label.get = function(...){
    result=sjmisc::get_label(list(...))
    return(result)
}

#' set variable label, wrapper of \code{\link[sjmisc]{set_label}}
#' @description
#' @param
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
z.label.set = sjmisc::set_label

#' wrapper of \code{\link[sjmisc]{to_label}}
#' @description continous/factorial number-->factorial level string, say, gender=0/1-->male/female
#' \cr more "agressive" than \code{\link{z.2factor}}; opposite of \code{\link{z.2value}}
#' @param drop.is_na ignore is_na attr, if yes, treat as NA
#' @details Both value and variable label attributes will be removed when converting variables to factors.
#' @examples
#'
#' @return returns a factor with string as its levels
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.2label = function(x, add.non.labelled=TRUE, drop.is_na=FALSE,...){
    result=sjmisc::to_label(x, add.non.labelled=add.non.labelled, drop.na=drop.is_na)
    return(result)
}

#' wrapper of \code{\link[sjmisc]{to_factor}}
#' @description continous number-->categorical number
#' \cr converts a variable into a factor, but preserves variable and value label attributes.
#' \cr more "gentle" than \code{\link{z.2label}}; opposite of \code{\link{z.2value}}
#' @param
#' @details
#' @examples
#'
#' @return returns a factor with number as its levels
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.2factor = function(x, add.non.labelled=TRUE, drop.na=FALSE, ref.lvl=NULL,...){
    result=sjmisc::to_factor(x, add.non.labelled=add.non.labelled, drop.na=drop.na, ref.lvl=ref.lvl)
    return(result)
}

#' wrapper of \code{\link[sjmisc]{to_value}}
#' @description continous number<--categorical string/number
#' @param x factor or a data frame with factors. May also be a character vector.
#' @param start.at starting index, i.e. the lowest numeric value of the variable's value range. By default, this argument is NULL, hence the lowest value of the returned numeric variable corresponds to the lowest factor level (if factor is numeric) or to 1 (if factor levels are not numeric).
#' @details opposite of \code{\link{z.2factor}}, \code{\link{z.2label}}
#' @examples
#' # starting at 1
#' dummy <- factor(c("D", "F", "H"))
#' to_value(dummy)
#' # [1] 1 2 3
#' # attr(,"labels")
#' # D F H
#' # 1 2 3
#' @return returns a numeric variable
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
z.2value = function(x, start.at=NULL, keep.labels=TRUE,...){
    result=sjmisc::to_value(x, start.at=start.at, keep.labels=keep.labels,...)
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
#' \cr\cr z.recode replaces the original var with recoded var;
#' \cr z.recode2 saves orignal var as var_ori, and then recodes var
#' @param df data.frame to be recoded
#' @param varName the name of var to be recoded, must be a string in quotes ""
#' @param recodes Definition of the recoding rules. See details
#' @details recodes contains a set of recoding rules separated by ";". There are three different types of recoding rules:
#' \itemize{
#'  \item{}{The simplest codes one value to another. If we wish to recode 1 into 2, we could use the rule "1=2;".}
#'  \item{}{A range of values can be coded to a single value using "1:3=4;". This rule would code all values between 1 and 3 inclusive into 4. For factors, a value is between two levels if it is between them in the factor ordering. One sided ranges can be specified using the lo and hi key words (e.g."lo:3=0; 4:hi=1")}
#'  \item{}{Default conditions can be coded using "else." For example, if we wish to recode all values >=0 to 1 and all values <0 to missing, we could use ("0:hi=1; else=NA")}
#' }
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
#' z.recode(data[c("a","b")] , "lo:0 = 0;0:hi = 1;")
#' data[c("male")] <- z.recode(data[c("male")] , "1 = 'Male';0 = 'Female';else = NA;")
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
z.recode = function(df, varName, recodes){
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
    # remove all label attr
    newVar = sjmisc::set_labels(newVar,"")
    cmd = sprintf("reshape::rename(df,c(%s='%s'))",varName,paste0(varName,'_ori'))
    # parse: http://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    df = eval(parse(text=cmd))
    df = dplyr::bind_cols(df,newVar)
    cmd = sprintf("z.move(df,'%s before %s')",varName, paste0(varName,'_ori'))
    df = eval(parse(text=cmd))

    cmd = sprintf("dplyr::select(df,-%s)",paste0(varName,'_ori'))
    df = eval(parse(text=cmd))
    return(df)
}

#' recode
#' @description Recodes one single according to a set of rules
#' \cr\cr z.recode replaces the original var with recoded var;
#' \cr z.recode2 saves orignal var as var_ori, and then recodes var
#' @param df data.frame to be recoded
#' @param varName the name of var to be recoded, must be a string in quotes ""
#' @param recodes Definition of the recoding rules. See details
#' @details recodes contains a set of recoding rules separated by ";". There are three different types of recoding rules:
#' \itemize{
#'  \item{}{The simplest codes one value to another. If we wish to recode 1 into 2, we could use the rule "1=2;".}
#'  \item{}{A range of values can be coded to a single value using "1:3=4;". This rule would code all values between 1 and 3 inclusive into 4. For factors, a value is between two levels if it is between them in the factor ordering. One sided ranges can be specified using the lo and hi key words (e.g."lo:3=0; 4:hi=1")}
#'  \item{}{Default conditions can be coded using "else." For example, if we wish to recode all values >=0 to 1 and all values <0 to missing, we could use ("0:hi=1; else=NA")}
#' }
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
#' z.recode(data[c("a","b")] , "lo:0 = 0;0:hi = 1;")
#' data[c("male")] <- z.recode(data[c("male")] , "1 = 'Male';0 = 'Female';else = NA;")
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
z.recode2 = function(df, varName, recodes){
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
    # remove value labels
    newVar = sjmisc::set_labels(newVar,"")
    cmd = sprintf("reshape::rename(df,c(%s='%s'))",varName,paste0(varName,'_ori'))
    # parse: http://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    df = eval(parse(text=cmd))
    df = dplyr::bind_cols(df,newVar)
    cmd = sprintf("z.move(df,'%s before %s')",varName, paste0(varName,'_ori'))
    df = eval(parse(text=cmd))
    return(df)
}

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
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName")
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
#' @param replace c("oldColName"="newColName") or c(oldColName="newColName")
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
