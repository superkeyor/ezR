# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' print out summary statistics about a data frame or other object, alias of \code{\link[Hmisc]{describe}}
#' @description print out summary statistics about a data frame or other object, alias of \code{\link[Hmisc]{describe}}
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @export
ez.describe = function(x){
    if (!is.data.frame(x)) {(x = data.frame(x))}
    # flush otherwise not print large text
    # show(x)
    print(Hmisc::describe(x))
    flush.console()
    # print(summary(x))
    # flush.console()
    # cat('--------------------------------------------------------------------------------------------\n')
    # str(x)
    # flush.console()
}

#' compare two vectors, two dataframes
#' @description compare two vectors, two dataframes
#' @note nrow() for data frame, length() for vector. union/intersect remove duplication
#' @export
ez.compare = function(lh,rh,...) {
    if (is.data.frame(lh)) {
            len=nrow
            cat(sprintf('comparing nrow for two data frame uniques: %s\t%s\n\n',deparse(substitute(lh)),deparse(substitute(rh)) ))
        } else {
            len=length
            cat(sprintf('comparing length for two vector uniques: %s\t%s\n\n',deparse(substitute(lh)),deparse(substitute(rh)) ))
        }

    # https://github.com/tidyverse/dplyr/issues/3238
    # union/intersect remove duplication
    cat( sprintf('\t\t\t\tUnion: %4.0f\n',len(dplyr::union(lh,rh,...))) )
    cat( sprintf('\t\tLH: %4.0f /%4.0f\t\t\t\tRH: %4.0f /%4.0f\n',len(lh),len(unique(lh)),len(rh),len(unique(rh))) )
    cat( sprintf('\t\t\t\tInter: %4.0f\n',len(dplyr::intersect(lh,rh,...))) )
    cat('\n')
    cat( sprintf('\t\tLH>: %4.0f\t\t\t\t<RH: %4.0f\n',len(dplyr::setdiff(lh,rh,...)),len(dplyr::setdiff(rh,lh,...))) )
    cat('set equal?\n')
    cat(dplyr::setequal(lh,rh,...))
    cat('\n')
}

#' view the overview of a data frame or similar object (like spss variable view, but with much more information)
#' @description vi (view everything print out), vv (view format vector), vx (view excel), View (built-in).
#' @param df a data frame
#' @param id a single col name in string or number (eg, 'age' or 3), that serves as (potentially unique) id, except which duplicated rows will be checked against. If NULL, rownames() will be auto used
#' @param file a file name, if NULL, a temp generated, will save more detailed variable information to an excel file
#' @param width controls if too many factor levels to print, eg 300. NULL=unlimited
#' @param characterize T/F count the element freq of character cols or not
#' @note when file=NULL (which stores to a temp file), debug=NULL, use getOption('debug') which is TRUE if not set up
#' \cr when file=NULL, debug provided, overwrites getOption('debug')
#' \cr when file provided, any debug is ignored
#' \cr Bottom line: file > param debug > option debug
#' \cr Updated: as of Thu, Nov 30 2017, not any more a wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @return returns a list $row, $col, $dat (input data frame), $pth (file path)
#' @export
ez.vx = function(df, id=NULL, file=NULL, width=300, characterize=TRUE, incomparables=FALSE, debug=NULL, ...){
    # if temped and not debug, just jump out of the function to save time
    if (is.null(file)) {
        debugMode = if (is.null(getOption('debug'))) TRUE else getOption('debug')
        # overwritten by 'debug' passed to function
        if (is.null(debug)) {
            if (!debugMode) {
                return(invisible(NULL))
            }
        } else if (!debug) {
            return(invisible(NULL))
        }
    }

    # if duplicated col names, the following main codes would crash with weird reasons
    # duplicated row names are fine
    if ( sum(ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1))>0 ) {
        stop(sprintf('I cannot proceed. Duplicated col names found: %s\n', colnames(df)[which(ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1))] %>% toString))
    }

    # id string to tell user which col used as id
    idString = if (is.null(id)) 'rowname' else id

    temped=F
    if(is.null(file)){
        temped=T
        file=tempfile(pattern = paste0('view_id_',idString,'_',ez.moment(),'_'), tmpdir = tempdir(), fileext = ".xlsx")
        on.exit(unlink(file))
    }

    # row summary
    r.rowname=rownames(df) %>% ez.num()
    if (is.null(id)) {
        idname=rownames(df) %>% ez.num()
    } else {
        idname=df[,id]
    }

    r.duplicated.idname=ez.duplicated(idname,vec=TRUE,incomparables=incomparables,dim=1,vecgroup=TRUE)
    r.duplicated.idname[which(!r.duplicated.idname)]=NA
    # check duplicated row except the idname column
    if (is.null(id)) {
        r.duplicated.content=ez.duplicated(df,vec=TRUE,incomparables=incomparables,dim=1,vecgroup=TRUE)
    } else {
        # https://github.com/tidyverse/dplyr/issues/2184
        # to avoid the bug, in case variable name id is the same as one of the column names
        idididid=id
        r.duplicated.content=ez.duplicated(dplyr::select(df,-one_of(idididid)),vec=TRUE,incomparables=incomparables,dim=1,vecgroup=TRUE)
    }
    r.duplicated.content[which(!r.duplicated.content)]=NA

    tmpMatrix = is.na(df)
    r.ncol = rep(ncol(tmpMatrix), nrow(tmpMatrix))
    r.missing = rowSums(tmpMatrix,na.rm=TRUE)

    results0=data.frame(rowname=r.rowname,id=idname,duplicated_id=r.duplicated.idname,
                        duplicated_content_except_id=r.duplicated.content,ncol=r.ncol,missing=r.missing)
    results0=dplyr::mutate(results0,missing_rate=missing/ncol)
    results0=ez.rncol(results0,c('id'=paste0('id_',idString)))
    results0=dplyr::mutate(results0,nonmissing=ncol-missing,nonmissing_rate=nonmissing/ncol)



    # col summary
    results = ez.header(variable=character(),class=character(),n=numeric(),missing=numeric(),unique_including_na=numeric(),
                        levels_view1=character(),levels_view2=character(),
                        mean=numeric(),min=numeric(),max=numeric(),sum=numeric())
    vars=colnames(df)
    allFactorUniqueValues=character()
    allFactorCounts=integer()
    for (var in vars) {
        v.variable=var
        v.class=class(df[[var]]) %>% toString(width=width)  # could have multiple classes
        v.n=length(df[[var]])
        v.missing=sum(is.na(df[[var]]))
        v.unique=length(unique(df[[var]]))
        # countable as levels
        if ( is.factor(df[[var]]) | (is.character(df[[var]]) & characterize) | is.logical(df[[var]]) ) {
            freqtable=dplyr::count_(df,var)
            vallbl=sjmisc_get_labels(df[[var]],include.values='n',attr.only=T,include.non.labelled=F)
            if (!is.null(vallbl)){
                vallbl = ez.2label(freqtable[[1]])  # would be in the same order to freqtable
                vallbl = paste("[",vallbl,"]",sep="")
            } else {
                vallbl = rep("", nrow(freqtable))
            }

            v.levels1 = paste("(", paste(freqtable[[1]],vallbl,sep="",collapse=", "),")", " : ", "(", paste(freqtable[[2]],sep="",collapse=", "), ")", sep="") %>% toString(width=width)
            v.levels2 = paste("(",freqtable[[1]],vallbl,": ",freqtable[[2]],")",sep="",collapse=", ") %>% toString(width=width)

            freqtable=dplyr::count_(df,var)
            allFactorUniqueValues=unique(c(allFactorUniqueValues,unique(freqtable[[1]]) %>% as.character()))
            allFactorCounts=c(allFactorCounts,freqtable[[2]])
        } else {
            v.levels1=v.levels2=NA
        }
        # calculable
        is.date <- function(x) inherits(x, 'Date')
        # not all NA
        if ( is.numeric(df[[var]]) & !all(is.na(df[[var]])) ) {
            v.mean=mean(df[[var]],na.rm=TRUE)
            v.min=min(df[[var]],na.rm=TRUE)
            v.max=max(df[[var]],na.rm=TRUE)
            v.sum=sum(df[[var]],na.rm=TRUE)
        } else if ( is.date(df[[var]]) & !all(is.na(df[[var]])) ) {
            # converted to numeric, to convert back to date: ez.date(ori='R')
            v.mean=mean(df[[var]],na.rm=TRUE)
            v.min=min(df[[var]],na.rm=TRUE)
            v.max=max(df[[var]],na.rm=TRUE)
            # sum not defined for "Date" objects
            v.sum=NA
        } else {
            v.mean=v.min=v.max=v.sum=NA
        }
        results = ez.append(results,list(v.variable,v.class,v.n,v.missing,v.unique,v.levels1,v.levels2,v.mean,v.min,v.max,v.sum),print2screen=FALSE)
    }
    v.duplicated.varname=ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1,vecgroup=TRUE)
    v.duplicated.varname[which(!v.duplicated.varname)]=NA
    v.duplicated.content=ez.duplicated(df,vec=TRUE,incomparables=incomparables,dim=2,vecgroup=TRUE)
    v.duplicated.content[which(!v.duplicated.content)]=NA
    v.duplicated=data.frame(duplicated_varname=v.duplicated.varname,duplicated_content=v.duplicated.content)
    results=dplyr::bind_cols(results,v.duplicated) %>% ez.move('duplicated_varname, duplicated_content before n')
    results=dplyr::mutate(results,missing_rate=missing/n)
    results=ez.rncol(results,c('n'='nrow'))
    # no factors in the data frame!
    allFactorUniqueValues = if (length(allFactorUniqueValues)==0) NA else allFactorUniqueValues %>% toString(width=width)
    allFactorCounts = if (length(allFactorCounts)==0) NA else range(allFactorCounts,na.rm =T) %>% toString(width=width)
    results=dplyr::add_row(results,variable='Total',levels_view1=allFactorUniqueValues,
                          levels_view2=allFactorCounts)
    results=results %>% ez.move('levels_view1 levels_view2 after variable')
    results=dplyr::mutate(results,nonmissing=nrow-missing,nonmissing_rate=nonmissing/nrow)
    results=results %>% ez.move('missing_rate nonmissing nonmissing_rate before unique_including_na')
    # labels=attr(df,'variable.labels') # only work for foreign package: 'variable.labels'
    # in case of 'variable.labels', it contains all varialbes' labels,
    # even some of them are not in the current df due to variable selection, eg, select()
    # So only retrieve exisiting cols in current df
    labels = ez.label.get(df,cols=colnames(df))
    # if no labels at all, NULL; otherwise a string vector
    # labels' length should match #variables selected
    if ((!is.null(labels)) & nrow(results)==length(labels)+1) {results['varlbl']=c(labels,''); results=results %>% ez.move('varlbl before class')}

    ez.savexlist(list('row'=results0,'col'=results,'dat'=df),file=file,withFilter = TRUE,rowNames = FALSE, colNames = TRUE)

    # give some time to open the file and then on.exit will delete it
    # although OS will be able to auto clean temp files later on
    # tempdir() is where it is
    if (temped) {
        debugMode = if (is.null(getOption('debug'))) TRUE else getOption('debug')
        # overwritten by 'debug' passed to function
        if (is.null(debug)) {
            if (debugMode) {
                browseURL(file)
                ez.sleep(3)
            }
        } else {
            if (debug) {
                browseURL(file)
                ez.sleep(3)
            }
        }
    }
    return(invisible(list('row'=results0,'col'=results,'dat'=df,'pth'=file)))
}

#' format a vector for easy manual copy/processing.
#' @description vi (view everything print out), vv (view format vector), vx (view excel), View (built-in).
#' @param vec a vector
#' @param printn print first n and last n (useful for loooong vector). If 2n >= total length, print all. Inf=all
#' @param quote TRUE/FALSE, whether add a quote around each element (switch for string or number). NULL = auto (F for numeric, T otherwise)
#' @param order vector order for printing out, 'as','az','za'
#' @return nothing, only print out.
#' \cr By default in R when you type a variable name, you get [1] "rs171440fwd" "rs1800497fwd"
#' \cr now with this function you get 'rs171440fwd','rs1800497fwd','rs180043'
#' @seealso \code{\link{ez.print}} \code{\link{ez.pprint}}
#' @export
ez.vv = function(vec,printn=Inf,order='as',quote=NULL,print2screen=TRUE){
    if(is.null(quote)) {quote = if (is.numeric(vec)) FALSE else TRUE}

    if (2*printn >= length(vec)) {printn=NULL}

    if (order=='az') {vec=sort(vec,decreasing=F,na.last=T)}
    if (order=='za') {vec=sort(vec,decreasing=T,na.last=T)}

    if (quote) {
        if (is.null(printn)){
            printout=noquote(paste0("'",noquote(paste0(vec,collapse = "','")),"'"))
        } else {
            header = noquote(paste0("'",noquote(paste0(vec[1:printn],collapse = "','")),"'"))
            tailer = noquote(paste0("'",noquote(paste0(vec[(length(vec)-printn+1):length(vec)],collapse = "','")),"'"))
            printout = noquote(paste(header,tailer,sep=",..................,"))
        }
    } else {
        if (is.null(printn)){
            printout=noquote(paste0(vec,collapse = ","))
        } else {
            header = noquote(paste0(vec[1:printn],collapse = ","))
            tailer = noquote(paste0(vec[(length(vec)-printn+1):length(vec)],collapse = ","))
            printout = noquote(paste(header,tailer,sep=",..................,"))
        }
    }
    if (print2screen) {
        print(printout)
        cat(sprintf("Total elements: %d\n",length(vec)))
    }
    return(invisible(printout))
}

#' print sorted uniques of a df col or a vector (NA last) and other information
#' @description vi (view everything print out), vv (view format vector), vx (view excel), View (built-in). print sorted uniques of a df col or a vector (NA last) and other information
#' @param order vector order for printing out, 'as','az','za'
#' @param printn print first n and last n (useful for loooong vector). If 2n >= total length, print all. Inf=all
#' @param printcn Counts/Levels print n
#' @export
ez.vi = function(x,printn=35,printcn=600,order='as') {
    v = x
    if (is.data.frame(v) | is.matrix(v)) {
        if ( sum(ez.duplicated(colnames(v),vec=TRUE,dim=1))>0 ) {
            stop(sprintf('I cannot proceed. Duplicated col names foud: %s\n', colnames(v)[which(ez.duplicated(colnames(v),vec=TRUE,incomparables=incomparables,dim=1))] %>% toString))
        }
        v.class = if (is.matrix(v)) 'matrix' else class(v) %>% toString()
        if (is.matrix(v)) {v = data.frame(v)}
        v.cols = colnames(v) %>% ez.vv(print2screen=F,printn=printn,order=order)
        v.nrow = nrow(v)
        v.ncol = ncol(v)
        v.missing=ez.count(v,NA,dim=3)
        v.n.colNumsAllNAs = length(as.vector(which(colSums(is.na(v)) == nrow(v))))

        classes=character()
        for (col in colnames(v)) {
            classes=c(classes,class(v[[col]]))
        }
        freq = table(classes)
        v.classes = paste('#',freq %>% names,': ',freq,sep='',collapse = ', ')

        v.attrs = attributes(v)
        v.attrs[c('row.names','names','class')] <- NULL
        v.attrs = sapply(v.attrs,length)
        v.attrs = paste(names(v.attrs),v.attrs,sep = ' @ ',collapse = ', ')
        if (v.attrs == '') v.attrs = '@'

        first2rows = if (nrow(v)>=4) 1:2 else 1:nrow(v); last2rows = if (nrow(v)>=4) (nrow(v)-2+1):nrow(v) else NULL
        first3cols = if (ncol(v)>=6) 1:3 else 1:ncol(v); last3cols = if (ncol(v)>=6) (ncol(v)-3+1):ncol(v) else NULL
        # c(1:2,NULL) -> (1,2)
        rows = if (nrow(v)==0) integer(0) else c(first2rows,last2rows)
        cols = if (ncol(v)==0) integer(0) else c(first3cols,last3cols)
        print(v[rows,cols,drop=F])

        cat(v.cols)
        cat(sprintf('\n%-25s\tDim: %d x %d\t#EmptyCols: %d\t#NA: %d\n%s\n', v.class, v.nrow, v.ncol, v.n.colNumsAllNAs, v.missing, v.classes))
        cat(sprintf('attributes: %s\n',v.attrs))
    } else if (is.list(x)) {
        for (l in names(x)) {
            cat(sprintf('$%-25s\t%s\n',l,class(x[[l]]) %>% toString))
        }
        cat(sprintf('List of %d\n',length(x)))
    } else {
        v.elements = unique(v) %>% ez.vv(print2screen=F,printn=printn,order=order)
        v.class=class(v) %>% toString()
        if (!is.null(names(v))) v.class=paste0('Named ',v.class)
        v.n=length(v)
        v.missing=sum(is.na(v))
        v.unique=length(unique(v))

        v.attrs = attributes(v)
        v.attrs[c('levels','names','class')] <- NULL
        v.attrs = sapply(v.attrs,length)
        v.attrs = paste(names(v.attrs),v.attrs,sep = ' @ ',collapse = ', ')
        if (!is.null(ez.getlabel(v))) {v.attrs = paste(v.attrs,ez.getlabel(v),sep='\n',collapse = '')}
        if (v.attrs == '') v.attrs = '@'

        # calculable
        is.date <- function(x) inherits(x, 'Date')
        # not all NA
        if ( is.numeric(v) & !all(is.na(v)) ) {
            v.mean=mean(v,na.rm=TRUE)
            v.sd=sd(v,na.rm=TRUE)
            v.min=min(v,na.rm=TRUE)
            v.max=max(v,na.rm=TRUE)
            v.sum=sum(v,na.rm=TRUE)
        } else if ( is.date(v) & !all(is.na(v)) ) {
            # converted to numeric, to convert back to date: ez.date(ori='R')
            v.mean=mean(v,na.rm=TRUE)
            v.sd=sd(v,na.rm=TRUE)
            v.min=min(v,na.rm=TRUE)
            v.max=max(v,na.rm=TRUE)
            # sum not defined for "Date" objects
            v.sum=NA
        } else {
            v.mean=v.sd=v.min=v.max=v.sum=NA
            # count as factor
            if ( is.factor(v) | is.character(v) | is.logical(v) ) {
                freqtable=dplyr::count_(data.frame(tmpvar=v),"tmpvar")
                vallbl=sjmisc_get_labels(v,include.values='n',attr.only=T,include.non.labelled=F)
                if (!is.null(vallbl)){
                    vallbl = ez.2label(freqtable[[1]])  # would be in the same order to freqtable
                    vallbl = paste("[",vallbl,"]",sep="")
                } else {
                    vallbl = rep("", nrow(freqtable))
                }
                v.levels = paste(freqtable[[1]],vallbl,": ",freqtable[[2]],sep="",collapse="\n")
            }
        }

        if ( is.factor(v) | is.character(v) | is.logical(v) ) {
            cat(sprintf('Counts/Levels (Incl NA): \n%s\n',v.levels %>% toString(width=printcn)))
        }
        cat(sprintf('Uniques (Incl NA): %s\n', v.elements))
        cat(sprintf('\n%-25s\t#Unique (Incl NA): %d\t#NA: %d (%.0f%%)\t#Non-NA: %d\t#Total: %d\n', v.class, v.unique, v.missing, v.missing*100/v.n, v.n-v.missing, v.n))
        if ( (is.numeric(v) | is.date(v)) & !all(is.na(v)) ) {
            cat(sprintf('M = %.2f\tSD = %.2f\tRange = (%.2f,%.2f)\tSum = %.2f\n', v.mean, v.sd, v.min, v.max, v.sum))
        }
        cat(sprintf('attributes: %s\n',v.attrs))
    }
    return(invisible(NULL))
}

#' @rdname ez.vi
#' @export
vi=ez.vi

#' standard error of mean
#' @description na will be omitted before calculation, the formula is sqrt(var(x,na.rm=TRUE)/length(na.omit(x))) (equivalent to sd(x,na.rm=TRUE)/sqrt(length(na.omit(x))))
#' @param x a vector
#' @note \code{\link[stats]{sd}}, standard deviation (sigma or sd, s) is simply the (positive) square root of the variance (sigma^2, or s^2), \code{\link[stats]{var}}. Both sd(), var() use denominator n - 1, which gives an unbiased estimator of the (co)variance for i.i.d. observations.
#' se = sd/sqrt(n). see https://www.statsdirect.com/help/basic_descriptive_statistics/standard_deviation.htm
#' \cr\cr For zscore (x-mean(x,na.rm=T))/sd(x,na.rm=T), or use \code{\link{ez.scale}}(x, center = TRUE, scale = TRUE) demean: ez.scale(x,center=TRUE,scale=FALSE). (ez.scale() auto omits NAs)
#' \cr z-scores indeed have a mean of zero and a standard deviation of 1. Other than that, however, z-scores follow the exact same distribution as original scores. That is, standardizing scores doesn't make their distribution more or less "normal" in any way.
#' see https://www.spss-tutorials.com/z-scores-what-and-why/
#' @export
ez.se = function(x) {
    # http://stackoverflow.com/a/7220087/2292993
    sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}

#' scale
#' @description similar to base::scale, but 1d vector in and 1d vector out, NA ignored/returned in place
#' @export
ez.scale = function(x,center = TRUE, scale = TRUE) {
    as.vector(scale(x,center=center,scale=scale))
}

#' z score
#' @description z score, 1d vector in and 1d vector out, NA ignored/returned in place
#' @export
ez.z = function(x,center = TRUE, scale = TRUE) {
    as.vector(scale(x,center=center,scale=scale))
}

#' r correlation
#' @description r correlation
#' @param x df or matrix
#' @param col col included for calculation, ignored if x is not a df
#' @param type "pearson" or "spearman"
#' @param viewresult no effect if there is no NA
#' @return r matrix (you may get some NAs in the marix without warning)
#' @export
ez.r = function(x,col=NULL,type="pearson",print2screen=T) {
    if (is.data.frame(x) & !is.null(col)) {
        col=(ez.selcol(x,col))
        x=x[col]
    }
    # rcorr uses pairwise deletion; however, you may still get warnings/errors/NAs for calcuating r, p
    # because too few samples, or too small variance (close to 0) after deletion
    # https://www.statmethods.net/stats/correlations.html
    # stats::cor(Matrix or data frame, use=) use has
    #   all.obs (assumes no missing data - missing data will produce an error),
    #   complete.obs (listwise deletion), and
    #   pairwise.complete.obs (pairwise deletion)
    result = Hmisc::rcorr(data.matrix(x),type=type)$r
    CorrNACounts = data.frame(result) %>% ez.count(val=NA,dim=1) %>% tibble::rownames_to_column(var='variable') %>% dplyr::arrange(desc(count))
    NAs = sum(CorrNACounts$count)

    if (NAs > 0) {
        if (print2screen) {ez.pprint(sprintf('Attention: %s NAs contained in the correlation matrix (see CorrNACounts).', NAs), color='red')}
        assign("CorrNACounts", CorrNACounts, envir = .GlobalEnv)
    }
    return(result)
}

#' z residual
#' @description z residual, as.vector(scale(resid(model),center=T,scale=T))
#' @param method 1=scale(resid), 2=stats::rstandard, 3=MASS::stdres
#' @note according to jerry's test, \code{\link[stats]{rstandard}} and \code{\link[MASS]{stdres}} give the same results, both of which are slightly different from (but very close to, highly correlated with) method 1
#' \cr all three methods give different results from spss: linear regression->save->residuals, standarized
#' @export
ez.zresid = function(model,method=3) {
    if (method==1) {result=as.vector(scale(resid(model),center=TRUE,scale=TRUE))}
    if (method==2) {result=stats::rstandard(model)}
    if (method==3) {result=MASS::stdres(model)}
    return(result)
}

#' a series of regression, for many y and many x; if many y and many x at the same time, returns a list
#' @description df=ez.2value(df,y,...), df[[x]]=ez.2value(df[[x]],...), lm(scale(df[[y]])~scale(df[[x]]+scale(df[[covar]])))
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2value(df))
#' \cr NA in df will be auto excluded in lm(), reflected by degree_of_freedom
#' @param y internally evaluated by eval('dplyr::select()'), a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x internally evaluated by eval('dplyr::select()'), a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param covar NULL=no covar, internally evaluated by eval('dplyr::select()'), a vector of covariates c('var1','var2'), or a single variable 'var1'
#' @param pmethods c('bonferroni','fdr'), type p.adjust.methods for all methods. This correction applies for all possible tests that have been/could be done.
#' @param plot T/F, the black dash line is bonferroni p = 0.05, the grey black dash is uncorrected p = 0.05
#' @param cols number of columns for gmultiplot. NULL=auto calculate
#' @param showerror whether show error message when error occurs
#' @param ... dots passed to ez.2value(df,...)
#' @return an invisible list of data frame
#' \cr beta: standardized beta coefficients (simple or multiple regression) are the estimates resulting from a regression analysis that have been standardized
#' \cr so that the variances of dependent and independent variables are 1.
#' \cr Therefore, standardized coefficients refer to how many standard deviations a dependent variable will change,
#' \cr per standard deviation increase in the predictor variable.
#' \cr For simple regression (1 y ~ 1 x), the value of the standardized coefficient (beta) equals the correlation coefficient (r) (beta=r).
#' \cr For multiple regression (with covar), the value of the standardized coefficient (beta) is close to semi-partial correlation
#' \cr According to jerry testing, scale() or not for x,y or covar, does not change p values for predictors, although intercept would differ
#' \cr
#' \cr degree_of_freedom: from F-statistic
#' \cr rp is robust regression (MASS::rlm) p value (see codes for more detail)
#' @note To keep consistent with other R functions (eg, lm which converts numeric/non-numeric factor to values starting from 0), set start.at=0 in ez.2value(), then factor(1:2)->c(0,1), factor(c('girl','boy'))->c(1,0) # the level order is boy,girl
#' \cr in lm() the coding (0,1) vs.(1,2) does not affect slope, but changes intercept (but a coding from 1,2->1,3 would change slope--interval difference matters)
#' @export
ez.regressions = function(df,y,x,covar=NULL,showerror=T,viewresult=F,plot=T,cols=3,pmethods=c('bonferroni','fdr'),labsize=2,textsize=1.5,titlesize=3,...) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))

    # patch to handle multiple y, multiple x
    if (length(y)>1 & length(x)>1) {
        xlist = list(); plist = list()
        for (yy in y) {
            # plot = F; no need for sepearte plotlist
            result = ez.regressions(df,yy,x,covar=covar,showerror=showerror,viewresult=viewresult,plot=F,cols=cols,pmethods=pmethods,labsize=labsize,textsize=textsize,titlesize=titlesize,...)
            result = result[[1]]
            if (plot) {
                bonferroniP = -log10(0.05/length(result[['p']]))
                plist[[yy]] = lattice::xyplot(-log10(result$p) ~ result$beta,
                   xlab = list("Standardized Coefficient", cex=labsize, fontfamily="Times New Roman"),
                   ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                   scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                   type = "p", pch=16,
                   main = list(yy, cex=3, fontfamily="Times New Roman"),
                   col = "#e69f00",
                   ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                   abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
                )
            }
            xlist[[yy]] = result
        }
        if (plot) {if (is.null(cols)) {cols=floor(sqrt(length(plist)))}; gridExtra::grid.arrange(grobs=plist,ncol=cols)}
        return(invisible(xlist))
    }

    df=ez.2value(df,y,...)

    getStats = function(y,x,covar,swap=F,data,...){
        df=data; yy=y; xx=x
        # for single y but multiple x using lapply
        if (swap) {tmp=xx;xx=yy;yy=tmp}

        if (is.null(covar)) {
            covar = ''
            rcovar = ''
        } else {
            covar=ez.selcol(df,covar)
            df=ez.2value(df,covar,...)
            rcovar = paste('+',covar,sep='',collapse='')
            covar = paste('+scale(df[["',covar,'"]])',sep='',collapse='')
        }

        tryCatch({
        if (nlevels(df[[xx]])>2) ez.pprint(sprintf('col %s has >=3 factor levels, consider dummy coding instead of ez.2value.', xx), color='red')
        df[[xx]]=ez.2value(df[[xx]],...)

        # according to my own testing, scale or not for x,y or covar, does not change p values for predictors, although intercept would differ
        cmd = sprintf('model = summary( lm( scale(df[[yy]])~scale(df[[xx]])%s ) )', covar)
        ez.eval(cmd)
        p = model$coefficients[2,4]

        cmd = sprintf('rmodel = MASS::rlm(%s~%s%s,data=df)', yy,xx,rcovar)
        ez.eval(cmd)
        # https://stats.stackexchange.com/a/205615/100493
        rtest = sfsmisc::f.robftest(rmodel,var=xx)
        rp = rtest$p.value

        beta = model$coefficients[2,1]
        degree_of_freedom = model$df[2]

        # only calculate the one that varies
        v = if (!swap) df[[yy]] else df[[xx]]
        v.unique=length(unique(v))
        v.min=min(v,na.rm=TRUE)
        v.max=max(v,na.rm=TRUE)
        v.mean=mean(v,na.rm=TRUE)
        v.sd=sd(v,na.rm=TRUE)

        out = c(yy,xx,p,rp,beta,degree_of_freedom,v.unique,v.min,v.max,v.mean,v.sd)
        return(out)
        }, error = function(e) {
            if (showerror) message(sprintf('Error: %s %s. NA returned.',yy,xx))
            return(c(yy,xx,NA,NA,NA,NA,NA,NA,NA,NA,NA))
        })
    }

    if (length(y)>=1 & length(x)==1) result = lapply(y,getStats,x=x,covar=covar,data=df,...)
    if (length(y)==1 & length(x)>1) result = lapply(x,getStats,x=y,swap=T,covar=covar,data=df,...)
    result = result %>% data.frame() %>% data.table::transpose()
    names(result) <- c('y','x','p','rp','beta','degree_of_freedom','uniques_incl_na','min','max','mean','sd')
    result %<>% ez.num() %>% ez.dropna()

    if (plot) {
        bonferroniP = -log10(0.05/length(result[['p']]))
        # if (length(y)==1 & length(x)>1) {
        #     tt = sprintf('
        #     pp = result %%>%% ez.dropna() %%>%% ez.factorder("x",ord="as") %%>%% ggplot(aes(x=x,y=-log10(p),fill=y))+
        #         geom_bar(stat="identity")+
        #         geom_hline(yintercept = %f,color="black",linetype=5)+
        #         geom_hline(yintercept = -log10(0.05),color="grey",linetype=5)+
        #         scale_fill_manual(values=rep(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"),100))+
        #         theme(legend.position="none")+
        #         %s', bonferroniP, sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),'y') )
        # } else {
        #     tt = sprintf('
        #     pp = result %%>%% ez.dropna() %%>%% ez.factorder("y",ord="as") %%>%% ggplot(aes(x=y,y=-log10(p),fill=x))+
        #         geom_bar(stat="identity")+
        #         geom_hline(yintercept = %f,color="black",linetype=5)+
        #         geom_hline(yintercept = -log10(0.05),color="grey",linetype=5)+
        #         scale_fill_manual(values=rep(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"),100))+
        #         theme(legend.position="none")+
        #         %s', bonferroniP, sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),'x') )
        # }
        # eval(parse(text = tt))
        # print(pp)
        pp=lattice::xyplot(-log10(result$p) ~ result$beta,
               xlab = list("Standardized Coefficient", cex=labsize, fontfamily="Times New Roman"),
               ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
               scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
               type = "p", pch=16,
               main = list(ifelse((length(y)>=1 & length(x)==1),x,y), cex=3, fontfamily="Times New Roman"),
               col = "#e69f00",
               ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
               abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
        )
        print(pp)
    }

    for (method in pmethods) {
        result[[method]]=stats::p.adjust(result[['p']],method=method)
    }
    ylbl = ez.label.get(df,result$y); xlbl = ez.label.get(df,result$x)
    if (is.null(ylbl)) {ylbl=''}; if (is.null(xlbl)) {xlbl=''}; result$ylbl=ylbl; result$xlbl=xlbl
    result$orindex=1:nrow(result)
    result = ez.move(result,'orindex first; ylbl after y; xlbl after x') %>% dplyr::arrange(p)
    if (viewresult) {View(result)}
    return(invisible(list(result)))
}

#' a series of simple logistic regression, for many y and many x; if many y and many x at the same time, returns a list
#' @description df=ez.2value(df,y,...), df[[xx]]=ez.2value(df[[xx]],...), glm(df[[yy]]~df[[xx]],family=binomial)
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2value(df))
#' \cr NA in df will be auto excluded in glm(), reflected by degree_of_freedom
#' @param y internally evaluated by eval('dplyr::select()'), a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x internally evaluated by eval('dplyr::select()'), a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param covar NULL=no covar, internally evaluated by eval('dplyr::select()'), a vector of covariates c('var1','var2'), or a single variable 'var1'
#' @param pmethods c('bonferroni','fdr'), type p.adjust.methods for all methods. This correction applies for all possible tests that have been/could be done.
#' @param plot T/F, the dash line is bonferroni p = 0.05
#' @param cols number of columns for gmultiplot. NULL=auto calculate
#' @param showerror whether show error message when error occurs
#' @param ... dots passed to ez.2value(df,...)
#' @return an invisible list of data frame
#' \cr odds_ratio: odds ratio=exp(b), one unit increase in x result in the odds of being 1 for y "OR" times the odds of being 0 for y
#' \cr so that the variances of dependent and independent variables are 1.
#' \cr Therefore, standardized coefficients refer to how many standard deviations a dependent variable will change,
#' \cr per standard deviation increase in the predictor variable.
#' \cr
#' \cr degree_of_freedom
#' @export
ez.logistics = function(df,y,x,covar=NULL,showerror=T,viewresult=F,plot=T,cols=3,pmethods=c('bonferroni','fdr'),labsize=2,textsize=1.5,titlesize=3,...) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))

    # patch to handle multiple y, multiple x
    if (length(y)>1 & length(x)>1) {
        xlist = list(); plist = list()
        for (yy in y) {
            # plot = F; no need for sepearte plotlist
            result = ez.logistics(df,yy,x,covar=covar,showerror=showerror,viewresult=viewresult,plot=F,cols=cols,pmethods=pmethods,labsize=labsize,textsize=textsize,titlesize=titlesize,...)
            result = result[[1]]
            if (plot) {
                bonferroniP = -log10(0.05/length(result[['p']]))
                plist[[yy]] = lattice::xyplot(-log10(result$p) ~ log2(result$odds_ratio),
                   xlab = list("log2(Odds Ratio)", cex=labsize, fontfamily="Times New Roman"),
                   ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                   scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                   type = "p", pch=16,
                   main = list(yy, cex=3, fontfamily="Times New Roman"),
                   col = "#e69f00",
                   ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                   abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
                )
            }
            xlist[[yy]] = result
        }
        if (plot) {if (is.null(cols)) {cols=floor(sqrt(length(plist)))}; gridExtra::grid.arrange(grobs=plist,ncol=cols)}
        return(invisible(xlist))
    }

    df=ez.2value(df,y,...)

    getStats = function(y,x,covar,swap=F,data,...){
        df=data; yy=y; xx=x
        # for single y but multiple x using lapply
        if (swap) {tmp=xx;xx=yy;yy=tmp}

        if (is.null(covar)) {
            covar = ''
        } else {
            covar=ez.selcol(df,covar)
            df=ez.2value(df,covar,...)
            covar = paste('+df[["',covar,'"]]',sep='',collapse='')
        }

        tryCatch({
        if (nlevels(df[[xx]])>2) ez.pprint(sprintf('col %s has >=3 factor levels, consider dummy coding instead of ez.2value.', xx), color='red')
        df[[xx]]=ez.2value(df[[xx]],...)

        cmd = sprintf('model = summary(glm((df[[yy]])~(df[[xx]])%s,family = binomial(link = "logit")))', covar)
        ez.eval(cmd)

        p = model$coefficients[2,4]
        odds_ratio = exp(model$coefficients[2,1])
        degree_of_freedom = model$df[2]

        out = c(yy,xx,p,odds_ratio,degree_of_freedom)
        return(out)
        }, error = function(e) {
            if (showerror) message(sprintf('Error: %s %s. NA returned.',yy,xx))
            return(c(yy,xx,NA,NA,NA))
        })
    }

    if (length(y)>=1 & length(x)==1) result = lapply(y,getStats,x=x,covar=covar,data=df,...)
    if (length(y)==1 & length(x)>1) result = lapply(x,getStats,x=y,swap=T,covar=covar,data=df,...)
    result = result %>% data.frame() %>% data.table::transpose()
    names(result) <- c('y','x','p','odds_ratio','degree_of_freedom')
    result %<>% ez.num() %>% ez.dropna()

    if (plot) {
        bonferroniP = -log10(0.05/length(result[['p']]))
        pp=lattice::xyplot(-log10(result$p) ~ log2(result$odds_ratio),
               xlab = list("log2(Odds Ratio)", cex=labsize, fontfamily="Times New Roman"),
               ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
               scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
               type = "p", pch=16,
               main = list(ifelse((length(y)>=1 & length(x)==1),x,y), cex=3, fontfamily="Times New Roman"),
               col = "#e69f00",
               ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
               abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
        )
        print(pp)
    }

    for (method in pmethods) {
        result[[method]]=stats::p.adjust(result[['p']],method=method)
    }
    ylbl = ez.label.get(df,result$y); xlbl = ez.label.get(df,result$x)
    if (is.null(ylbl)) {ylbl=''}; if (is.null(xlbl)) {xlbl=''}; result$ylbl=ylbl; result$xlbl=xlbl
    result$orindex=1:nrow(result)
    result = ez.move(result,'orindex first; ylbl after y; xlbl after x') %>% dplyr::arrange(p)
    if (viewresult) {View(result)}
    return(invisible(list(result)))
}

#' a series of one-way anova, for many y and many x; if many y and many x at the same time, returns a list
#' @description df = ez.2value(df,y,...); df = ez.2factor(df,x); aov(df[[yy]]~df[[xx]])
#' @param df a data frame
#' \cr NA in df will be auto excluded in aov(), reflected by degree_of_freedom
#' @param y internally evaluated by eval('dplyr::select()'), a vector of continous variables c('var1','var2'), or a single variable 'var1', if it is a factor, auto converts to numeric (internally call ez.2value(df[[yy]]), (eg, names(select(beta,Gender:dmce)))
#' @param x internally evaluated by eval('dplyr::select()'), a vector of categorical variables, or a single categorical variable
#' @param pmethods c('bonferroni','fdr'), type p.adjust.methods for all methods. This correction applies for all possible tests that have been/could be done.
#' @param plot T/F, the dash line is bonferroni p = 0.05
#' @param cols number of columns for gmultiplot. NULL=auto calculate
#' @param showerror whether show error message when error occurs
#' @param ... dots passed to ez.2value(df[[yy]],...)
#' @return an invisible list of data frame
#' \cr the means column in excel can be split into mulitiple columns using Data >Text to Columns
#' \cr degree_of_freedom: from F-statistic
#' @note Eta squared measures the proportion of the total variance in a dependent variable that is associated with the membership of different groups defined by an independent variable.
#' \cr Partial eta squared is a similar measure in which the effects of other independent variables and interactions are partialled out. The development of these measures is described and their characteristics compared.
#' @export
ez.anovas = function(df,y,x,showerror=T,viewresult=F,plot=T,cols=3,pmethods=c('bonferroni','fdr'),labsize=2,textsize=1.5,titlesize=3,...) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))

    # patch to handle multiple y, multiple x
    if (length(y)>1 & length(x)>1) {
        xlist = list(); plist = list()
        for (xx in x) {
            # plot = F; no need for sepearte plotlist
            result = ez.anovas(df,y,xx,showerror=showerror,viewresult=viewresult,plot=F,cols=cols,pmethods=pmethods,labsize=labsize,textsize=textsize,titlesize=titlesize,...)
            result = result[[1]]
            if (plot) {
                bonferroniP = -log10(0.05/length(result[['p']]))
                if (!all(is.na(result$mean12_difference))) {
                    plist[[xx]] = lattice::xyplot(-log10(result$p) ~ result$mean12_difference,
                       xlab = list("Difference in Standardized Group Means", cex=labsize, fontfamily="Times New Roman"),
                       ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                       scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                       type = "p", pch=16,
                       main = list(xx, cex=titlesize, fontfamily="Times New Roman"),
                       col = "#e69f00",
                       ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                       abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
                    )
                } else {
                    plist[[xx]] = lattice::xyplot(-log10(result$p) ~ result$etasq2,
                           xlab = list(expression(eta^2), cex=labsize, fontfamily="Times New Roman"),
                           ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                           scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                           type = "p", pch=16,
                           main = list(xx, cex=titlesize, fontfamily="Times New Roman"),
                           col = "#e69f00",
                           ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                           abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
                    )
                }
            }
            xlist[[xx]] = result
        }
        if (plot) {if (is.null(cols)) {cols=floor(sqrt(length(plist)))}; gridExtra::grid.arrange(grobs=plist,ncol=cols)}
        return(invisible(xlist))
    }

    df = ez.2value(df,y,...); df = ez.2factor(df,x)

    getStats = function(y,x,swap=F,data,...){
        df=data; yy=y; xx=x
        # for single y but multiple x using lapply
        if (swap) {tmp=xx;xx=yy;yy=tmp}

        tryCatch({
        a = aov(df[[yy]]~df[[xx]])
        p = summary(a)[[1]][["Pr(>F)"]][[1]]
        degree_of_freedom = toString(summary(a)[[1]][['Df']])
        s = aggregate(df[[yy]]~df[[xx]],FUN=mean)
        n = aggregate(df[[yy]]~df[[xx]],FUN=length)
        means = ''; counts = ''
        for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
        for (i in 1:ez.size(n,1)) {counts = paste(counts,n[i,1],n[i,2],sep='\t')}
        # https://stats.stackexchange.com/a/78813/100493
        etasq2 = summary.lm(a)$r.squared
        s = aggregate(scale(df[[yy]])~df[[xx]],FUN=mean)
        mean12_difference = if (nrow(s)==2) s[1,2]-s[2,2] else NA
        out = c(xx,yy,p,etasq2,degree_of_freedom,mean12_difference,means,counts)
        return(out)
        }, error = function(e) {
            if (showerror) message(sprintf('Error: %s %s. NA returned.',xx,yy))
            return(c(xx,yy,NA,NA,NA,NA,NA,NA))
        })
    }

    if (length(y)>=1 & length(x)==1) result = lapply(y,getStats,x=x,data=df,...)
    if (length(y)==1 & length(x)>1) result = lapply(x,getStats,x=y,swap=T,data=df,...)
    result = result %>% data.frame() %>% data.table::transpose()
    names(result) <- c('x','y','p','etasq2','degree_of_freedom','mean12_difference','means','counts')
    result %<>% ez.num() %>% ez.dropna()

    if (plot) {
        bonferroniP = -log10(0.05/length(result[['p']]))
        if (!all(is.na(result$mean12_difference))) {
            pp=lattice::xyplot(-log10(result$p) ~ result$mean12_difference,
                       xlab = list("Difference in Standardized Group Means", cex=labsize, fontfamily="Times New Roman"),
                       ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                       scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                       type = "p", pch=16,
                       main = list(ifelse((length(y)>=1 & length(x)==1),x,y), cex=3, fontfamily="Times New Roman"),
                       col = "#e69f00",
                       ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                       abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
            )
        } else {
            pp=lattice::xyplot(-log10(result$p) ~ result$etasq2,
                   xlab = list(expression(eta^2), cex=labsize, fontfamily="Times New Roman"),
                   ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                   scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                   type = "p", pch=16,
                   main = list(ifelse((length(y)>=1 & length(x)==1),x,y), cex=3, fontfamily="Times New Roman"),
                   col = "#e69f00",
                   ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                   abline=list(h=c(bonferroniP,-log10(0.05)),lty=2,lwd=2,col=c('black','darkgrey'))
            )
        }
        print(pp)
    }

    for (method in pmethods) {
        result[[method]]=stats::p.adjust(result[['p']],method=method)
    }
    ylbl = ez.label.get(df,result$y); xlbl = ez.label.get(df,result$x)
    if (is.null(ylbl)) {ylbl=''}; if (is.null(xlbl)) {xlbl=''}; result$ylbl=ylbl; result$xlbl=xlbl
    result$orindex=1:nrow(result)
    result = ez.move(result,'orindex first; ylbl after y; xlbl after x') %>% dplyr::arrange(p)
    if (viewresult) {View(result)}
    return(invisible(list(result)))
}

#' a series of fisher.test, for many y and many x; if many y and many x at the same time, returns a list
#' @description df=ez.2factor(df,c(x,y)), fisher.test(df[[xx]],df[[yy]])
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2factor(df))
#' \cr NA in df will be auto excluded in fisher.test(), reflected by total
#' @param y internally evaluated by eval('dplyr::select()'), a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x internally evaluated by eval('dplyr::select()'), a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param pmethods c('bonferroni','fdr'), type p.adjust.methods for all methods. This correction applies for all possible tests that have been/could be done.
#' @param plot T/F, the dash line is bonferroni p = 0.05
#' @param cols number of columns for gmultiplot. NULL=auto calculate
#' @param showerror whether show error message when error occurs
#' @param width width for toString(countTable,width=width)
#' @return an invisible list of data frame
#' @note odds ratio only exist for 2x2 table, otherwise 0 (arbitrary assigned by jerry)
#' @export
ez.fishers = function(df,y,x,showerror=T,viewresult=F,plot=T,cols=3,pmethods=c('bonferroni','fdr'),labsize=2,textsize=1.5,titlesize=3,width=300) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))

    # patch to handle multiple y, multiple x
    if (length(y)>1 & length(x)>1) {
        xlist = list(); plist = list()
        for (xx in x) {
            # plot = F; no need for sepearte plotlist
            result = ez.fishers(df,y,xx,showerror=showerror,viewresult=viewresult,plot=F,cols=cols,pmethods=pmethods,labsize=labsize,textsize=textsize,titlesize=titlesize,width=width)
            result = result[[1]]
            if (plot) {
                bonferroniP = -log10(0.05/length(result[['p']]))
                plist[[xx]] = lattice::barchart(-log10(result$p) ~ result$y,
                   xlab = list("Variable", cex=labsize, fontfamily="Times New Roman"),
                   ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
                   scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                   type = "p", pch=16,
                   main = list(xx, cex=titlesize, fontfamily="Times New Roman"),
                   col = "#e69f00",
                   ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
                   panel=function(x,y,...){
                       panel.barchart(x,y,...)
                       panel.abline(h=bonferroniP,col.line="black",lty=2,lwd=2)
                       panel.abline(h=-log10(0.05),col.line="darkgrey",lty=2,lwd=2)}
                )
            }
            xlist[[xx]] = result
        }
        if (plot) {if (is.null(cols)) {cols=floor(sqrt(length(plist)))}; gridExtra::grid.arrange(grobs=plist,ncol=cols)}
        return(invisible(xlist))
    }

    df=ez.2factor(df,c(x,y))

    getStats = function(y,x,swap=F,data){
        df=data; yy=y; xx=x
        # for single y but multiple x using lapply
        if (swap) {tmp=xx;xx=yy;yy=tmp}

        tryCatch({
        fisher.test(df[[xx]],df[[yy]]) -> model # by default, pairwise NA auto removed
        p = model$p.value
        odds_ratio = if (is.null(model$estimate)) 0 else model$estimate  # only exist for 2x2 table
        countTable = table(df[[xx]],df[[yy]])   # by default, pairwise NA auto removed
        counts = toString(countTable,width=width)
        total = sum(countTable)
        out = c(xx,yy,p,odds_ratio,counts,total)
        return(out)
        }, error = function(e) {
            if (showerror) message(sprintf('Error: %s %s. NA returned.',xx,yy))
            return(c(xx,yy,NA,NA,NA,NA))
        })
    }

    if (length(y)>=1 & length(x)==1) result = lapply(y,getStats,x=x,data=df)
    if (length(y)==1 & length(x)>1) result = lapply(x,getStats,x=y,swap=T,data=df)
    result = result %>% data.frame() %>% data.table::transpose()
    names(result) <- c('x','y','p','odds_ratio','counts','total')
    result %<>% ez.num() %>% ez.dropna()

    if (plot) {
        bonferroniP = -log10(0.05/length(result[['p']]))
        if (length(y)>=1 & length(x)==1) {
            pp=lattice::barchart(-log10(result$p) ~ result$y,
               xlab = list("Variable", cex=labsize, fontfamily="Times New Roman"),
               ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
               scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
               type = "p", pch=16,
               main = list(x, cex=3, fontfamily="Times New Roman"),
               col = "#e69f00",
               ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
               panel=function(x,y,...){
                   panel.barchart(x,y,...)
                   panel.abline(h=bonferroniP,col.line="black",lty=2,lwd=2)
                   panel.abline(h=-log10(0.05),col.line="darkgrey",lty=2,lwd=2)}
            )
        } else {
            pp=lattice::barchart(-log10(result$p) ~ result$x,
               xlab = list("Variable", cex=labsize, fontfamily="Times New Roman"),
               ylab = list("-log10(p-Value)", cex=labsize, fontfamily="Times New Roman"),
               scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
               type = "p", pch=16,
               main = list(y, cex=3, fontfamily="Times New Roman"),
               col = "#e69f00",
               ylim=c(-0.5,max(c(bonferroniP,-log10(result$p)))+0.5),
               panel=function(x,y,...){
                   panel.barchart(x,y,...)
                   panel.abline(h=bonferroniP,col.line="black",lty=2,lwd=2)
                   panel.abline(h=-log10(0.05),col.line="darkgrey",lty=2,lwd=2)}
            )
        }
        print(pp)
    }

    for (method in pmethods) {
        result[[method]]=stats::p.adjust(result[['p']],method=method)
    }
    ylbl = ez.label.get(df,result$y); xlbl = ez.label.get(df,result$x)
    if (is.null(ylbl)) {ylbl=''}; if (is.null(xlbl)) {xlbl=''}; result$ylbl=ylbl; result$xlbl=xlbl
    result$orindex=1:nrow(result)
    result = ez.move(result,'orindex first; ylbl after y; xlbl after x') %>% dplyr::arrange(p)
    if (viewresult) {View(result)}
    return(invisible(list(result)))
}

#' table xy
#' @description (df, x, y) or (x, y), \code{\link[gmodels]{CrossTable}},  auto convert/reset factor
#' \cr ez.table input could be one, two, or more varibles/cols
#' @export
#' @examples
#' # dnn=c('row','col')
ez.table2 = function(df, x, y=NULL, digits=2, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c=FALSE,
                       prop.t=TRUE, prop.chisq=FALSE, chisq = FALSE, fisher=TRUE, mcnemar=FALSE,
                       resid=FALSE, sresid=FALSE, asresid=FALSE,
                       missing.include=FALSE,
                       format="SPSS", dnn = NULL, ...) {
    # (df, x, y)
    if (!is.null(y)) {
        df = ez.2factor(df,c(x,y))
        if (is.null(dnn)) dnn=ez.eval(sprintf('c("%s","%s")',x,y))
        result = gmodels::CrossTable(df[[x]], df[[y]], digits=digits, max.width = max.width, expected=expected, prop.r=prop.r, prop.c=prop.c,
                           prop.t=prop.t, prop.chisq=prop.chisq, chisq=chisq, fisher=fisher, mcnemar=mcnemar,
                           resid=resid, sresid=sresid, asresid=asresid,
                           missing.include=missing.include,
                           format=format, dnn = dnn, ...)
    # (x, y)
    } else {
        if (is.null(dnn)) {
            dnn = c(deparse(substitute(df)),deparse(substitute(x)))
            dnn=gsub('^[\\w\\.]+\\$','',dnn,perl=T)
        }
        y=x; x=df
        x=ez.2factor(x); y=ez.2factor(y)
        result = gmodels::CrossTable(x, y, digits=digits, max.width = max.width, expected=expected, prop.r=prop.r, prop.c=prop.c,
                           prop.t=prop.t, prop.chisq=prop.chisq, chisq=chisq, fisher=fisher, mcnemar=mcnemar,
                           resid=resid, sresid=sresid, asresid=asresid,
                           missing.include=missing.include,
                           format=format, dnn = dnn, ...)
    }
    return(invisible(result))
}

#' freq table
#' @description freq table, df followed by col names (df, col1, col2, col3), or vector, factor (eg, x,y,z), auto convert/reset factor
#' \cr ez.table2 output is more beautiful, but must be two varibles/cols
#' \cr result table can be further passed to prop.table(), addmargins(), addmargins(prop.table())
#' @export
#' @param x, ... df followed by col names (df, col1, col2, col3), or vector, factor (eg, x,y,z)
#' @param dnn c('col','row')
#' @param exclude values to use in the exclude argument of factor when interpreting non-factor objects. could be NULL to include NA
#' @param row.vars    a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the rows of the flat contingency table.
#' @param col.vars    a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the flat contingency table.
ez.table = function(x, ..., dnn=NULL, exclude = c(NA, NaN), row.vars = NULL,col.vars = NULL) {
    if (is.data.frame(x)) {
        # input = x[c(...)]  # when pass date frame, dnn not working
        dots=sapply(as.list(substitute(list(...)))[-1L], deparse)
        input = paste('ez.2factor(x$',dots,')',sep='',collapse=', ')
        if (is.null(dnn)) dnn=c(...)
        cmd = sprintf('ftable(%s, exclude=exclude, row.vars=row.vars, col.vars=col.vars, dnn=dnn)', input)
        theTable = ez.eval(cmd)
        cmd = sprintf('fisher.test(%s)',input)
    } else {
        dots=sapply(as.list(substitute(list(...)))[-1L], deparse)
        if (is.null(dnn)) {
            dnn=paste("'",c(deparse(substitute(x)),dots),"'",sep='',collapse = ', ')
            # print(dnn)
            # get rid of df$
            dnn=gsub('[\\w\\.]+\\$','',dnn,perl=T) %>% paste0('c(',.,')')
            dnn=ez.eval(dnn)
        }
        input = paste('ez.2factor(',c('x',dots),')',sep='',collapse = ', ')
        cmd = sprintf('ftable(%s, exclude=exclude, row.vars=row.vars, col.vars=col.vars, dnn=dnn)', input)
        # print(cmd)
        theTable = ez.eval(cmd)
    }

    print(theTable)

    if (is.data.frame(x)) {
        if (length(list(...))>0) {
            fisher = ez.eval(cmd)
            cat(sprintf("\nFisher's Exact Test, two-sided p = %f\n",fisher$p.value))
        }
    } else {
        if (length(list(...))>0) {
            fisher = fisher.test(x,...)
            cat(sprintf("\nFisher's Exact Test, two-sided p = %f\n",fisher$p.value))
        }
    }

    return(invisible(theTable))
}

#' maximize np by checking recursively various variable combinations (PCA trick)
#' @description maximize np by checking recursively various variable combinations (PCA trick)
#' @param targetVar a single outcome var, will calculate its mean in each completed subsamples
#' @param fixedVars vars you must have remained. (does not matter if it includes targetVar or not)
#' @return returns a new data frame
#' @export
ez.maxnp = function(df,targetVar=NULL,fixedVars=NULL,labsize=2.5,textsize=1.5) {
    if (!is.null(targetVar)) {targetVar=ez.selcol(df,targetVar); df %<>% ez.move(sprintf('%s first',targetVar))}
    if (!is.null(fixedVars)) {fixedVars = ez.selcol(df,fixedVars); fixedVars = dplyr::setdiff(fixedVars,targetVar)}

    if ( (!is.null(targetVar)) | (!is.null(fixedVars)) ) {
        # c() works even if targetVar=NULL
        df %>% select(-one_of(c(targetVar,fixedVars))) -> df01
    } else {
        df -> df01
    }

    # df01: 0s and 1s without target, fix, completed vars, zeroVars for PCA
    ind.NAs = is.na(df01)
    lapply(df01, as.character) %>% data.frame(stringsAsFactors = F) -> df01
    df01[!ind.NAs] <- 1; df01[ind.NAs] <- 0
    lapply(df01, as.numeric) %>% data.frame() -> df01

    # get rid of constant 1, for scale and proper pca solution
    # according to my test, if constant 1 into pca, you cannot scale and pca seems strange:
    # the constant var get loading of 0, in the middle of positive and negative laodings
    completedVars = ez.selcol(df01, which(colSums(df01) == nrow(df01)))
    zeroVars  = ez.selcol(df01, which(colSums(df01) == 0))
    if (!is.null(completedVars)) {df01 %<>% select(-one_of(completedVars))}
    if (!is.null(zeroVars)) {df01 %<>% select(-one_of(zeroVars))}

    ####************************************************************************************************
                                         ####*obsolete*####
    ####************************************************************************************************
    # no need to scale for pca, has the same scale 0/1 (well, still not same variance?)
    # in fact scale error if all 1 or 0, scale = TRUE cannot be used if there are zero or constant (for center = TRUE) variables

    # # https://stat.ethz.ch/pipermail/r-help/2006-April/104616.html
    # # duplicated to increse sample size if necessary
    # # because 'princomp' can only be used with more samples than variables
    # df01 = df01[rep(seq(nrow(df01)), each = ceiling(ncol(df01)/nrow(df01))),,drop=F]
    # pcaObj = stats::princomp(base::scale(df01))
    # pcaLoadings <- with(pcaObj, unclass(loadings)) %>% data.frame()
    # pcaLoadings %>% tibble::rownames_to_column() %>% arrange(Comp.1) %>% .$rowname %>% ez.vv()
    ####************************************************************************************************
                                         ####*obsolete*####
    ####************************************************************************************************

    pcaObj = stats::prcomp(df01,scale=T)
    # high loading=more 1s, which means fewer missing values
    pcaLoadings = pcaObj$rotation %>% data.frame() %>% tibble::rownames_to_column() %>% arrange(desc(PC1))

    allvars = c(targetVar,fixedVars,completedVars,pcaLoadings$rowname,zeroVars)
    # sample#, variable#, mean(targetVar)
    counts = matrix(NA,nrow=length(allvars),ncol=3)
    # chop from the last var
    for (i in length(allvars):1) {
        vars = allvars[1:i]
        df %>% select(vars) %>% ez.dropna(print2screen = F) -> df3
        counts[i,1] = nrow(df3)
        counts[i,2] = ncol(df3)

        if (is.null(targetVar)) {
            counts[i,3] = NA
        } else if (is.nan( mean(df3[[targetVar]]) )) {
            # if df3 empty, mean=NaN
            counts[i,3] = NA
        } else {
            counts[i,3] = mean(df3[[targetVar]])
        }
    }
    counts = data.frame(counts)
    colnames(counts) = c('sampleNum','variableNum','targetMean')
    counts$orderedVar = allvars

    # the plot() will not return an object. plot directly, hard to capture to an object
    # graphics::plot(x = variableNum, y = sampleNum)
    p1=lattice::xyplot(counts$sampleNum ~ counts$variableNum,
                  xlab = list("Number of Variables Kept", cex=labsize, fontfamily="Times New Roman"),
                  ylab = list("Sample Size Without Missing Values", cex=labsize, fontfamily="Times New Roman"),
                  scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                  type = "p", pch=16,
                  col="#e69f00")
    p2=NULL
    if (!all(is.na(counts$targetMean))) {
        p2=lattice::xyplot(counts$targetMean ~ counts$variableNum,
                  xlab = list("Number of Variables Kept", cex=labsize, fontfamily="Times New Roman"),
                  ylab = list(sprintf("Mean Value of %s",targetVar), cex=labsize, fontfamily="Times New Roman"),
                  scales = list( x=list(cex=textsize, fontfamily="Times New Roman"), y=list(cex=textsize, fontfamily="Times New Roman") ),
                  type = "p", pch=16,
                  col="#56b4e9")
    }
    gmultiplot(p1,p2,cols=2)

    return(invisible(counts))
}

dprime <- function(hit, fa, miss, cr, adjusted=TRUE) {

    n_hit = hit
    n_fa = fa
    n_miss = miss
    n_cr = cr

    # 1) correct ------------------------------------------------------
    correct <- (n_hit+n_cr)/(n_hit+n_miss+n_fa+n_cr)


    # 2) Parametric Indices ------------------------------------------------------
    if (adjusted == TRUE) {
        # Adjusted ratios: Advocates of the loglinear approach recommend using it regardless of whether or not extreme rates are obtained.
        hit_rate_adjusted <- (n_hit + 0.5) / ((n_hit + 0.5) + n_miss + 1)
        fa_rate_adjusted <- (n_fa + 0.5) / ((n_fa + 0.5) + n_cr + 1)

        # dprime
        dprime <- stats::qnorm(hit_rate_adjusted) - stats::qnorm(fa_rate_adjusted)

        # beta
        zhr <- stats::qnorm(hit_rate_adjusted)
        zfar <- stats::qnorm(fa_rate_adjusted)
        beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)

        # criterion c
        c <- -(stats::qnorm(hit_rate_adjusted) + stats::qnorm(fa_rate_adjusted)) / 2
        # normalized c prime
        cprime <- c/dprime

    } else {
        # Ratios
        n_targets <- n_hit + n_miss
        n_distractors <- n_fa + n_cr

        hit_rate <- n_hit / n_targets
        fa_rate <- n_fa / n_distractors

        # dprime
        dprime <- stats::qnorm(hit_rate) - stats::qnorm(fa_rate)

        # beta
        zhr <- stats::qnorm(hit_rate)
        zfar <- stats::qnorm(fa_rate)
        beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)

        # criterion c
        c <- -(stats::qnorm(hit_rate) + stats::qnorm(fa_rate)) / 2
        # normalized c prime
        cprime <- c/dprime
    }


    # 3) Non-Parametric Indices ------------------------------------------------------
    # Ratios
    n_targets <- n_hit + n_miss
    n_distractors <- n_fa + n_cr
    hit_rate <- n_hit / n_targets
    fa_rate <- n_fa / n_distractors

    # aprime
    a <- 1 / 2 + ((hit_rate - fa_rate) * (1 + hit_rate - fa_rate) / (4 * hit_rate * (1 - fa_rate)))
    b <- 1 / 2 - ((fa_rate - hit_rate) * (1 + fa_rate - hit_rate) / (4 * fa_rate * (1 - hit_rate)))

    a[fa_rate > hit_rate] <- b[fa_rate > hit_rate]
    a[fa_rate == hit_rate] <- .5
    aprime <- a

    # bppd
    bppd <- ((1 - hit_rate) * (1 - fa_rate) - hit_rate * fa_rate) / ((1 - hit_rate) * (1 - fa_rate) + hit_rate * fa_rate)


    # 4) return ------------------------------------------------------
    n_targets <- n_hit + n_miss
    n_distractors <- n_fa + n_cr
    hr <- n_hit / n_targets
    far <- n_fa / n_distractors
    
    return(list(dprime = dprime, beta = beta, c = c, cprime = cprime, hr = hr, far = far, correct = correct, aprime = aprime, bppd = bppd))
}

#' Computes Signal Detection Theory indices (percent of correct, d', beta, A', B''D, c, c')
#' @description Computes Signal Detection Theory indices (percent of correct, d', beta, A', B''D, c, c').
#' @param hit Number of hits.
#' @param fa Number of false alarms.
#' @param miss Number of misses.
#' @param cr Number of correct rejections.
#' @param adjusted Should it use the Hautus (1995) adjustments for extreme values (hit rate of 1 and false alarm rate of 0). Note: only affects dprime, beta, c, cprime; all other results have nothing to do with adjustment. see for more the following notes and \href{https://stats.stackexchange.com/questions/134779}{stackexchange}
#'
#' @return Returns a data frame: 
#' (summary: aprime, bppd are nonparametric version of dprime, beta, nonparemetric not subjective to assumptions/extreme values)
#' \itemize{
#'  \item{\strong{hr}: }{hit rate, calculated with raw data, the unadjusted one returned}
#'  \item{\strong{far}: }{false alarm rate, , the unadjusted one returned}    
#'  \item{\strong{correct}: }{correct = (Hits + CR)/(Hits+Misses+FA+CR) , but it cannot disentangle the two components, ie, discriminability and bias (The major contribution of SDT to psychology is the separation of these two). Intuitively, the best subject maximizes H (and thus minimizes the Miss rate) and minimizes F (and thus maximizes the Correct Rejection rate); hit rate H = hit / (hit+miss); false alarm rate F = FA / (FA + CR); targets = hit + miss; distractors= fa + cr.}
#'  \item{\strong{dprime (d')}: }{The sensitivity, discriminability index. Reflects the mean/peak distance (in standard deviation unit) between the signal and noise distributions (d' = z(H) - z(F), other sensitivity measures include a transform other than z, or even no transform at all). The larger the difference between H and F, the better the subject's sensitivity. A value of 0 indicates an inability to distinguish signals from noise, whereas larger values indicate a correspondingly greater ability to distinguish signals from noise. Negative values of d' can arise through sampling error or response confusion (responding yes when intending to respond no, and vice versa). Though Z values can have any real value, normally distributed ones are between -2 and 2 about 95 percent of the time. SDT states that d' is unaffected by response bias (i.e., is a pure measure of sensitivity) if two assumptions are met regarding the decision variable: (1) The signal and noise distributions are both normal, and (2) the signal and noise distributions have the same standard deviation. We call these the d' assumptions. If either assumption is violated, d' will vary with response bias. Because of this, some researchers prefer to use nonparametric measures of sensitivity. The most popular is A'. }
#'  \item{\strong{beta}: }{The decision bias, response bias, bias/criterion. Use of this measure assumes that responses are based on a likelihood ratio. Suppose the decision variable achieves a value of x on a given trial. The numerator for the ratio is the likelihood of obtaining x on a signal trial (i.e., the height of the signal distribution at x), whereas the denominator for the ratio is the likelihood of obtaining x on a noise trial (i.e., the height of the noise distribution at x). Formula is exp(-(zH-zF) x (zH+zF)/2). When subjects favor neither the yes response nor the no response, beta=1. Values less than 1 signify a bias toward responding yes (liberal), whereas values of beta greater than 1 signify a bias toward the no (conservative) response. Because beta is based on a ratio, the natural logarithm of beta is often analyzed in place of beta itself. This script gives beta, not ln(beta).}
#'  \item{\strong{aprime (A')}: }{Non-parametric estimate of discriminability. A' typically ranges from .5, which indicates that signals cannot be distinguished from noise, to 1, which corresponds to perfect performance. Values less than .5 may arise from sampling error or response confusion; the minimum possible value is 0.}
#'  \item{\strong{bppd (B''D)}: }{Non-parametric estimate of bias. B''D ranges from -1 (extreme bias in favor of yes/liberal responses) to 1 (extreme bias in favor of no/conservative responses). A value of 0 signifies no response bias.}
#'  \item{\strong{c}: }{Another index of bias, criterion c. c is defined as the distance (the number/unit of standard deviations) between the criterion and the neutral/midpoint point between these two distributions, where neither response is favored (beta=1). If the criterion is located at this point, c has a value of 0. Negative values of c signify a bias toward responding yes (the criterion lies to the left of the neutral point), whereas positive values signify a bias toward the no response (the criterion lies to the right of the neutral point). One advantage of c is that it is unaffected by changes in d', whereas beta is not.}
#'  \item{\strong{cprime}: }{Normalized c, which is c/dprime}
#'  }
#'
#'
#' Note that for d', beta, c, cprime, adjustement for extreme values are made following the loglinear recommandations of Hautus (1995). These extreme values are particularly likely to arise when signals differ markedly from noise, few trials are presented (so that sampling error is large), or subjects adopt extremely liberal or conservative criteria (as might occur if, for example, the consequences of a false alarm are severe). Advocates of the loglinear approach recommend using it regardless of whether or not extreme rates are obtained.
#' @examples
#' hit <- 9
#' fa <- 2
#' miss <- 1
#' cr <- 7
#'
#' indices <- ez.dprime(hit, fa, miss, cr)
#'
#'
#' df <- data.frame(Participant = c("A", "B", "C"),
#'     hit = c(1, 2, 5), miss = c(9, 8, 5),
#'     fa = c(6, 8, 1), cr = c(4, 2, 9))
#'
#' indices <- ez.dprime(hit=df$hit,
#'     fa=df$fa,
#'     miss=df$miss,
#'     cr=df$cr,
#'     adjusted=FALSE)
#'
#'
#' @author Jerry modified from \href{https://dominiquemakowski.github.io/}{Dominique Makowski}. See Pallier (2002) for the algorithms, Stanislaw & Todorov (1999) for a good tutorial.
#'
#' @export
ez.dprime = function(hit, fa, miss, cr, adjusted=TRUE){
    if (length(hit)>1){
        # Vectorize accepts SIMPLIFY, but even though dprime() returns a data.frame
        # Vectorized function still returns a list
        # so I deal with list and convert it to a data frame
        myfun = Vectorize(dprime)
        result = myfun(hit, fa, miss, cr, adjusted)
        # return a data frame
        result = data.frame(t(result))
        # convert each column to numeric from list inherited from Vectorize
        result[] = lapply(result,unlist)
        result[] = lapply(result,ez.nan2na)
    } else {
        # return a data frame
        result = dprime(hit, fa, miss, cr, adjusted)
        result = data.frame(result)
    }
    return(result)
}

#' calculate effect size
#' @description calculate effect size
#' @param m1 mean
#' @param s1 standard deviation
#' @param n1 numbers/subjects/samples group 1
#' @param m2 mean
#' @param s2 standard deviation
#' @param n2 numbers/subjects/samples group 2
#' @return returns invisible
#' @export
ez.es.t.independent.msn = function(m1,s1,n1,m2,s2,n2) {
    # simply sd weighted by sample size
    s_pooled = sqrt( (((n1-1)*s1*s1)+((n2-1)*s2*s2)) / (n1+n2-2) )
    d = (m1-m2)/s_pooled

    output = sprintf("d = %0.2f", d)
    cat(output, "\n", sep = "")
    cat('d [0.20 0.50) = small, [0.50 0.80) = medium, [0.80 ) = large. The sign of d is arbitrary.\n')
    return(invisible(d))
}

#' calculate effect size
#' @description calculate effect size
#' @param t t (equal variances assumed in SPSS), numbers/subjects/samples group 1 and 2
#' @return returns invisible
#' @export
ez.es.t.independent.tn = function(t,n1,n2) {
    # this formula could be derived from t formula for independent t-test
    # equivalently sqrt((n1+n2)/(n1*n2))
    d = t*( sqrt((1/n1+1/n2)) )

    output = sprintf("d = %0.2f", d)
    cat(output, "\n", sep = "")
    cat('d [0.20 0.50) = small, [0.50 0.80) = medium, [0.80 ) = large. The sign of d is arbitrary.\n')
    return(invisible(d))
}

#' calculate effect size
#' @description calculate effect size
#' @param t t for paired samples t test, available in SPSS paired samples Test output table
#' @param n number of pairs, in SPSS paired samples Test output table, n=df+1
#' @param r correlation, In case, the correlation is unknown, please fill in 0.
#' @return returns invisible
#' @note formula from Dunlap 1996: Meta-analysis of experiments with matched groups or repeated measures designs. And notes from Section 5 https://www.psychometrica.de/effect_size.html
#' @export
ez.es.t.paired.tnr = function(t,n,r) {
    d = t*sqrt( 2.0*(1.0-r)/n )

    output = sprintf("d = %0.2f", d)
    cat(output, "\n", sep = "")
    cat('d [0.20 0.50) = small, [0.50 0.80) = medium, [0.80 ) = large. The sign of d is arbitrary.\n')
    return(invisible(d))
}

#' calculate effect size
#' @description calculate effect size
#' @param m12 the mean of differences that equals the difference of means (m1-m2), available in SPSS paired samples Test output table
#' @param s12 the standard deviation of the difference score, available in SPSS paired samples Test output table
#' @param r correlation, In case, the correlation is unknown, please fill in 0.
#' @return returns invisible
#' @export
ez.es.t.paired.m12s12r = function(m12,s12,r) {
    # derive the following formulas, based on t = m12/se12 = m12/(s12/sqrt(n)), therefore
    # d = t*sqrt(2.0*(1.0-r)/n) = ( m12/(s12/sqrt(n)) ) * sqrt(2.0*(1.0-r)/n) = m12*sqrt(2*(1-r))/s12
    d = m12*sqrt(2*(1-r))/s12

    output = sprintf("d = %0.2f", d)
    cat(output, "\n", sep = "")
    cat('d [0.20 0.50) = small, [0.50 0.80) = medium, [0.80 ) = large. The sign of d is arbitrary.\n')
    return(invisible(d))
}

#' calculate effect size
#' @description calculate effect size
#' @param m1 mean
#' @param s1 standard deviation
#' @param m2 mean
#' @param s2 standard deviation
#' @param r correlation, In case, the correlation is unknown, please fill in 0.
#' @return returns invisible
#' @export
ez.es.t.paired.msr = function(m1,s1,m2,s2,r) {
    s12 = sqrt( s1*s1 + s2*s2 - 2*r*s1*s2 )
    m12 = m1 - m2
    d = m12*sqrt(2*(1-r))/s12

    output = sprintf("d = %0.2f", d)
    cat(output, "\n", sep = "")
    cat('d [0.20 0.50) = small, [0.50 0.80) = medium, [0.80 ) = large. The sign of d is arbitrary.\n')
    return(invisible(d))
}

#' retrieve article citation numbers from pubmed
#' @description retrieve article citation numbers from pubmed
#' @param xmlFile EndNote library file, contains exported articles with correct titles
#' @param outFile an excel file to store the results, if not provided, same base name as xmlFile
#' @param index 1:10, same syntax titles[index] to choose a subset of titles in xmlFile to process, NULL=all 
#' @return returns invisible, save an excel file with results
#' @note get citation numbers cited by available pubmed central papers
#' @export
ez.citen = function(xmlFile,outFile=NULL,index=NULL){

    if (is.null(outFile)){
        outFile=gsub('xml$','xlsx',xmlFile,perl=TRUE)
    }
    # https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html
    # https://www.ncbi.nlm.nih.gov/books/NBK25501/
    # https://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/XML.pdf

    bibs = XML::xmlParse(file = xmlFile)
    nodes = XML::getNodeSet(bibs,"//title")
    titles = XML::xmlSApply(nodes,XML::xmlValue)
    
    strcomp <- function(s1,s2) {
        s1 = tolower(gsub('\\W','',s1,perl=TRUE))
        s2 = tolower(gsub('\\W','',s2,perl=TRUE))
        return(s1==s2)
    }

    pubmedcites = function(title){
        # title = "Dissociating Normal Aging from Alzheimer's Disease: A View from Cognitive Neuroscience"
        rsearch <- rentrez::entrez_search(db="pubmed", retmax=1, term=title)

        if (rsearch$count==0) {
            # nothing found, give up
            result = list("sortpubdate" = "",
                          "pmcrefcount" = NA_integer_,
                          "sortfirstauthor" = "",
                          "lastauthor" = "",
                          "fulljournalname" = "",
                          "title" = "")
        } else {
            rsum <- rentrez::entrez_summary(db="pubmed", id=rsearch$ids)
            result <- rentrez::extract_from_esummary(rsum, c("sortpubdate", "pmcrefcount", "sortfirstauthor", "lastauthor", "fulljournalname", "title"))
            # if not same title, try title search according to previously returned parsed search term
            if (!strcomp(title,result$title)){
                term=gsub('\\[.*?\\]','[Title]',rsearch$QueryTranslation,perl=TRUE)
                rsearch <- rentrez::entrez_search(db="pubmed", retmax=1, term=term)
                # if fails, give up
                if (rsearch$count==0) {
                    result = list("sortpubdate" = "",
                                  "pmcrefcount" = NA_integer_,
                                  "sortfirstauthor" = "",
                                  "lastauthor" = "",
                                  "fulljournalname" = "",
                                  "title" = "")
                } else {
                    rsum <- rentrez::entrez_summary(db="pubmed", id=rsearch$ids)
                    result <- rentrez::extract_from_esummary(rsum, c("sortpubdate", "pmcrefcount", "sortfirstauthor", "lastauthor", "fulljournalname", "title"))
                }
            }

        }


        result$OriginalTitle = title
        result$RetrievedDate = ez.moment()

        if (strcomp(title,result$title)){
           result$TitleMatch = 1
        } else {
           result$TitleMatch = 0
        }

        return(result)
    }

    if (!is.null(index)) titles=titles[index]
    cites = lapply(titles, pubmedcites)
    results = as.data.frame( t(matrix(unlist(cites), nrow=length((cites[[1]])))) )
    names(results) = names(cites[[1]])
    results = ez.num(results, c('pmcrefcount','TitleMatch'),force=TRUE)
    results$ID = seq.int(nrow(results))
    results = dplyr::arrange(results,desc(pmcrefcount))

    ez.savex(results,file=outFile,withFilter=TRUE)
    return(invisible(results))
}
