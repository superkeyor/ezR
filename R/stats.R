# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' print out summary statistics about a data frame or other object, alias of \code{\link[Hmisc]{describe}}
#' @description
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @return
#' @examples
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
#' @description Updated: as of Thu, Nov 30 2017, not any more a wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param df a data frame
#' @param id a single col name in string or number (eg, 'age' or 3), that serves as (potentially unique) id, except which duplicated rows will be checked against. If NULL, rownames() will be auto used
#' @param file a file name, if NULL, a temp generated, will save more detailed variable information to an excel file
#' @param width controls if too many factor levels to print, eg 300. NULL=unlimited
#' @param characterize T/F count the element freq of character cols or not 
#' @return returns a list $row, $col, $dat (input data frame), $pth (file path)
#' @examples
#' @export
ez.view = function(df, id=NULL, file=NULL, width=300, characterize=TRUE, incomparables=FALSE, debug=NULL, ...){
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

    # ez.view = function(df, file=NULL, id=NULL, show.frq = T, show.prc = T, sort.by.name = F, ...){
    # do not need, my own is better
    # sjPlot::view_df(df, show.frq = show.frq, show.prc = show.prc, sort.by.name = sort.by.name, ...)

    # if duplicated col names, the following main codes would crash with weird reasons
    # duplicated row names are fine
    if ( sum(ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1))>0 ) {
        stop(sprintf('I cannot proceed. Duplicated col names foud: %s\n', colnames(df)[which(ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1))] %>% toString))
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

    r.duplicated.idname=ez.duplicated(idname,vec=TRUE,incomparables=incomparables,dim=1)
    r.duplicated.idname[which(!r.duplicated.idname)]=NA
    # check duplicated row except the idname column
    if (is.null(id)) {
        r.duplicated.content=ez.duplicated(df,vec=TRUE,incomparables=incomparables,dim=1)
    } else {
      # https://github.com/tidyverse/dplyr/issues/2184
      # to avoid the bug, in case variable name id is the same as one of the column names
      idididid=id
        r.duplicated.content=ez.duplicated(dplyr::select(df,-one_of(idididid)),vec=TRUE,incomparables=incomparables,dim=1)
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
        v.class=class(df[[var]])
        v.n=length(df[[var]])
        v.missing=sum(is.na(df[[var]]))
        v.unique=length(unique(df[[var]]))
        # countable as levels
        if ( is.factor(df[[var]]) | (is.character(df[[var]]) & characterize) | is.logical(df[[var]]) ) {
            v.levels1=dplyr::count_(df,var) %>% 
                format.data.frame() %>% toString(width=width) %>%  # width controls if too many factor levels
                gsub('"','',.,fixed = T) %>% gsub('c(','(',.,fixed = T)

            freqtable=dplyr::count_(df,var)
            col1=format.factor(freqtable[[1]])
            col2=as.character(freqtable[[2]])
            v.levels2=paste0(col1,'(',col2,')') %>% toString(width=width)
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
    v.duplicated.varname=ez.duplicated(colnames(df),vec=TRUE,incomparables=incomparables,dim=1)
    v.duplicated.varname[which(!v.duplicated.varname)]=NA
    v.duplicated.content=ez.duplicated(df,vec=TRUE,incomparables=incomparables,dim=2)
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

#' print sorted uniques of a df col or a vector (NA last) and other information
#' @description print sorted uniques of a df col or a vector (NA last) and other information
#' @export
view=function(x) {
    v = x
    if (is.data.frame(v)) {
        if ( sum(ez.duplicated(colnames(v),vec=TRUE,dim=1))>0 ) {
            stop(sprintf('I cannot proceed. Duplicated col names foud: %s\n', colnames(v)[which(ez.duplicated(colnames(v),vec=TRUE,incomparables=incomparables,dim=1))] %>% toString))
        }
        v.cols = colnames(v) %>% ez.format.vector(print2screen=F)
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

        cat(v.cols)
        cat(sprintf('\nDim: %d x %d\t#EmptyCols: %d\t#NA: %d\n%s\n', v.nrow, v.ncol, v.n.colNumsAllNAs, v.missing, v.classes))

    } else {
        v.elements = unique(v) %>% sort(na.last=T) %>% ez.format.vector(print2screen=F)
        v.class=class(v)
        v.n=length(v)
        v.missing=sum(is.na(v))
        v.unique=length(unique(v))

        # calculable 
        is.date <- function(x) inherits(x, 'Date')
        # not all NA
        if ( is.numeric(v) & !all(is.na(v)) ) {
            v.mean=mean(v,na.rm=TRUE)
            v.min=min(v,na.rm=TRUE)
            v.max=max(v,na.rm=TRUE)
            v.sum=sum(v,na.rm=TRUE)
        } else if ( is.date(v) & !all(is.na(v)) ) {
            # converted to numeric, to convert back to date: ez.date(ori='R')
            v.mean=mean(v,na.rm=TRUE)
            v.min=min(v,na.rm=TRUE)
            v.max=max(v,na.rm=TRUE)
            # sum not defined for "Date" objects
            v.sum=NA
        } else {
            v.mean=v.min=v.max=v.sum=NA
        }

        cat(v.elements)
        cat(sprintf('\n%s\t#Unique: %d\t#NA: %d (%.0f%%)\t#Total: %d\n', v.class, v.unique, v.missing, v.missing*100/v.n, v.n))
        if ( (is.numeric(v) | is.date(v)) & !all(is.na(v)) ) {
            cat(sprintf('M = %.2f\t(%.2f,%.2f)\t%.2f\n', v.mean, v.min, v.max, v.sum))
        }
    }
    return(invisible(NULL))
}

#' standard error of mean
#' @description na will be omitted before calculation, the formula is sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
#' @param x a vector
#' @return
#' @examples
#' @export
ez.se = function(x) {
    # http://stackoverflow.com/a/7220087/2292993
    sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}

#' a series of simple regression, for many y and many x
#' @description df=ez.2value(df,y,...), df[[xx]]=ez.2value(df[[xx]],...), lm(scale(df[[yy]])~scale(df[[xx]]))
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2value(df))
#' \cr NA in df will be auto excluded in lm(), reflected by degree_of_freedom
#' @param y internally evaluated by eval('dplyr::select()'), a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x internally evaluated by eval('dplyr::select()'), a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @param ... dots passed to ez.2value(df,...)
#' @return an invisible data frame with y,x,p,beta,degree_of_freedom and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' \cr beta: standardized coefficients or beta coefficients are the estimates resulting from a regression analysis that have been standardized 
#' \cr so that the variances of dependent and independent variables are 1.
#' \cr Therefore, standardized coefficients refer to how many standard deviations a dependent variable will change, 
#' \cr per standard deviation increase in the predictor variable. 
#' \cr For simple regression (1 y ~ 1 x), the value of the standardized coefficient (beta) equals the correlation coefficient (r) (beta=r).
#' \cr 
#' \cr degree_of_freedom: from F-statistic
#' @note To keep consistent with other R functions (eg, lm which converts numeric/non-numeric factor to values starting from 0), set start.at=0 in ez.2value(), then factor(1:2)->c(0,1), factor(c('girl','boy'))->c(1,0)
#' \cr in lm() the coding (0,1) vs.(1,2) does not affect slope, but changes intercept (but a coding from 1,2->1,3 would change slope--interval difference matters)
#' @examples
#' @export
ez.regressions = function(df,y,x,pthreshold=.05,showerror=F,print2screen=T,plot=T,...) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))
    results = ez.header('y'=character(),'x'=character(),'p'=numeric(),'beta'=numeric(),'degree_of_freedom'=numeric())
    results4plot = results
    df=ez.2value(df,y,...)
    for (yy in y) {
        for (xx in x) {
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    # nlevels(nonfactor)=0
                    if (nlevels(df[[xx]])>2) ez.pprint(sprintf('col %s has >=3 factor levels, consider dummy coding instead of ez.2value.', xx), color='red')
                    df[[xx]]=ez.2value(df[[xx]],...)
                    lm(scale(df[[yy]])~scale(df[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    beta = model$coefficients[2,1]
                    degree_of_freedom = model$df[2]
                    if (plot) {results4plot = ez.append(results4plot,list(yy,xx,p,beta,degree_of_freedom),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(yy,xx,p,beta,degree_of_freedom),print2screen=print2screen)}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    if (nlevels(df[[xx]])>2) ez.pprint(sprintf('col %s has >=3 factor levels, consider dummy coding instead of ez.2value.', xx), color='red')
                    df[[xx]]=ez.2value(df[[xx]],...)
                    lm(scale(df[[yy]])~scale(df[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    beta = model$coefficients[2,1]
                    degree_of_freedom = model$df[2]
                    if (plot) {results4plot = ez.append(results4plot,list(yy,xx,p,beta,degree_of_freedom),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(yy,xx,p,beta,degree_of_freedom),print2screen=print2screen)}
                }, error = function(e) {})
            }
        }
        if (length(x)>1 & yy!=y[length(y)]) results = ez.append(results,list('','',NA,NA,NA),print2screen=print2screen)  # empty line between each y
    }
    if (plot) {
        results4plot %>% ez.dropna() %>% ggplot(aes(x=x,y=p,fill=y))+
            geom_bar(stat='identity')+
            geom_hline(yintercept = 0.05,color='black',linetype=5)+
            scale_fill_manual(values=rep(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"),100)) %>%
            print()
    }
    return(invisible(results))
}

#' a series of one-way anova, for many y and many x
#' @description df = ez.2value(df,y,...); df = ez.2factor(df,x); aov(df[[yy]]~df[[xx]])
#' @param df a data frame
#' \cr NA in df will be auto excluded in aov(), reflected by degree_of_freedom
#' @param y internally evaluated by eval('dplyr::select()'), a vector of continous variables c('var1','var2'), or a single variable 'var1', if it is a factor, auto converts to numeric (internally call ez.2value(df[[yy]]), (eg, names(select(beta,Gender:dmce)))
#' @param x internally evaluated by eval('dplyr::select()'), a vector of categorical variables, or a single categorical variable
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @param ... dots passed to ez.2value(df[[yy]],...)
#' @return an invisible data frame with x,y,p,means and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' \cr the means column in excel can be split into mulitiple columns using Data >Text to Columns
#' \cr degree_of_freedom: from F-statistic
#' @examples
#' @export
ez.anovas = function(df,y,x,pthreshold=.05,showerror=F,print2screen=T,plot=T,...) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))
    results = ez.header('x'=character(),'y'=character(),'p'=numeric(),'degree_of_freedom'=character(),'means'=character())
    results4plot = results
    df = ez.2value(df,y,...); df = ez.2factor(df,x)
    for (xx in x) {
        for (yy in y) {
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    a = aov(df[[yy]]~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    degree_of_freedom = toString(summary(a)[[1]][['Df']])
                    s = aggregate(df[[yy]]~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (plot) {results4plot = ez.append(results4plot,list(xx,yy,p,degree_of_freedom,means),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,degree_of_freedom,means),print2screen=print2screen)}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    a = aov(df[[yy]]~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    degree_of_freedom = toString(summary(a)[[1]][['Df']])
                    s = aggregate(df[[yy]]~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (plot) {results4plot = ez.append(results4plot,list(xx,yy,p,degree_of_freedom,means),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,degree_of_freedom,means),print2screen=print2screen)}
                }, error = function(e) {})
            }
        }
        if (length(y)>1 & xx!=x[length(x)]) results = ez.append(results,list('','',NA,''),print2screen=print2screen)  # empty line between each x
    }
    if (plot) {
        results4plot %>% ez.dropna() %>% ggplot(aes(x=x,y=p,fill=y))+
            geom_bar(stat='identity')+
            geom_hline(yintercept = 0.05,color='black',linetype=5)+
            scale_fill_manual(values=rep(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"),100)) %>%
            print()
    }
    return(invisible(results))
}

#' a series of fisher.test, for many y and many x
#' @description df=ez.2factor(df,c(x,y)), fisher.test(df[[xx]],df[[yy]])
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2factor(df))
#' \cr NA in df will be auto excluded in fisher.test(), reflected by total
#' @param y internally evaluated by eval('dplyr::select()'), a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x internally evaluated by eval('dplyr::select()'), a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @param width width for toString(countTable,width=width)
#' @return an invisible data frame with x,y,p,counts,total and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' @examples
#' @export
ez.fishers = function(df,y,x,pthreshold=.05,showerror=F,print2screen=T,plot=T,width=300) {
    y=(ez.selcol(df,y)); x=(ez.selcol(df,x))
    results = ez.header('x'=character(),'y'=character(),'p'=numeric(),'counts'=character(),'total'=numeric())
    results4plot = results
    df=ez.2factor(df,c(x,y))
    for (xx in x) {
        for (yy in y) {
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    fisher.test(df[[xx]],df[[yy]]) -> model # by default, pairwise NA auto removed
                    p = model$p.value
                    countTable = table(df[[xx]],df[[yy]])   # by default, pairwise NA auto removed
                    counts = toString(countTable,width=width)
                    total = sum(countTable)
                    if (plot) {results4plot = ez.append(results4plot,list(xx,yy,p,counts,total),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,counts,total),print2screen=print2screen)}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    fisher.test(df[[xx]],df[[yy]]) -> model # by default, pairwise NA auto removed
                    p = model$p.value
                    countTable = table(df[[xx]],df[[yy]])   # by default, pairwise NA auto removed
                    counts = toString(countTable,width=width)
                    total = sum(countTable)
                    if (plot) {results4plot = ez.append(results4plot,list(xx,yy,p,counts,total),print2screen=F)}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,counts,total),print2screen=print2screen)}
                }, error = function(e) {})
            }
        }
        if (length(y)>1 & xx!=x[length(x)]) results = ez.append(results,list('','',NA,'',NA),print2screen=print2screen)  # empty line between each x
    }
    if (plot) {
        results4plot %>% ez.dropna() %>% ggplot(aes(x=x,y=p,fill=y))+
            geom_bar(stat='identity')+
            geom_hline(yintercept = 0.05,color='black',linetype=5)+
            scale_fill_manual(values=rep(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"),100)) %>%
            print()
    }
    return(invisible(results))
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