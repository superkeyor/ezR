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
    print(dplyr::setequal(lh,rh,...))
}

#' view the overview of a data frame or similar object (like spss variable view, but with much more information)
#' @description Updated: as of Thu, Nov 30 2017, not any more a wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param x a data frame
#' @param file a file name, if NULL, a temp generated, will save more detailed variable information to an excel file
#' @param id a single col name in string or number (eg, 'age' or 3), that serves as (potentially unique) id, except which duplicated rows will be checked against. If NULL, rownames() will be auto used
#' @param width controls if too many factor levels to print, eg 300. NULL=unlimited
#' @param characterize T/F count the element freq of character cols or not 
#' @return returns file path
#' @examples
#' @export
ez.view = function(x, file=NULL, id=NULL, width=NULL, characterize=TRUE, incomparables=FALSE, ...){
    # ez.view = function(x, file=NULL, id=NULL, show.frq = T, show.prc = T, sort.by.name = F, ...){
    # do not need, my own is better
    # sjPlot::view_df(x, show.frq = show.frq, show.prc = show.prc, sort.by.name = sort.by.name, ...)

    # if duplicated col names, the following main codes would crash with weird reasons
    # duplicated row names are fine
    if ( sum(ez.duplicated(colnames(x),vec=TRUE,incomparables=incomparables,dim=1))>0 ) {
        stop(sprintf('I cannot proceed. Duplicated col names foud: %s\n', colnames(x)[which(ez.duplicated(colnames(x),vec=TRUE,incomparables=incomparables,dim=1))] %>% toString))
    }

    temped=F
    if(is.null(file)){
        temped=T
        file=tempfile(pattern = "view_", tmpdir = tempdir(), fileext = ".xlsx")
        on.exit(unlink(file))
    }

    if (!is.null(file)) {
        # row summary
        r.rowname=rownames(x) %>% ez.num()
        if (is.null(id)) {
            idname=rownames(x) %>% ez.num()
        } else {
            idname=x[,id]
        }

        r.duplicated.idname=ez.duplicated(idname,vec=TRUE,incomparables=incomparables,dim=1)
        r.duplicated.idname[which(!r.duplicated.idname)]=NA
        # check duplicated row except the idname column
        if (is.null(id)) {
            r.duplicated.content=ez.duplicated(x,vec=TRUE,incomparables=incomparables,dim=1)
        } else {
          # https://github.com/tidyverse/dplyr/issues/2184
          # to avoid the bug, in case variable name id is the same as one of the column names
          idididid=id
            r.duplicated.content=ez.duplicated(dplyr::select(x,-one_of(idididid)),vec=TRUE,incomparables=incomparables,dim=1)
        }
        r.duplicated.content[which(!r.duplicated.content)]=NA
        
        tmpMatrix = is.na(x)
        r.ncol = rep(ncol(tmpMatrix), nrow(tmpMatrix))
        r.missing = rowSums(tmpMatrix,na.rm=TRUE)
        
        results0=data.frame(rowname=r.rowname,id=idname,duplicated_id=r.duplicated.idname,
                            duplicated_content_except_id=r.duplicated.content,ncol=r.ncol,missing=r.missing)
        results0=dplyr::mutate(results0,missing_rate=missing/ncol)
        


        # col summary
        results = ez.header(variable=character(),class=character(),n=numeric(),missing=numeric(),unique=numeric(),
                            levels_view1=character(),levels_view2=character(),
                            mean=numeric(),min=numeric(),max=numeric(),sum=numeric())
        vars=colnames(x)
        allFactorUniqueValues=character()
        allFactorCounts=integer()
        for (var in vars) {
            v.variable=var
            v.class=class(x[[var]])
            v.n=length(x[[var]])
            v.missing=sum(is.na(x[[var]]))
            v.unique=length(unique(x[[var]]))
            if ( is.factor(x[[var]]) | (is.character(x[[var]]) & characterize) ) {
                v.levels1=dplyr::count_(x,var) %>% 
                    format.data.frame() %>% toString(width=width) %>%  # width controls if too many factor levels
                    gsub('"','',.,fixed = T) %>% gsub('c(','(',.,fixed = T)

                freqtable=dplyr::count_(x,var)
                col1=format.factor(freqtable[[1]])
                col2=as.character(freqtable[[2]])
                v.levels2=paste0(col1,'(',col2,')') %>% toString(width=width)
                allFactorUniqueValues=unique(c(allFactorUniqueValues,unique(freqtable[[1]]) %>% as.character()))
                allFactorCounts=c(allFactorCounts,freqtable[[2]])
            } else {
                v.levels1=v.levels2=NA
            }
            if (is.numeric(x[[var]])) {
                v.mean=mean(x[[var]],na.rm=TRUE)
                v.min=min(x[[var]],na.rm=TRUE)
                v.max=max(x[[var]],na.rm=TRUE)
                v.sum=sum(x[[var]],na.rm=TRUE)
            } else {
                v.mean=v.min=v.max=v.sum=NA
            }
            results = ez.append(results,list(v.variable,v.class,v.n,v.missing,v.unique,v.levels1,v.levels2,v.mean,v.min,v.max,v.sum),print2screen=FALSE)
        }
        v.duplicated.varname=ez.duplicated(colnames(x),vec=TRUE,incomparables=incomparables,dim=1)
        v.duplicated.varname[which(!v.duplicated.varname)]=NA
        v.duplicated.content=ez.duplicated(x,vec=TRUE,incomparables=incomparables,dim=2)
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


        # ez.savex(results0,file)
        wb <- openxlsx::createWorkbook(creator = 'openxlsx')
        openxlsx::addWorksheet(wb, sheetName = "row")
        openxlsx::writeData(wb, 'row', results0, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE, headerStyle = NULL,
          borders = c("none", "surrounding", "rows", "columns", "all"),
          borderColour = getOption("openxlsx.borderColour", "black"),
          borderStyle = getOption("openxlsx.borderStyle", "thin"),
          withFilter = TRUE, keepNA = FALSE)
        openxlsx::addWorksheet(wb, sheetName = "col")
        openxlsx::writeData(wb, 'col', results, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE, headerStyle = NULL,
          borders = c("none", "surrounding", "rows", "columns", "all"),
          borderColour = getOption("openxlsx.borderColour", "black"),
          borderStyle = getOption("openxlsx.borderStyle", "thin"),
          withFilter = TRUE, keepNA = FALSE)
        openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

        # give some time to open the file and then on.exit will delete it
        # although OS will be able to auto clean temp files later on
        # tempdir() is where it is
        if (temped) {browseURL(file);ez.sleep(3)}  
        return(invisible(file))
    }
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
#' @description lm(scale(df[[yy]])~scale(df[[xx]]))
#' @param df a data frame, if its column is factor, auto converts to numeric (internally call ez.2value(df))
#' \cr NA in df will be auto excluded in lm(), reflected by degree_of_freedom
#' @param y a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)), but both mulitple/single x, only simple regression)
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @return an invisible data frame with y,x,p,beta,degree_of_freedom and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' \cr beta: standardized coefficients or beta coefficients are the estimates resulting from a regression analysis that have been standardized 
#' \cr so that the variances of dependent and independent variables are 1.
#' \cr Therefore, standardized coefficients refer to how many standard deviations a dependent variable will change, 
#' \cr per standard deviation increase in the predictor variable. 
#' \cr For simple regression (1 y ~ 1 x), the value of the standardized coefficient (beta) equals the correlation coefficient (r) (beta=r).
#' \cr 
#' \cr degree_of_freedom: from F-statistic
#' @param ... dots passed to ez.2value(df,...)
#' @examples
#' @export
ez.regressions = function(df,y,x,pthreshold=.05,showerror=F,print2screen=T,...) {
    results = ez.header('y'=character(),'x'=character(),'p'=numeric(),'beta'=numeric(),'degree_of_freedom'=numeric())
    for (yy in y) {
        for (xx in x) {
            dfdf=ez.2value(df,...)
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    lm(scale(dfdf[[yy]])~scale(dfdf[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    beta = model$coefficients[2,1]
                    degree_of_freedom = model$df[2]
                    if (p < pthreshold) {results = ez.append(results,list(yy,xx,p,beta,degree_of_freedom),print2screen=print2screen)}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    lm(scale(dfdf[[yy]])~scale(dfdf[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    beta = model$coefficients[2,1]
                    degree_of_freedom = model$df[2]
                    if (p < pthreshold) {results = ez.append(results,list(yy,xx,p,beta,degree_of_freedom),print2screen=print2screen)}
                }, error = function(e) {})
            }
        }
        if (length(x)>1) results = ez.append(results,list('','',NA,NA,NA),print2screen=print2screen)  # empty line between each y
    }
    return(invisible(results))
}

#' a series of one-way anova, for many y and many x
#' @description aov(ez.2value(df[[yy]])~df[[xx]])
#' @param df a data frame
#' \cr NA in df will be auto excluded in aov(), reflected by degree_of_freedom
#' @param y a vector of continous variables c('var1','var2'), or a single variable 'var1', if it is a factor, auto converts to numeric (internally call ez.2value(df[[yy]]), (eg, names(select(beta,Gender:dmce)))
#' @param x a vector of categorical variables, or a single categorical variable
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @param ... dots passed to ez.2value(df[[yy]],...)
#' @return an invisible data frame with x,y,p,means and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' \cr the means column in excel can be split into mulitiple columns using Data >Text to Columns
#' \cr degree_of_freedom: from F-statistic
#' @examples
#' @export
ez.anovas = function(df,y,x,pthreshold=.05,showerror=F,print2screen=T) {
    results = ez.header('x'=character(),'y'=character(),'p'=numeric(),'degree_of_freedom'=character(),'means'=character())
    for (xx in x) {
        for (yy in y) {
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    a = aov(ez.2value(df[[yy]],...)~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    degree_of_freedom = toString(summary(a)[[1]][['Df']])
                    s = aggregate(ez.2value(df[[yy]],...)~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,degree_of_freedom,means),print2screen=print2screen)}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    a = aov(ez.2value(df[[yy]],...)~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    degree_of_freedom = toString(summary(a)[[1]][['Df']])
                    s = aggregate(ez.2value(df[[yy]],...)~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (p < pthreshold) {results = ez.append(results,list(xx,yy,p,degree_of_freedom,means),print2screen=print2screen)}
                }, error = function(e) {})
            }
        }
        if (length(y)>1) results = ez.append(results,list('','',NA,''),print2screen=print2screen)  # empty line between each x
    }
    return(invisible(results))
}
