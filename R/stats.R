# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stats
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' show information about a data frame or other object, alias of \code{\link{ez.info}}
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @return
#' @examples
#' @seealso \code{\link{ez.view}}
#' @export
ez.show = function(x){
    if (!is.data.frame(x)) {(x = data.frame(x))}
    # flush otherwise not print large text
    # show(x)
    print(Hmisc::describe(x))
    flush.console()
    print(summary(x))
    flush.console()
    cat('--------------------------------------------------------------------------------------------\n')
    str(x)
    flush.console()
    cat('--------------------------------------------------------------------------------------------\n')
    ez.view(x)
}

#' show information about a data frame or other object, alias of \code{\link{ez.show}}
#' @param x a data frame or a vector or sth else that can be converted into a data frame
#' @return
#' @examples
#' @seealso \code{\link{ez.view}}
#' @export
ez.info = ez.show

#' show information about a data frame or similar object (like spss variable view)
#' @description wrapper of \code{\link[sjPlot]{view_df}}; can make the html bigger by openning in internet browser
#' @param
#' @return
#' @seealso \code{\link{ez.info}} or \code{\link{ez.show}}
#' @examples
#' @export
ez.view = function(x, show.frq = T, show.prc = T, sort.by.name = F, ...){
    sjPlot::view_df(x, show.frq = show.frq, show.prc = show.prc, sort.by.name = sort.by.name, ...)
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
#' @param y a vector of outcome variables c('var1','var2'), or a single variable 'var1'
#' @param x a vector of predictors, or a single predictor, (eg, names(select(beta,Gender:dmce)))
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @return an invisible data frame with y,x,p,b and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' @examples
#' @export
ez.regressions = function(df,y,x,pthreshold=.05,showerror=F) {
    results = ez.header('y'=character(),'x'=character(),'p'=numeric(),'b'=numeric())
    for (yy in y) {
        # note a new row needs to have the same column numbers defined in header
        results = ez.append(results,c(yy,'',paste0('n = ',ez.size(df,1)),''))
        for (xx in x) {
            dfdf=ez.2value(df)
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    lm(scale(dfdf[[yy]])~scale(dfdf[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    b = model$coefficients[2,1]
                    if (p < pthreshold) {results = ez.append(results,c(yy,xx,p,b))}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    lm(scale(dfdf[[yy]])~scale(dfdf[[xx]])) %>% summary() ->model
                    p = model$coefficients[2,4]
                    b = model$coefficients[2,1]
                    if (p < pthreshold) {results = ez.append(results,c(yy,xx,p,b))}
                }, error = function(e) {})
            }
        }
        results = ez.append(results,c('','','',''))  # empty line between each y
    }
    return(invisible(results))
}

#' a series of one-way anova, for many y and many x
#' @description aov(ez.2value(df[[yy]])~df[[xx]])
#' @param df a data frame
#' @param y a vector of continous variables c('var1','var2'), or a single variable 'var1', if it is a factor, auto converts to numeric (internally call ez.2value(df[[yy]]), (eg, names(select(beta,Gender:dmce)))
#' @param x a vector of categorical variables, or a single categorical variable
#' @param pthreshold default .05, print/output results whenever p < pthreshold, could be 1 then get all
#' @param showerror whether show error message when error occurs, default F
#' @return an invisible data frame with x,y,p,means and print results out on screen; results can then be saved using ez.savex(results,'results.xlsx')
#' \cr the means column in excel can be split into mulitiple columns using Data >Text to Columns
#' @examples
#' @export
ez.anovas = function(df,y,x,pthreshold=.05,showerror=F) {
    results = ez.header('x'=character(),'y'=character(),'p'=numeric(),'means'=numeric())
    for (xx in x) {
        # note a new row needs to have the same column numbers defined in header
        results = ez.append(results,c(xx,'',paste0('n = ',ez.size(df,1)),''))
        for (yy in y) {
            if (showerror) {
                # try is implemented using tryCatch
                # try(expr, silent = FALSE)
                try({
                    a = aov(ez.2value(df[[yy]])~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    s = aggregate(ez.2value(df[[yy]])~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (p < pthreshold) {results = ez.append(results,c(xx,yy,p,means))}
                    })
            } else {
                # go to next loop item, in case error
                tryCatch({
                    a = aov(ez.2value(df[[yy]])~df[[xx]])
                    p = summary(a)[[1]][["Pr(>F)"]][[1]]
                    s = aggregate(ez.2value(df[[yy]])~df[[xx]],FUN=mean)
                    means = ''
                    for (i in 1:ez.size(s,1)) {means = paste(means,s[i,1],s[i,2],sep='\t')}
                    if (p < pthreshold) {results = ez.append(results,c(xx,yy,p,means))}
                }, error = function(e) {})
            }
        }
        results = ez.append(results,c('','','',''))  # empty line between each x
    }
    return(invisible(results))
}
