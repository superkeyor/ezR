###**************************************************.
###*functions for simpler data type.
###**************************************************.
#' alias of TRUE
true <- TRUE
#' alias of TRUE
True <- TRUE

#' alias of FALSE
false <- FALSE
#' alias of FALSE
False <- FALSE

#' alias of \code{\link{class}}
#' @export
z.typeof = class

#' alias of \code{\link{as.numeric}}
#' @export
z.num = as.numeric

#' alias of \code{\link{as.character}}
#' @export
z.str = as.character

#' mimic python join, supports vectorization, wrapper of \code{\link{paste}}
#' @param sep default is nothing
#' @examples
#' z.join('','a',' x ','b') # "a x b"
#' z.join(..., sep = " ", collapse = NULL)
#' z.join(1:12) # same as as.character(1:12)
#' z.join("A", 1:6, sep = "")
#' z.join("Today is", date())
#' @export
z.join = function(sep='',...){
    paste(...,sep=sep)
}



###**************************************************.
###*data frame, matrix functions.
###**************************************************.
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

#' similar to matlab find, find non-zero in a vector
#' @param v logical input
#' @return returns index vector
#' @examples
#' z.find(a>1)
#' @export
z.find = function(v){
    which( if (is.logical(v)) v else v != 0 )
}

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

###**************************************************.
###*simple numerical functions.
###**************************************************.
#' alias of \code{\link{ceiling}}
#' @param
#' @return
#' @examples
#' @export
z.ceil = ceiling

#' alias of \code{\link{floor}}
#' @param
#' @return
#' @examples
#' @export
z.floor = floor

#' alias of \code{\link{trunc}}
#' @param
#' @return
#' @examples
#' @export
z.fix = trunc

#' alias of \code{\link{round}}
#' @param
#' @return
#' @examples
#' @export
z.round = round

#' quotient
#' @param m dividend
#' @param n divider
#' @examples
#' 11 is given as the dividend and 5 the divider, (ie, 11 divided by 5, or divide 11 by 5), then 2(quotient) and 1(remainder).
#' @export
#' @seealso \code{\link{z.remainder}}
z.quotient = function (m,n) {
    return(m%/%n)
}

#' remainder
#' @param m dividend
#' @param n divider
#' @examples
#' 11 is given as the dividend and 5 the divider, (ie, 11 divided by 5, or divide 11 by 5), then 2(quotient) and 1(remainder).
#' @export
#' @seealso \code{\link{z.quotient}}
z.remainder = function (m,n){
    return(m%%n)
}

#' similar to python range (python: left inclusive, right exclusive), wrapper of \code{\link{seq}}, may also consider 1:3
#' @param
#' @return returns a vector (both ends inclusive)
#' @examples
#' z.range(1,3) # 1 2 3, equivalent to 1:3
#' @export
z.range = function(start, stop, step=1){seq(start, stop, by=step)}

#' linspace
#' @param n number of points
#' @return
#' @examples
#' @export
z.linspace = function(start, stop, n){
    seq(start, stop, length=n)
}

#' replicate a matrix, n * n (if m not provided) or n * m
#' @param
#' @return
#' @examples
#' @export
z.repmat = function(a, n, m = n) {
    if (length(a) == 0) return(c())
    if (!is.numeric(a) && !is.complex(a))
        stop("Argument 'a' must be a numeric or complex.")
    if (is.vector(a))
        a <- matrix(a, nrow = 1, ncol = length(a))
    if (!is.numeric(n) || !is.numeric(m) ||
        length(n) != 1 || length(m) != 1)
        stop("Arguments 'n' and 'm' must be single integers.")
    n <- max(floor(n), 0)
    m <- max(floor(m), 0)
    if (n <= 0 || m <= 0)
        return(matrix(0, nrow = n, ncol = m))

    matrix(1, n, m) %x% a  # Kronecker product
}

###**************************************************.
###*structual transformation.
###**************************************************.



#' find with regular expression (case sensitive)
#' @param s string
#' @param pat regular expression pattern
#' @param once match once or many times
#' @param split split or not (see below)
#' @return  returns a list ($start, $end, $match, $split)
#' \cr $start and $end as numeric vectors indicating the start and end positions of the matches.
#' \cr $match contains each exact match
#' \cr $split contains the character vector of splitted strings (split by the matched pattern, ie, the rest that do not match).
#' \cr If no match is found and split=FALSE, all components will be NULL
#' \cr If no match is found and split=TRUE, all components will be NULL except that split will contain the whole string
#' @examples
#' @export
z.regexp = function (s, pat, ignorecase = FALSE, once = FALSE, split = FALSE) {
    stopifnot(is.character(pat), is.character(s))
    if (length(pat) > 1) {
        warning("Only the first string in argument 'pat' is taken.")
        pat <- pat[1]
    }
    if (length(s) > 1) {
        warning("Only the first string in argument 's' is taken.")
        s <- s[1]
    }
    if (is.na(pat) || is.na(s))
        stop("In arguments 'pat' and 's' NA values not allowed.")
    if (once) {
        res <- regexpr(pat, s, ignore.case = ignorecase, perl = TRUE)
    }
    else {
        res <- gregexpr(pat, s, ignore.case = ignorecase, perl = TRUE)[[1]]
    }
    if (length(res) == 1 && res < 0)
        if (split)
            return(list(start = NULL, end = NULL, match = NULL,
                        split = s))
    else return(list(start = NULL, end = NULL, match = NULL,
                     split = NULL))
    rstart <- res
    rend <- rstart + attr(res, "match.length") - 1
    attr(rstart, "match.length") <- attr(rend, "match.length") <- NULL
    rmatch <- substring(s, rstart, rend)
    if (split) {
        n <- nchar(s)
        rs <- c(0, rstart, n + 1)
        re <- c(0, rend, n + 1)
        rsplit <- c()
        for (i in 1:(length(rs) - 1)) {
            if (rs[i + 1] - re[i] > 1)
                rsplit <- c(rsplit, substr(s, re[i] + 1, rs[i +
                                                                1] - 1))
        }
    }
    else {
        rsplit <- NULL
    }
    list(start = rstart, end = rend, match = rmatch, split = rsplit)
}

#' find with regular expression (case insensitive)
#' @param s string
#' @param pat regular expression pattern
#' @param once match once or many times
#' @param split split or not (see below)
#' @return  returns a list ($start, $end, $match, $split)
#' \cr $start and $end as numeric vectors indicating the start and end positions of the matches.
#' \cr $match contains each exact match
#' \cr $split contains the character vector of splitted strings (split by the matched pattern, ie, the rest that do not match).
#' \cr If no match is found and split=FALSE, all components will be NULL
#' \cr If no match is found and split=TRUE, all components will be NULL except that split will contain the whole string
#' @examples
#' @export
z.regexpi = function (s, pat, ignorecase = TRUE, once = FALSE, split = FALSE) {
    # A list with components start and end as numeric vectors indicating the start and end positions of the matches.
    # match contains each exact match, and split contains the character vector of splitted strings.
    # If no match is found all components will be NULL, except split that will contain the whole string if split = TRUE.
    # $start, $end, $match, $split
    z.regexp(s, pat, ignorecase, once, split)
}

#' replace string using regular expression (case sensitive)
#' @param
#' @return
#' @examples
#' @export
z.regexprep = function (s, expr, repstr, ignorecase = FALSE, once = FALSE){
    if (!is.character(s))
        stop("Argument 's' must be a character vector.")
    if (!is.character(expr) || !is.character(repstr) || length(expr) !=
        1 || length(repstr) != 1)
        stop("Arguments 'old' and 'new' must be simple character strings.")
    if (once) {
        sub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
    else {
        gsub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
}

#' replace string using regular expression (case insensitive)
#' @param
#' @return
#' @examples
#' @export
z.regexprepi = function (s, expr, repstr, ignorecase = TRUE, once = FALSE){
    if (!is.character(s))
        stop("Argument 's' must be a character vector.")
    if (!is.character(expr) || !is.character(repstr) || length(expr) !=
        1 || length(repstr) != 1)
        stop("Arguments 'old' and 'new' must be simple character strings.")
    if (once) {
        sub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
    else {
        gsub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
}

#' reshape matrix
#' @param
#' @return reshape(a, n, m) returns the n-by-m matrix whose elements are taken column-wise from a.
#' \cr An error results if a does not have n*m elements. If m is missing, it will be calculated from n and the size of a.
#' @examples
#' @export
z.reshape = function (a, n, m){
    if (missing(m))
        m <- length(a)%/%n
    if (length(a) != n * m)
        stop("Matrix 'a' does not have n*m elements")
    dim(a) <- c(n, m)
    return(a)
}

#' apply array function
#' @param
#' @return
#' @examples
#' @export
z.arrayfun = function (func, ...){
    dots <- list(...)
    if (length(dots) < 1)
        stop("Empty list of arrays: Rsult cannot be computed.")
    d <- dim(dots[[1]])
    r <- mapply(func, ...)
    dim(r) <- d
    return(r)
}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# datafile, plot
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' read csv table, alias of \code{\link{read.csv}}
#' @param
#' @return returns a data frame
#' @export
#' @examples
#' read.table(file, header = FALSE, sep = "",
#'            dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
#'            row.names, col.names, as.is = !stringsAsFactors,
#'            na.strings = "NA", colClasses = NA, nrows = -1,
#'            skip = 0, check.names = TRUE, fill = !blank.lines.skip,
#'            strip.white = FALSE, blank.lines.skip = TRUE,
#'            comment.char = "#",
#'            allowEscapes = FALSE, flush = FALSE,
#'            stringsAsFactors = default.stringsAsFactors(),
#'            fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)
#'
#' read.csv(file, header = TRUE, sep = ",",
#'          dec = ".", fill = TRUE, comment.char = "", ...)
z.read = read.csv


#' wrapper of write.csv, but with row.names removed
#' @param
#' @return
#' @examples
#' (x, file="RData.csv", row.names=FALSE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
z.save = z.write = function(x, file="RData.csv", row.names=FALSE, ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    x = data.frame(x)
    rownames(x) <- NULL
    write.csv(x=x, file=file, row.names=row.names, ...)
}

#' read an xlsx file
#' @param
#' @return
#' @examples
#' read.xlsx(file, sheetIndex, sheetName=NULL, rowIndex=NULL,
#'           startRow=NULL, endRow=NULL, colIndex=NULL,
#'           as.data.frame=TRUE, header=TRUE, colClasses=NA,
#'           keepFormulas=FALSE, encoding="unknown", ...)
#' colClasses: Only numeric, character, Date, POSIXct, column types are accepted
#' colClasses=c("Date", "character","integer", rep("numeric", 2),  "POSIXct")
#' @export
z.readx = function(file, sheetIndex=1, ...){
    result = xlsx::read.xlsx(file, sheetIndex, ...)
    return(result)
}

#' read spss .sav file with foreign package
#' @param
#' @return
#' @examples
#' (file, valuelabel=TRUE,tolower=FALSE)
#' label means variable value label, not variable label
#' if TRUE, varialbe becomes a factor, and its levels becomes character labels
#' ie, without label False gender=1,2; with label True gender=Male,Female
#' tolower, whether convert all variable names to lower case
#'
#' alternatively, one can use SPSS R plugin to pass data between SPSS and R.
#' @export
z.reads2 = function(file, valuelabel=TRUE, tolower=FALSE){
    # can safely ignore the warnings about type 7 and etc; data is not lost
    # # http://stackoverflow.com/questions/3136293/read-spss-file-into-r

    # for variable label, it is possible to hack
    # because the function stores read-in info in attributes, see
    # http://www.r-bloggers.com/migrating-from-spss-to-r-rstats/
    # http://stackoverflow.com/questions/19914962/
    result = suppressWarnings(foreign::read.spss(file, use.value.labels = valuelabel, to.data.frame = TRUE,
                                        max.value.labels = Inf, trim.factor.names = TRUE,
                                        trim_values = TRUE, reencode = NA, use.missings = to.data.frame))
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' save an xlsx file
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
z.savex = z.writex = function(x, file="RData.xlsx", sheetName="Sheet1", row.names = FALSE, ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    # require('xlsx')
    x = data.frame(x)
    rownames(x) <- NULL
    xlsx::write.xlsx2(x=x, file=file, sheetName=sheetName, ..., row.names=row.names)
    # detach("package:xlsx", unload=TRUE)
}

#' show the content of a file in read-only mode
#' @param
#' @return
#' @examples
#' @export
z.type = function(path){
    result = file.show(path,title='File (read-only)')
}

#' edit a file
#' @param
#' @return
#' @examples
#' @export
z.edit = function(path){
    result = file.edit(path)
}

#' Prints output to both terminal and a file (log.txt) globally.
#' @param mode a=append; w=overwrite
#' @return
#' @examples
#' @export
z.log = function(file='log.txt',mode='a',status=TRUE){
    append = ifelse(mode=='a',TRUE,FALSE)
    if (status) {
        sink(file,append=append,split=TRUE)
        print(sprintf('log on at %s...', date()))
    }
    else {
        cat(sprintf('log off at %s...\n\n', date()))
        sink()
    }
}

#' alias of pdf
#' @param
#' @return
#' @examples
#' first z.export(filename) to create a new canvas
#' then plot all following graphs to it and
#' finally dev.off()/z.clean(3) to unlink

#' pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
#' width, height, onefile, family, title, fonts, version,
#' paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
#' useDingbats, useKerning, fillOddEven, compress)

#' i = 100
#' fileName = z.join('plot', i, '.pdf')
#' @export
z.export = pdf


#' subplot, wrapper of \code{\link{par}}
#' @param
#' @return
#' @examples
#' subplot(n,m,...) divides the current figure into an n-by-m grid
#' A vector of the form c(n, m). Subsequent figures will be drawn in an n-by-m array on the device
#' by columns (mfcol), or rows (mfrow), respectively.
#' see more ?par
#' @export
z.subplot = function(n, m, ...){
    par(mfrow=c(n,m), ...)
}

#' embed a new plot within an existing plot at the coordinates specified (in user units of the existing plot)
#' @param
#' @return
#' @examples
#' see from http://cran.r-project.org/web/packages/TeachingDemos/TeachingDemos.pdf
#' # e.g.,
#' # 3 rows subplots
#' par(mfrow=c(3,1) )

#' plot(1:10, 1:10, main = "Plot 1")
#' # draw another arbitrary subplot
#' z.embed(plot(10,10, xlab="", ylab=""), x=2, y=8, size = c(0.5, 0.5) )

#' # demonstrates mfg usage, draw the third plot before the drawing the second
#' par(mfg=c(3,1) ); plot(11:20, 11:20, main = "Plot 3")
#' z.embed(plot(10,10, xlab="", ylab=""), x=12, y=18, size = c(0.5, 0.5))

#' par(mfg=c(2,1)); plot(21:30, 21:30, main = "Plot 2")
#' z.embed(plot(10,10, xlab="", ylab=""), x=22, y=28, size = c(0.5, 0.5))
#' @export
z.embed = function(fun, x, y=NULL, size=c(1,1), vadj=0.5, hadj=0.5,
                   inset=c(0,0), type=c('plt','fig'), pars=NULL){
    type <- match.arg(type)
    old.par <- par( c(type, 'usr', names(pars) ) )
    on.exit(par(old.par))
    if(missing(x)) x <- locator(2)
    if(is.character(x)) {
        if(length(inset) == 1) inset <- rep(inset,2)
        x.char <- x
        tmp <- par('usr')
        x <- (tmp[1]+tmp[2])/2
        y <- (tmp[3]+tmp[4])/2
        if( length(grep('left',x.char, ignore.case=TRUE))) {
            x <- tmp[1] + inset[1]*(tmp[2]-tmp[1])
            if(missing(hadj)) hadj <- 0
        }
        if( length(grep('right',x.char, ignore.case=TRUE))) {
            x <- tmp[2] - inset[1]*(tmp[2]-tmp[1])
            if(missing(hadj)) hadj <- 1
        }
        if( length(grep('top',x.char, ignore.case=TRUE))) {
            y <- tmp[4] - inset[2]*(tmp[4]-tmp[3])
            if(missing(vadj)) vadj <- 1
        }
        if( length(grep('bottom',x.char, ignore.case=TRUE))) {
            y <- tmp[3] + inset[2]*(tmp[4]-tmp[3])
            if(missing(vadj)) vadj <- 0
        }
    }
    xy <- xy.coords(x,y)
    if(length(xy$x) != 2){
        pin <- par('pin')
        tmpx <- grconvertX( xy$x[1], to='npc' )
        tmpy <- grconvertY( xy$y[1], to='npc' )
        x <- c( tmpx - hadj*size[1]/pin[1],
                tmpx + (1-hadj)*size[1]/pin[1] )
        y <- c( tmpy - vadj*size[2]/pin[2],
                tmpy + (1-vadj)*size[2]/pin[2] )
        xyx <- grconvertX(x, from='npc', to='nfc')
        xyy <- grconvertY(y, from='npc', to='nfc')
    } else {
        xyx <- grconvertX(x, to='nfc')
        xyy <- grconvertY(y, to='nfc')
    }

    par(pars)
    if(type=='fig'){
        par(fig=c(xyx,xyy), new=TRUE)
    } else {
        par(plt=c(xyx,xyy), new=TRUE)
    }
    fun
    tmp.par <- par(no.readonly=TRUE)

    return(invisible(tmp.par))
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# environment, file, folder, os
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' alias of \code{\link{library}}
#' @param
#' @return
#' @examples
#' @export
z.import = library

#' alias of \code{\link{library}}
#' @param
#' @return
#' @examples
#' @export
z.include = z.import

#' alias of \code{\link{install.packages}}
#' @param
#' @return
#' @examples
#' @export
z.install = install.packages

#' alias of \code{\link{remove.packages}}
#' @param
#' @return
#' @examples
#' @export
z.remove = remove.packages

#' alias of \code{\link{remove.packages}}
#' @param
#' @return
#' @examples
#' @export
z.uninstall = remove.packages

#' alias of \code{\link{system}}
#' @param
#' @return
#' @examples
#' @export
z.execute = system

#' alias of \code{\link{file.path}}
#' @param
#' @return
#' @examples
#' paste(..., sep=.Platform$file.sep)
#' @export
z.joinpath = file.path

#'
#' @param
#' @return
#' @examples
#' z.splitpath(path)
#' returns
#' $dir
#' [1] "/Users/jerry/Downloads"
#'
#' $file
#' [1] "026999397379845a"
#'
#' $ext
#' '.pdf
#' @export
z.splitpath = function(path){
    dir = dirname(path)
    file = basename(path)
    # file = strsplit(file, "\\.")[[1]][1]
    file = tools::file_path_sans_ext(file)
    # ext = tools::file_ext(path)  # does not have . i.e., 'pdf' instead of '.pdf'
    ext = substring(basename(path),nchar(file)+1)  # substring to extract ext, see ?substring
    return(list(dir=dir, file=file, ext=ext))
}

#' parentdir
#' @param
#' @return
#' @examples
#' @export
z.parentdir = function(path){dirname(dirname(path))}

#' alias of \code{\link{setwd}}
#' @param
#' @return
#' @examples
#' @export
z.cd = setwd;

#' alias of \code{\link{getwd}}
#' @param
#' @return
#' @examples
#' @export
z.cwd = getwd

#' alias of \code{\link{getwd}}
#' @param
#' @return
#' @examples
#' @export
z.pwd = getwd

#' current script dir
#' @param
#' @return
#' @examples
#' works only if the script was \code{source}d
#' or run with \code{Rscript} or using the \code{--file} parameter to the
#' \code{R} executable.
#' if couldn't determined, return NULL
#' @export
z.csd <- function() {
    # http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
    if (!is.null(res <- .thisfile_source())) res
    else if (!is.null(res <- .thisfile_rscript())) dirname(res)
    else NULL
}
# Helper functions
.thisfile_source <- function() {
    for (i in -(1:sys.nframe())) {
        if (identical(sys.function(i), base::source))
            return (normalizePath(sys.frame(i)$ofile))
    }

    NULL
}
.thisfile_rscript <- function() {
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    cmdArgsTrailing <- commandArgs(trailingOnly = TRUE)
    cmdArgs <- cmdArgs[seq.int(from=1, length.out=length(cmdArgs) - length(cmdArgsTrailing))]
    res <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmdArgs)

    # If multiple --file arguments are given, R uses the last one
    res <- tail(res[res != ""], 1)
    if (length(res) > 0)
        return (res)

    NULL
}

#' clear, clean
#' @param
#' @return
#' @examples
#' @export
z.clear = function(area=0) {
    # clear console, workspace, plot
    # area = 0 all, 1 console only, 2 workspace only, 3 plot only
    if (area == 0) {console = TRUE; workspace = TRUE; plot = TRUE}
    if (area == 1) {console = TRUE; workspace = FALSE; plot = FALSE}
    if (area == 2) {console = FALSE; workspace = TRUE; plot = FALSE}
    if (area == 3) {console = FALSE; workspace = FALSE; plot = TRUE}
    if (console) {
        # cat("\014")  # clear console ctrl+l
        cat(rep("\n",50)) # "visually" clear
    }
    if (workspace) {
        # rm(list=ls()) # clear workspace
        # rm(list = ls(all=TRUE, .GlobalEnv), envir = .GlobalEnv) # clear also .hidden objects
        rm(list = ls(.GlobalEnv), envir = .GlobalEnv)
    }
    if (plot) {
        try(dev.off(), silent=TRUE)  # clear plot
        # dev.off(dev.list()["RStudioGD"])
    }
    null = gc()  # call garbage collection
}

#' clear, clean
#' @param
#' @return
#' @examples
#' @export
z.clean = z.clear

#' alias of \code{\link{find}}
#' @param
#' @return
#' @examples
#' @export
z.which = find

#' alias of \code{\link{objects}}
#' @param
#' @return returns/gives the names of the objects in the specified environment
#' @examples
#' @export
z.whos = objects

#' alias of \code{\link{search}}
#' @param
#' @return
#' @examples
#' @export
# user path like in Matlab
z.path = search

#' lsd
#' @param
#' @return
#' @examples
#' can also include .folders
#' all.files--hidden files, include.dirs--subdirs, no..--. and .. folders
#' @export
z.lsd = function(path='.', pattern=NULL){
    folders = dir(path=path, pattern=pattern, all.files=TRUE, full.name=FALSE,
                  recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    fullFolders = file.path(path,folders)
    infos = file.info(fullFolders)
    isdirs = infos$isdir
    return(folders[isdirs])
}

#' ls
#' @param
#' @return
#' @examples
#' @export
z.ls = function(path='.', pattern=NULL){
    files = list.files(path = path, pattern = pattern, all.files = TRUE,
                       full.names = TRUE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    isfiles = !(file.info(files)$isdir)
    return(files[isfiles])
}

#' fls
#' @param
#' @return
#' @examples
#' @export
z.fls = function(path='.', pattern=NULL){
    files = list.files(path = path, pattern = pattern, all.files = TRUE,
                       full.names = TRUE, recursive = TRUE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    isfiles = !(file.info(files)$isdir)
    return(files[isfiles])
}

#' mkdir, no warning for exisiting folder
#' @param
#' @return
#' @examples
#' @export
z.mkdir = function(path){
    result = dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

#' alias of \code{\link{file.exists}}
#' @param
#' @return
#' @examples
#' @export
z.exists = file.exists

#' remove a file, wrapper of \code{\link{unlink}}
#' @param
#' @return
#' @examples
#' support c('a.txt','b.txt')
#' x, a character vector with the names of the file(s) or directories to be deleted.
#' Wildcards (normally ‘*’ and ‘?’) are allowed.
#' 0 for success, 1 for failure.
#' @export
z.rm = function(x){
    result = unlink(x, recursive = TRUE, force = TRUE)
}

#' rename
#' @param
#' @return
#' @examples
#' support c('a.txt','b.txt'), c('d.txt','e.txt')
#' to parent folder must exist already; otherwise error
#' in case new name exists
#'       if old and new both folders, move old to new as subfolder
#'       if old and new both files, overwrite the new file with old file without prompt
#' @export
z.rn = function(from,to){
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) stop("Destination parent folder does not exist")
    # both from and to are exisiting folders
    if ((nchar(tools::file_ext(from)) == 0) && (nchar(tools::file_ext(to)) == 0)) {
        if ((isTRUE(file.info(from)$isdir)) && (isTRUE(file.info(to)$isdir))) {
            result = z.mv(from,to)
        }
        else {
            result = file.rename(from,to)
        }
    }
    else {
        result = file.rename(from,to)
    }
}

#' copy 
#' @param
#' @return
#' @examples
#' support c('a.txt','b.txt')
#' to folder does not have to exist already
#' e.g.,
#' 1) both works z.cp('a.txt','folder'), z.cp('a.txt','folder/b.txt')
#' the former copy still has the same name 'a.txt', the latter copy new name 'b.txt'
#' also z.cp(c('a.txt','b.txt'),'folder')
#' 2) folder: z.cp('a','b')-->if b not exists, cp contents of a to b; if b exist, a becomes subfolder of b
#' kinda combines rn and mv
#' 3) regular expression
#' flist <- list.files("patha", "^filea.+[.]csv$", full.names = TRUE)
#' file.copy(flist, "pathb")
#' @export
z.cp = function(from,to){
    # if from is file
    if (!(file.info(from)$isdir[1])) {
        # both works file.copy('a.txt','folder'), file.copy('a.txt','folder/b.txt')
        # the former copy still has the same name 'a.txt', the latter copy new name 'b.txt'

        # if to is folder-like
        if (nchar(tools::file_ext(to)) == 0) {
            # if to not exist
            if (!isTRUE(file.info(to)$isdir)) dir.create(to, recursive=TRUE)
        }
        else {
            # if to parent not exist
            todir <- dirname(to)
            if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
        }
        result = file.copy(from, to, overwrite = TRUE, recursive = FALSE)
    }
    else {
        # if to exist, from becomes a subfolder of to
        if (isTRUE(file.info(to)$isdir)) {
            result = file.copy(from, to, overwrite = TRUE, recursive = TRUE)
        }
        # else copy each individual file
        else {
            dir.create(to, recursive=TRUE)
            allFiles = dir(from)
            result = sapply(allFiles, function(x) {
                file.copy(from=file.path(from, x),
                          to=file.path(to, x),
                          overwrite=TRUE) })
        }
    }
}

#' move
#' @param
#' @return
#' @examples
#' support c('a.txt','b.txt')
#' to parent folder does not have to exist already
#' z.mv('a.txt','folder'), z.mv('a.txt','folder/a.txt'), z.mv('a.txt','folder/b.txt')
#' z.mv('a','b')-->get b/a, b now has a as subfolder, regardless of b exists or not
#'                  use z.rn('a','b') to change name a->b
#' @export
z.mv = function(from,to){
    # if to is folder-like
    if (nchar(tools::file_ext(to)) == 0) {
        # if to not exist
        if (!isTRUE(file.info(to)$isdir)) dir.create(to, recursive=TRUE)
        to = file.path(to, basename(from))
    }
    else {
        # if to parent not exist
        todir <- dirname(to)
        if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    }
    result = file.rename(from = from, to = to)
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# misc
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# options(warn=-1)  # turn off warning
# options(warn=0)  # turn back on warning

#' random number, alias of \code{\link{runif}}
#' @param
#' @return
#' @examples
#' runif(n, min = 0, max = 1)
#' @export
z.rand = runif

#' alias of \code{\link{stop}}
#' @param
#' @return
#' @examples
#' @export
z.error = stop

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

#' wrapper of cat
#' @param
#' @return
#' @examples
#' @seealso \code{\link{sprintf}},  \code{\link{print}}
#' @export
z.print = function(...){
    cat(..., "\n")
}