###**************************************************.
###*functions for simple data type.
###**************************************************.
#' not
#' @description not
#' @export
not = magrittr::not

#' and
#' @description and
#' @export
and = magrittr::and

#' or
#' @description or
#' @export
or = magrittr::or

#' alias of \code{\link{class}}
#' @description alias of \code{\link{class}}
#' @export
ez.typeof = class

#' alias of \code{\link{Sys.sleep}}, in seconds
#' @description alias of \code{\link{Sys.sleep}}, in seconds
#' @seealso \code{\link{ez.pause}}
#' @export
ez.sleep = Sys.sleep

#' moment
#' @description current date,time in string
#' @param format see example or https://www.stat.berkeley.edu/~s133/dates.html
#' @return returns a string
#' @examples
#' # %a  Abbreviated weekday 
#' # %A  Full weekday
#' # %b  Abbreviated month   
#' # %B  Full month
#' # %c  Locale-specific date and time   
#' # %d  Decimal date
#' # %H  Decimal hours (24 hour) 
#' # %I  Decimal hours (12 hour)
#' # %j  Decimal day of the year 
#' # %m  Decimal month
#' # %M  Decimal minute  
#' # %p  Locale-specific AM/PM
#' # %S  Decimal second  
#' # %U  Decimal week of the year (starting on Sunday)
#' # %w  Decimal Weekday (0=Sunday)  
#' # %W  Decimal week of the year (starting on Monday)
#' # %x  Locale-specific Date    
#' # %X  Locale-specific Time
#' # %y  2-digit year    
#' # %Y  4-digit year
#' # %z  Offset from GMT 
#' # %Z  Time zone (character)
#' @export
ez.moment = function(format='%Y-%m-%d_%H-%M-%S') {
    moment = Sys.time()
    result = format(moment,format)
    return(result)
}

#' pause the execution of an R script until a user presses the Enter key, no parameter () needed
#' @description pause the execution of an R script until a user presses the Enter key, no parameter () needed
#' @seealso \code{\link{ez.sleep}}
#' @export
ez.pause = function(){
    # https://diego.assencio.com/?index=86c137b502561d44b8be02f06d80ee16
    if (interactive())
    {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else
    {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}

#' convert a column (or all columns) in a data frame, or a vector into numeric type, call type.convert or as.numeric
#' @description convert a column (or all columns) in a data frame, or a vector into numeric type, call type.convert or as.numeric
#' @param x a character vector, data frame, list, or a factor
#' @param col internally evaluated by eval('dplyr::select()')()
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all cols in x
#' \cr        if x is not a data frame, col is ignored
#' @param force T/F, if force, will try to convert everything (factor, etc) to character first then to numeric, (no warning for NA coerce)
#' \cr else only convert a vec/col that only has string of num, eg '1'->1, see example for "gentle" output 
#' @details Both value and variable label attributes will be removed when converting variables to characters.
#' @return returns a converted vector, data frame
#' \cr with \code{\link{ez.2value}} if x is a factor with chars, will be converted to 1 2 3 etc, see its example
#' \cr \code{\link{ez.num}} keeps the same char as is
#' @details see \url{http://stackoverflow.com/a/22701462/2292993}
#' @seealso \code{\link{ez.2value}}
#' @family data transformation functions
#' @export
#' @examples
#' ez.num(c(1,'2','a',3,NA))          # -> same chara vector
#' ez.num(c(1,'2',3,NA))              # -> 1 2 3 NA int vector
#' ez.num(factor(c(1,'2','a',3,NA)),force=T)  # -> 1 2 NA 3 NA int (warning is suppressed)
#' ez.num(factor(c(1,'2','a',3, NA)),force=F)  # factor['1','2','a','3', 'NA']
#' ez.num(factor(c(1,'2',3, NA)),force=F)  # factor['1','2','3', NA]
#' ez.num(factor(c(1,'2',3,NA)),force=T)  # 1 2 3 NA int
#' 
#' d <- data.frame(char = letters[1:5], 
#'                 fake_char = as.character(1:5), 
#'                 fac = factor(1:5), 
#'                 char_fac = factor(letters[1:5]), 
#'                 mixed_char = c(1,2,3,'4','f'),
#'                 num = 1:5, stringsAsFactors = FALSE)
#' #   char fake_char fac char_fac mixed_char num
#' # 1    a         1   1        a          1   1
#' # 2    b         2   2        b          2   2
#' # 3    c         3   3        c          3   3
#' # 4    d         4   4        d          4   4
#' # 5    e         5   5        e          f   5
#' sapply(d, class)
#' #        char   fake_char         fac    char_fac  mixed_char         num 
#' # "character" "character"    "factor"    "factor" "character"   "integer" 
#' sapply(ez.num(d), class)
#'        char   fake_char         fac    char_fac  mixed_char         num 
#' "character"   "integer"    "factor"    "factor" "character"   "integer" 
ez.num = function(x, col=NULL, force=FALSE, ...){
    if (is.factor(x)) {
        # http://stackoverflow.com/a/22701462/2292993
        # as.numeric(factor(5:10)) # not what you'd expect
        # f <- factor(1:5)
        # ## what you typically meant and want:
        # as.numeric(as.character(f))
        # ## the same, considerably (for long factors) more efficient:
        # as.numeric(levels(f))[f]
        # you get warning
       if (force) result = suppressWarnings(as.numeric(levels(x))[x]) else result=x
    } else if (is.data.frame(x) && is.null(col)) {
        # check before is.list, because a data frame is a list, but not the other way
        # https://stackoverflow.com/a/33050704/2292993
        if (!force) {
            x[] = rapply(x, utils::type.convert, classes = "character", how = "replace", as.is = TRUE)
        } else {
            x = dplyr::mutate_all(x, funs(suppressWarnings(as.numeric(as.character(.)))))
        }
        result = x
    } else if (is.data.frame(x) && !is.null(col)) {
        col=(ez.selcol(x,col))
        cols=col
        for (col in cols) {
            # recursive to is.data.frame(x) && is.null(col)
            x[col] = ez.num(x[col],force=force)
            result=x
        }
    } else if (is.list(x)){
        result = utils::type.convert(as.character(unlist(x)), as.is = TRUE, ...)
    } else {
        # cannot pass factor to type.convert()
        # utils::type.convert(c(1,2,'3'), as.is = F) -> int
        # utils::type.convert(c(1,2,'3'), as.is = T) -> int
        # utils::type.convert(c(1,2,'a'), as.is = F) -> fac  ['1' '2' 'a']
        # utils::type.convert(c(1,2,'a'), as.is = T) -> vec  ('1' '2' 'a')
        if (!force) result = utils::type.convert(x, as.is = TRUE, ...) else result=suppressWarnings(as.numeric(x))
    }
    return(result)
}

#' convert to date
#' @description convert to date with friendly ori (no need to remember exact origin date)
#' @param x a vector of char, num. param format for class 'character' (ignore ori); param ori for class 'numeric' (ignore format)
#' \cr a string factor treated as char (ie, using param format), a num factor cannot be processed
#' @param ori one of 'Excel', 'Matlab', 'R', 'SPSS'
#' @param format specify in date format. see examples
#' @examples 
#' # format one of c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%m/%d/%Y", "%Y-%m-%d"). %y for two year digits
#' @return returns a vector
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.convertible}} \code{\link{ez.age}} \code{\link{ez.time}}
#' @export
ez.date = function(x,ori="Excel",format="%m/%d/%Y",...) {
    # from R help as.Date()
    if (is.numeric(x)) {
        if (ori=='Excel') origin="1899-12-30"
        # if (ori=='Excel.Mac') origin="1904-01-01"  # tested on my mac, seems the same as Excel.Win
        if (ori=='Matlab') origin="1970-01-01"
        if (ori=='R') origin="1970-01-01"
        if (ori=='SPSS') {
            # http://scs.math.yorku.ca/index.php/R:_Importing_dates_from_SPSS
            # 24*60*60=86400
            x = x/86400
            origin = "1582-10-14"
        }
        result = as.Date(x,origin=origin,...)
        if (ori=='Matlab') result=result-719529
    }
    if (is.character(x) | is.factor(x)) {
        result=as.Date(x,format=format,...)
    }
    return(result)
}

#' convert to time
#' @description convert from a time to class (chron) times (17:48:00--military only, format specified by out.format) or class numeric (0.7416667--fractions of a day): 
#' \cr \cr SPSS military time (civilian time not supported) specified in date format, 17:48 read into R as 64080 (seconds of a day), or 
#' \cr \cr Excel military/civilian time specified in time format, 5:48:00 PM, 17:48:00 read into R as 0.7416667 (fractions of a day), or
#' \cr \cr string "17:48:00" (must have hour, min, seconds. with specified param format)
#' @param x a vector of number or character
#' @param ori one of 'Excel', 'SPSS' (ignored if x is character)
#' @param format input format, eg, "h:m:s", ignored if x is numeric
#' @param out.type string, 'numeric' (fractions of a day) or 'times'/'time'
#' @param out.format  "h:m:s", 'h_m_s' (must have h m s), ignored if out.type is numeric
#' @return returns a vector of number (class numeric) or time (class times). class times can be passed to as.character(.), or substr(.,1,5), or as.numeric(.), see more at \code{\link[chron]{chron}}
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.convertible}} \code{\link{ez.age}} \code{\link{ez.time}}
#' @export
ez.time = function(x,ori='SPSS',format="h:m:s",out.type='numeric',out.format="h:m:s",...) {
    format <- c(dates = "m/d/y", times = format)
    out.format <- c(dates = "m/d/y", times = out.format)
    if (is.numeric(x)) {
        if (ori=='SPSS') {
            if (out.type %in% c('time','times')) {
                # https://stackoverflow.com/a/39208186/2292993
                result = chron::times(x/(24*60*60),out.format=out.format,...)
            }
            if (out.type=='numeric') {
                result = x/(24*60*60)
            }
        }
        if (ori=='Excel') {
            # https://stackoverflow.com/a/28044345/2292993
            if (out.type %in% c('time','times')) result = chron::times(x,out.format=out.format,...)
            if (out.type=='numeric') result = x
        }
    }
    if (is.character(x) | is.factor(x)) {
        result=chron::chron(times.=x,format=format,out.format=out.format,...)
        if (out.type=='numeric') result = as.numeric(result)
    }
    return(result)
}

#' check if a vector is already stored as a date type
#' @description check if a vector is already stored as a date type
#' @param x a vector, no need to specify date formats
#' @return returns a single T/F (not vectorized). If x is a date, is.numeric(x) is FALSE.
#' @examples
#' mydate = c("10/11/2012","10/12/2012")
#' # format one of c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%m/%d/%Y", "%Y-%m-%d"). %y for two year digits
#' mydate = as.Date(mydate,format = "%m/%d/%Y")  # "2012-10-11" "2012-10-12"
#' ez.is.date(mydate)  # T
#' @export
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.convertible}} \code{\link{ez.age}} \code{\link{ez.time}}
ez.is.date = function(x) {
    # https://stackoverflow.com/a/37062951/2292993
    return( inherits(x, 'Date') )
}

#' check if a vector of char, number is convertiable to date type
#' @description check if a vector of char, number is convertiable to date type as.Date(as.character(x), format)
#' @param x a vector of char, number
#' @param format specify date format. see examples
#' @param ... other parameters passed to as.Date(...) 
#' @return returns a vector of T/F (vectorized because of is.na() used).
#' @examples
#' mydate = c("10/11/2012","10-12-2012", 345)
#' # format one of c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%m/%d/%Y", "%Y-%m-%d"). %y for two year digits
#' ez.is.date.convertible(mydate, format = "%m/%d/%Y")  # T F F
#' @export
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.convertible}} \code{\link{ez.age}} \code{\link{ez.time}}
ez.is.date.convertible = function(x,format="%m/%d/%Y",...) {
    # https://stackoverflow.com/a/37062951/2292993
    result = !is.na( as.Date(as.character(x), format = format, ...) )
    return( result )
}

#' convert a column (or all columns) in a data frame, or a vector into character type, call as.character
#' @description convert a column (or all columns) in a data frame, or a vector into character type, call as.character
#' @param x a data frame or a vector/col
#' @param col internally evaluated by eval('dplyr::select()')()
#' \cr        if x is a data frame, col is specified (e.g., "cond"), convert that col only
#' \cr        if x is a data frame, col is unspecified (i.e., NULL default), convert all cols in x
#' \cr        if x is a data frame, col is NA (a special case), convert all numeric NAs to character NAs, for bind_rows. see https://github.com/tidyverse/dplyr/issues/2584
#' \cr        if x is not a data frame, col is ignored
#' @details Both value and variable label attributes will be removed when converting variables to characters.
#' @examples
#'
#' @return returns a character vector or a data frame with changed col(s)
#' @family data transformation functions
#' @export
#' @seealso \code{\link{ez.2char}}
ez.str = function(x, col=NULL){
    if (is.data.frame(x) && is.null(col)){
        result = dplyr::mutate_all(x, as.character)
    } else if (is.data.frame(x) && is.na(col)) {
        # convert all numeric NAs to character NAs, for bind_rows
        # https://github.com/tidyverse/dplyr/issues/2584
        # use ifelse, not if_else because we know we are going to deal with different data types
        # use & not &&, because we are vectorizing
        result = dplyr::mutate_all(x,funs(ifelse(is.na(.)&is.numeric(.),NA_character_,.)))
    } else if (is.data.frame(x) && !is.null(col)) {
        col=(ez.selcol(x,col))
        cols=col
        for (col in cols) {
            x[[col]] = as.character(x[[col]])
            result=x
        }
    } else {
        result = as.character(x)
    }
    return(result)
}

#' rev a str: 'abc'->'cba'
#' @description rev a str: 'abc'->'cba'
#' @export
ez.strrev <- function(x) {
  nc <- nchar(x)
  paste(substring(x, nc:1, nc:1), collapse = "")
}

#' alias of \code{\link{ceiling}}
#' @description alias of \code{\link{ceiling}}
#' @export
ez.ceil = ceiling

#' alias of \code{\link{floor}}
#' @description alias of \code{\link{floor}}
#' @export
ez.floor = floor

#' alias of \code{\link{trunc}}
#' @description alias of \code{\link{trunc}}
#' @export
ez.fix = trunc

#' alias of \code{\link{round}}
#' @description alias of \code{\link{round}}
#' @export
ez.round = round

#' quotient
#' @description quotient
#' @param m dividend
#' @param n divider
#' @examples
#' 11 is given as the dividend and 5 the divider, (ie, 11 divided by 5, or divide 11 by 5), then 2(quotient) and 1(remainder).
#' @export
#' @seealso \code{\link{ez.remainder}}
ez.quotient = function (m,n) {
    return(m%/%n)
}

#' remainder
#' @description remainder
#' @param m dividend
#' @param n divider
#' @examples
#' 11 is given as the dividend and 5 the divider, (ie, 11 divided by 5, or divide 11 by 5), then 2(quotient) and 1(remainder).
#' @export
#' @seealso \code{\link{ez.quotient}}
ez.remainder = function (m,n){
    return(m%%n)
}

#' similar to python range (python: left inclusive, right exclusive), wrapper of \code{\link{seq}}, may also consider 1:3
#' @description similar to python range (python: left inclusive, right exclusive), wrapper of \code{\link{seq}}, may also consider 1:3
#' @return returns a vector (both ends inclusive)
#' @examples
#' ez.range(1,3) # 1 2 3, equivalent to 1:3
#' @export
ez.range = function(start, stop, step=1){seq(start, stop, by=step)}

#' linspace
#' @description linspace
#' @param n number of points
#' @export
ez.linspace = function(start, stop, n){
    seq(start, stop, length=n)
}

#' replicate a matrix, n * n (if m not provided) or n * m
#' @description replicate a matrix, n * n (if m not provided) or n * m
#' @export
ez.repmat = function(a, n, m = n) {
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

#' find with regular expression (case sensitive)
#' @description find with regular expression (case sensitive)
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
#' @export
ez.regexp = function (s, pat, ignorecase = FALSE, once = FALSE, split = FALSE) {
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
#' @description find with regular expression (case insensitive)
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
#' @export
ez.regexpi = function (s, pat, ignorecase = TRUE, once = FALSE, split = FALSE) {
    # A list with components start and end as numeric vectors indicating the start and end positions of the matches.
    # match contains each exact match, and split contains the character vector of splitted strings.
    # If no match is found all components will be NULL, except split that will contain the whole string if split = TRUE.
    # $start, $end, $match, $split
    ez.regexp(s, pat, ignorecase, once, split)
}

#' Merge multiple spaces to single space in the middle, and remove trailing/leading spaces
#' @description underlying function is \code{\link{gsub}} with regular expression
#' @param s a string 
#' @param how a num 1=left only; 2=right only; 3=left and right; 4 (default)=left and right and merge middle
#' @examples  "Hi        buddy        what's up    Bro"  --> "Hi buddy what's up bro"
#' For portability, whitespace is taken as the character class [ \t\r\n] (space, horizontal tab, line feed, carriage return).
#' @seealso \code{\link{trimws}}
#' @export
ez.trim = function (s, how=4){
    if (how==1) {
        s = trimws(s,"left")
    } else if (how==2) {
        s = trimws(s,"right")
    } else if (how==3) {
        s = trimws(s,"both")
    } else if (how==4) {
        # http://stackoverflow.com/a/25734388/2292993
        s = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", s, perl=TRUE)
    }
    return(s)
}

#' replace string or string vectors literally
#' @description support single string, vectors
#' \cr case sensitive! wrapper of sub, gsub
#' @seealso \code{\link{ez.regexprep}} \code{\link{ez.regexprepi}} \code{\link{ez.replace}} 
#' @export
ez.strreplace = function (s, expr, repstr, once = FALSE){
    if (once) {
        sub(expr, repstr, s, fixed = TRUE)
    }
    else {
        gsub(expr, repstr, s, fixed = TRUE)
    }
}

#' replace string or string vectors using regular expression (case sensitive)
#' @description replace string or string vectors using regular expression (case sensitive)
#' @seealso \code{\link{ez.strreplace}}
#' @export
ez.regexprep = function (s, expr, repstr, ignorecase = FALSE, once = FALSE){
    if (!is.character(s))
        stop("Argument 's' must be a character vector.")
    if (!is.character(expr) || !is.character(repstr))
        stop("Arguments 'old' and 'new' must be simple character strings.")
    if (once) {
        sub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
    else {
        gsub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
}

#' replace string or string vectors using regular expression (case insensitive)
#' @description replace string or string vectors using regular expression (case insensitive)
#' @seealso \code{\link{ez.strreplace}}
#' @export
ez.regexprepi = function (s, expr, repstr, ignorecase = TRUE, once = FALSE){
    if (!is.character(s))
        stop("Argument 's' must be a character vector.")
    if (!is.character(expr) || !is.character(repstr))
        stop("Arguments 'old' and 'new' must be simple character strings.")
    if (once) {
        sub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
    else {
        gsub(expr, repstr, s, ignore.case = ignorecase, perl = TRUE)
    }
}

#' reshape matrix
#' @description reshape matrix
#' @return reshape(a, n, m) returns the n-by-m matrix whose elements are taken column-wise from a.
#' \cr An error results if a does not have n*m elements. If m is missing, it will be calculated from n and the size of a.
#' @export
ez.reshape = function (a, n, m){
    if (missing(m))
        m <- length(a)%/%n
    if (length(a) != n * m)
        stop("Matrix 'a' does not have n*m elements")
    dim(a) <- c(n, m)
    return(a)
}

#' apply array function, wrapper of mapply
#' @description apply array function, wrapper of mapply
#' @seealso consider using \code{\link[dplyr]{mutate}}, eg beta = mutate(beta, Gr=substr(ez.trim(Gr),1,1))
#' @export
ez.arrayfun = function(func, ...){
    dots <- list(...)
    if (length(dots) < 1)
        stop("Empty list of arrays: Rsult cannot be computed.")
    d <- dim(dots[[1]])
    r <- mapply(func, ...)
    dim(r) <- d
    return(r)
}

#' similar to matlab find, find non-zero in a vector
#' @description similar to matlab find, find non-zero in a vector
#' @param v logical input
#' @return returns index vector
#' @examples
#' ez.find(a>1)
#' @export
ez.find = function(v){
    which( if (is.logical(v)) v else v != 0, arr.ind = TRUE )
}

#' random number, alias of \code{\link{runif}}
#' @description random number, alias of \code{\link{runif}}
#' @examples
#' runif(n, min = 0, max = 1)
#' @export
ez.rand = runif

#' mimic python join, supports vectorization, wrapper of \code{\link{paste}}
#' @description mimic python join, supports vectorization, wrapper of \code{\link{paste}}
#' @param sep default is nothing
#' @examples
#' ez.join('','a',' x ','b') # "a x b"
#' @export
ez.join = function(sep='',...){
    paste(...,sep=sep)
}

#' wrapper of \code{\link{cat}}
#' @description wrapper of \code{\link{cat}}
#' @param ... could be char, num, vector, or mixed (no need to convert type) e.g., 
#' \cr        but be careful with factors (in number)--convert with as.character(factor variable)
#' \cr        ez.print('a=',3)
#' \cr        ez.print('a=',c(3,'b'))
#' \cr        ez.print(var,'\\t',p)
#' \cr        ez.print(c(var,'\\t',p))
#' @param sep default empty
#' @return each print generates a new line automatically
#' @details do not use R \code{\link{print}}, not actually printing \\n
#' @seealso \code{\link{sprintf}}, \code{\link{ez.log}}, \code{\link{ez.join}}
#' @export
ez.print = function(...,sep=''){
    cat(..., "\n", sep = sep)
}

#' colorful print
#' @description colorful print
#' @param color string, eg, bold, italic, underline, strikethrough, black, red, green, yellow, blue, magenta, cyan, white, silver, bgWhite, bgGreen, bgCyan etc
#' @export
ez.pprint = function(string,color='green') {
    cmd = sprintf('cat(crayon::%s("%s\n"))', color, string)
    eval(parse(text = cmd))
}

#' format a vector for easy manual copy/processing. 
#' @description format a vector for easy manual copy/processing. 
#' @param vec a vector
#' @param quote TRUE/FALSE, whether add a quote around each element (switch for string or number). NULL = auto (F for numeric, T otherwise)
#' @return nothing, only print out. 
#' \cr By default in R when you type a variable name, you get [1] "rs171440fwd" "rs1800497fwd"
#' \cr now with this function you get 'rs171440fwd','rs1800497fwd','rs180043'
#' @seealso \code{\link{ez.print}} \code{\link{ez.pprint}}
#' @export
ez.format.vector = function(vec, quote=NULL,print2screen=TRUE){
    if(is.null(quote)) {quote = if (is.numeric(vec)) FALSE else TRUE}

    if (quote) {
        printout=noquote(paste0("'",noquote(paste0(vec,collapse = "','")),"'"))
    } else {
        printout=noquote(paste0(vec,collapse = ","))
    }
    if (print2screen) {
        print(printout)
        cat(sprintf("Total elements: %d\n",length(vec)))
    }
    return(invisible(printout))
}

#' wrapper of \code{\link{eval}}
#' @description wrapper of \code{\link{eval}}
#' @param cmd an R cmd in text, e.g., constructed with sprintf()
#' @param env caller's envir, could be environment(), default is parent.frame()
#'        if default parent.frame() does not work properly, try passing environment()
#'        tricky/hard to hard-code/default it in this wrapper function, R does not support call stack well.
#' @return this function evaluates the cmd in the caller's envir, so the actual return depends on the caller/cmd
#' @examples
#' ez.eval('z=9',environment())
#' @export
ez.eval = function(cmd,env = parent.frame()){
    eval(parse(text = cmd),envir = env)
}

#' wrapper of \code{\link{eval}}
#' @description wrapper of \code{\link{eval}}
#' @param cmd an R cmd in text, e.g., constructed with sprintf()
#' @param env caller's envir, could be environment(), default is parent.frame()
#'        if default parent.frame() does not work properly, try passing environment()
#'        tricky/hard to hard-code/default it in this wrapper function, R does not support call stack well.
#' @return this function evaluates the cmd in the caller's envir, so the actual return depends on the caller/cmd
#' @examples
#' ez.eval('z=9',environment())
#' @export
ez.evaluate = ez.eval

#' Function to calculate age from date of birth.
#' @description his function calculates age in days, months, or years from a 
#' date of birth to another arbitrary date. This returns a numeric vector in 
#' the specified units.
#' @param dob a vector of class \code{Date} representing the date of birth/start date
#' @param enddate a vector of class Date representing the when the observation's 
#' age is of interest, defaults to current date.
#' @param floorize TRUE/FALSE, age in integer, using \code{\link{floor}}
#' @param units character, which units of age should be calculated? allowed values are 
#' days, months, and years
#' @param precise logical indicating whether or not to calculate with leap year 
#' and leap second precision
#' @return A numeric vector of ages the same length as the dob vector
#' @source This function was developed in part from this response on the R-Help mailing list.
#' @seealso See also \code{\link{difftime}} which this function uses and mimics 
#' some functionality but at higher unit levels.  \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.convertible}} \code{\link{ez.age}} 
#' @author Jason P. Becker from package eeptools (sligthly modified by Jerry)
#' @export
#' @examples
#' x <- as.Date(c("2011-01-01", "1996-02-29"))
#' age(x[1],x[2], units = "years")  # 3.893151 18.731507
#' floor(age(x[1],x[2], units = "years")) # 3 18
#'
#' a <- as.Date(seq(as.POSIXct('1987-05-29 018:07:00'), len=26, by="21 day"))
#' b <- as.Date(seq(as.POSIXct('2002-05-29 018:07:00'), len=26, by="21 day"))
#' age <- age(a, units='years')
#' age
#' age <- age(a, units='months')
#' age
#' age <- age(a, as.Date('2005-09-01'))
#' age
ez.age <- function(dob, enddate=Sys.Date(), floorize=FALSE, units='years', precise=TRUE){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date")){
    stop("Both dob and enddate must be Date class objects")
  }
  if(any(enddate < dob)){
    stop("End date must be a date after date of birth")
  }
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  if(precise){
    start_is_leap <- ifelse(start$year %% 400 == 0, TRUE, 
                            ifelse(start$year %% 100 == 0, FALSE,
                                   ifelse(start$year %% 4 == 0, TRUE, FALSE)))
    end_is_leap <- ifelse(end$year %% 400 == 0, TRUE, 
                          ifelse(end$year %% 100 == 0, FALSE,
                                 ifelse(end$year %% 4 == 0, TRUE, FALSE)))
  }
  if(units=='days'){
    result <- difftime(end, start, units='days')
  }else if(units=='months'){
    months <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end), 
                            by='months', SIMPLIFY=FALSE), 
                     length) - 1
    # length(seq(start, end, by='month')) - 1
    if(precise){
      month_length_end <- ifelse(end$mon==1 & end_is_leap, 29,
                                 ifelse(end$mon==1, 28, 
                                        ifelse(end$mon %in% c(3, 5, 8, 10), 
                                               30, 31)))
      month_length_prior <- ifelse((end$mon-1)==1 & start_is_leap, 29, 
                                   ifelse((end$mon-1)==1, 28, 
                                        ifelse((end$mon-1) %in% c(3, 5, 8, 10), 
                                                 30, 31)))
      month_frac <- ifelse(end$mday > start$mday,
                           (end$mday-start$mday)/month_length_end,
                           ifelse(end$mday < start$mday, 
                                  (month_length_prior - start$mday) / 
                                    month_length_prior + 
                                    end$mday/month_length_end, 0.0))
      result <- months + month_frac
    }else{
      result <- months
    }
  }else if(units=='years'){
    years <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end), 
                           by='years', SIMPLIFY=FALSE), 
                    length) - 1
    if(precise){
      start_length <- ifelse(start_is_leap, 366, 365)
      end_length <- ifelse(end_is_leap, 366, 365)
      start_day <- ifelse(start_is_leap & start$yday >= 60,
                          start$yday - 1,
                          start$yday)
      end_day <- ifelse(end_is_leap & end$yday >=60,
                        end$yday - 1,
                        end$yday)
      year_frac <- ifelse(start_day < end_day,
                          (end_day - start_day)/end_length,
                          ifelse(start_day > end_day, 
                                 (start_length-start_day) / start_length +
                                   end_day / end_length, 0.0))
      result <- years + year_frac
    }else{
      result <- years
    }
  }else{
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  if (floorize) result = as.integer(floor(result))
  return(result)
}
