###**************************************************.
###*functions for simple data type.
###**************************************************.
#' not in
#' @description not in
#' @export
#' @name not_in
# https://stackoverflow.com/questions/5831794/opposite-of-in
`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

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

#' alias of \code{\link[glue]{glue}}
#' @description alias of \code{\link[glue]{glue}}
#' @export
ez.sprintf = glue::glue

#' wrap each element of a vector around ""
#' @description wrap each element of a vector around ""
#' @return returns a vector
#' @export
ez.quoted = function(v){
    base::strsplit(paste0('"', v, '"',collapse = '/|||/'), split = '/|||/',fixed = TRUE)[[1]]
}

#' Empty Value
#'
#' Rails-inspired helper that checks if vector values are "empty", i.e. if it's: \code{NULL}, zero-length, \code{NA}, \code{NaN}, \code{FALSE}, an empty string. Note that unlike its native R \code{is.<something>} sibling functions, \code{is.empty} is vectorised (hence the "values").
#' @param x an object to check its emptiness
#' @param trim trim whitespace? (\code{TRUE} by default)
#' @param ... additional arguments for \code{\link{sapply}}
#' @return a logical vector (vectorized).
#' @examples \dontrun{
#' is.empty(NULL)     # [1] TRUE
#' is.empty(c())      # [1] TRUE
#' is.empty(NA)       # [1] TRUE
#' is.empty(NaN)      # [1] TRUE
#' is.empty("")       # [1] TRUE
#' is.empty(0)        # [1] TRUE --> (\code{0} jerry changed to FALSE)
#' is.empty(0.00)     # [1] TRUE --> (\code{0} jerry changed to FALSE)
#' is.empty("    ")   # [1] TRUE
#' is.empty("foobar") # [1] FALSE
#' is.empty("    ", trim = FALSE)    # [1] FALSE
#' # is.empty is vectorised!
#' all(is.empty(rep("", 10)))        # [1] TRUE
#' all(is.empty(matrix(NA, 10, 10))) # [1] TRUE
#' }
#' @export
#' @note copied from https://cran.r-project.org/web/packages/rapportools/index.html
ez.is.empty <- function(x, trim = TRUE, ...) {
    vgsub <- function(pattern, replacement, x, ...){
        for(i in 1:length(pattern))
            x <- gsub(pattern[i], replacement[i], x, ...)
        x
    }

    trim.space <- function(x, what = c('both', 'leading', 'trailing', 'none'), space.regex = '[:space:]', ...){
        if (missing(x))
            stop('nothing to trim spaces to =(')
        re <- switch(match.arg(what),
                     both     = sprintf('^[%s]+|[%s]+$', space.regex, space.regex),
                     leading  = sprintf('^[%s]+', space.regex),
                     trailing = sprintf('[%s]+$', space.regex),
                     none     = {
                         return (x)
                     })
        vgsub(re, '', x, ...)
    }

    if (length(x) <= 1) {
        if (is.null(x))
            return (TRUE)
        if (length(x) == 0)
            return (TRUE)
        if (is.na(x) || is.nan(x))
            return (TRUE)
        if (is.character(x) && nchar(ifelse(trim, trim.space(x), x)) == 0)
            return (TRUE)
        if (is.logical(x) && !isTRUE(x))
            return (TRUE)
        # jerry: not use this
        # if (is.numeric(x) && x == 0)
        #     return (TRUE)
        return (FALSE)
    } else
        sapply(x, ez.is.empty, trim = trim, ...)
}

#' Check numeric
#'
#' @description check if a str obj is actually numeric
#' @param x a str vector, or a factor of str vector, or numeric vector. x will be coerced and trimws.
#' @param na.strings case sensitive strings that will be treated to NA.
#' @param naAsTrue whether NA (including actual NA and na.strings) will be treated as numeric like
#' @return a logical vector (vectorized).
#' @export
#' @note Using regular expression
#' \cr TRUE for any actual numeric c(3,4,5,9.9) or c("-3","+4.4",   "-42","4L","9L",   "1.36e4","1.36E4",    NA, "NA", "","NaN", NaN): 
#' \cr positive or negative numbers with no more than one decimal c("-3","+4.4") OR
#' \cr positive or negative integers (e.g., c("-42","4L","39L")) OR
#' \cr positive or negative numbers in scientific notation c("1.36e4","1.36E4")
#' \cr NA, or na.strings
ez.is.numeric.like <- function(x,naAsTrue=TRUE,na.strings=c('','.','NA','na','N/A','n/a','NaN','nan')){
    x = trimws(x,'both')
    x[x %in% na.strings] = NA
    # https://stackoverflow.com/a/21154566/2292993
    result = grepl("^[\\-\\+]?[0-9]+[\\.]?[0-9]*$|^[\\-\\+]?[0-9]+[L]?$|^[\\-\\+]?[0-9]+[\\.]?[0-9]*[eE][0-9]+$",x,perl=TRUE)
    if (naAsTrue) result = result | is.na(x)
    return((result))
}

#' alias of \code{\link{class}}
#' @description alias of \code{\link{class}}
#' @export
ez.typeof = class

#' alias of \code{\link{Sys.sleep}}, in seconds
#' @description alias of \code{\link{Sys.sleep}}, in seconds
#' @seealso \code{\link{ez.pause}}
#' @export
ez.sleep = Sys.sleep

#' profile
#' @description profile
#' @export
ez.profile = function(){system("open -a 'Sublime Text' $HOME/.bash_profile")}

#' rprofile
#' @description rprofile
#' @export
ez.rprofile = function(){system("open -a 'Sublime Text' $HOME/Dropbox/Apps/RStudio/.Rprofile")}

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
#' @param force T/F. a string vec/col that only has string of num, eg c('1','2')->c(1,2) will always become num.
#' \cr If T, convert everything (factor, etc) to character first then to numeric (no warning for NA coerce). Otherwise, factor(1:2)-->factor(1:2)
#' \cr See example for more
#' @details Both value and variable label attributes will be removed when converting variables to characters.
#' @return returns a converted vector, data frame
#' \cr with \code{\link{ez.2value}} if x is a factor with chars, will be converted to 1 2 3 etc, see its example
#' \cr \code{\link{ez.num}} keeps the same char as is
#' @details see \url{http://stackoverflow.com/a/22701462/2292993}
#' @seealso \code{\link{ez.2value}}
#' @family data transformation functions
#' @export
#' @examples
#' ez.num(c(1,2,3,NA),force=T)        # -> 1 2 3 NA numeric vector
#' ez.num(c(1,2,3,NA),force=F)        # -> 1 2 3 NA int vector
#' ez.num(c(1,'2','a',3,NA))          # -> same chara vector
#' ez.num(c(1,'2',3,NA))              # -> 1 2 3 NA int vector
#' ez.num(c(1,'',3))                  # -> 1 NA 3 int vector
#' ez.num(c(1,'',3),froce=T)          # -> 1 NA 3 numeric vector
#' ez.num(factor(c(1,'2','a',3,NA)),force=F)  # factor['1','2','a','3', 'NA']
#' ez.num(factor(c(1,'2','a',3,NA)),force=T)  # -> 1 2 NA 3 NA int (warning is suppressed)
#' ez.num(factor(c(1,'2',3,NA)),force=F)  # factor['1','2','3', NA]
#' ez.num(factor(c(1,'2',3,NA)),force=T)  # 1 2 3 NA numeric
#' ez.num(factor(c(1,2,3,NA)),force=F)  # factor['1','2','3', NA]
#' ez.num(factor(c(1,2,3,NA)),force=T)  # 1 2 3 NA numeric
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
ez.num = function(x, col=NULL, force=FALSE, print2screen=TRUE, ...){
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
        if (print2screen) {
            newNAs = length(which( (!is.na(x)) & is.na(result) ))
            if (newNAs>0) ez.pprint(sprintf('Attention: %d NAs introduced when converting to num', newNAs))
        }
    } else if (is.data.frame(x) && is.null(col)) {
        # check before is.list, because a data frame is a list, but not the other way
        # https://stackoverflow.com/a/33050704/2292993
        if (!force) {
            x[] = rapply(x, utils::type.convert, classes = "character", how = "replace", as.is = TRUE)
        } else {
            oldNAs = ez.count(x)
            # x = dplyr::mutate_all(x, funs(suppressWarnings(as.numeric(as.character(.)))))
            x[] = lapply( x, function(e){suppressWarnings(as.numeric(as.character(e)))} )
            if (print2screen) {
                newNAs = ez.count(x) - oldNAs
                if (newNAs>0) ez.pprint(sprintf('Attention: %d NAs introduced when converting to num', newNAs))
            }
        }
        result = x
    } else if (is.data.frame(x) && !is.null(col)) {
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     # recursive to is.data.frame(x) && is.null(col)
        #     x[col] = ez.num(x[col],force=force)
        #     result=x
        # }
        oldNAs = ez.count(x[cols])
        x[cols] = lapply(x[cols],function(e,force){ez.num(e,force=force,print2screen=F)},force=force)
        if (print2screen) {
            newNAs = ez.count(x[cols]) - oldNAs
            if (newNAs>0) ez.pprint(sprintf('Attention: %d NAs introduced when converting to num', newNAs))
        }
        result = x
    } else if (is.list(x)){
        result = utils::type.convert(as.character(unlist(x)), as.is = TRUE, ...)
    } else {
        # cannot pass factor to type.convert()
        # utils::type.convert(c(1,2,'3'), as.is = F) -> int
        # utils::type.convert(c(1,2,'3'), as.is = T) -> int
        # utils::type.convert(c(1,2,'a'), as.is = F) -> fac  ['1' '2' 'a']
        # utils::type.convert(c(1,2,'a'), as.is = T) -> vec  ('1' '2' 'a')
        if (!force) result = utils::type.convert(as.character(x), as.is = TRUE, ...) else result=suppressWarnings(as.numeric(as.character(x)))
        if (print2screen) {
            newNAs = length(which( (!is.na(x)) & is.na(result) ))
            if (newNAs>0) ez.pprint(sprintf('Attention: %d NAs introduced when converting to num', newNAs))
        }
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
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.like}} \code{\link{ez.age}} \code{\link{ez.time}}
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
#' @description convert from a time (numeric, character/factor, hms/difftime) to class (chron) times (17:48:00--h:m:s military only) or to class numeric (0.7416667--fractions of a day), call \code{\link[chron]{times}}:
#' \cr \cr SPSS military time (civilian time not supported) specified in date format, 17:48 read into R as 64080 (seconds of a day, foreign package), or as hms/difftime (same as SPSS, haven package), or
#' \cr \cr Excel military/civilian time specified in time format, 5:48:00 PM, 17:48:00 read into R as 0.7416667 (fractions of a day), or
#' \cr \cr Pass directly a string "17:48:00" (format specified by param format)
#' @param x a vector of number or character, hms/difftime
#' @param ori one of 'Excel', 'SPSS' (ignored if x is character)
#' @param format input format, see examples (more formats at \code{\link{strptime}}). ignored if x is numeric/hms
#' @param out.type string, 'numeric' (fractions of a day) or 'times'/'time'
#' @return returns a vector of number (class numeric) or time (class times). class times can be passed to as.character(.), or substr(.,1,5), or as.numeric(.), see more at \code{\link[chron]{chron}} chron stores as a fraction of a day, if you do as.numeric
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.like}} \code{\link{ez.age}} \code{\link{ez.time}}
#' @export
#' @examples
#' ez.time("5:20:31 am",format="%I:%M:%S %p",out.type="time")  # 5, 05; am AM aM Am OK. %I = 12 hr
#' ez.time("15:32", format="%H:%M",out.type="time")  # 5 05 OK.
#' ez.time("5:2", format="%H:%M",out.type="time")
ez.time = function(x,ori='SPSS',format="%H:%M",out.type='numeric',...) {
    if (is.numeric(x)) {
        if (ori=='SPSS') {
            if (out.type %in% c('time','times')) {
                # https://stackoverflow.com/a/39208186/2292993
                result = chron::times(x/(24*60*60),...)
            }
            if (out.type=='numeric') {
                result = x/(24*60*60)  # SPSS stores as seconds of a day
            }
        }
        if (ori=='Excel') {
            # https://stackoverflow.com/a/28044345/2292993
            if (out.type %in% c('time','times')) result = chron::times(x,...)
            if (out.type=='numeric') result = x  # Excel stores as fractions of a day
        }
    }
    if (is.character(x) | is.factor(x)) {
        # https://stackoverflow.com/a/36347366/2292993
        x=format(strptime(x, format = format), "%H:%M:%S")
        result=chron::chron(times.=x,format=c(dates = "m/d/y", times = "h:m:s"),...)
        if (out.type=='numeric') result = as.numeric(result)  # here chron stores as fraction of a day, as.numeric gets you that
    }
    if ( all(class(x) %in% c('hms','difftime')) ) {
        if (out.type %in% c('time','times')) result = chron::times(x,...)  # simply change class type
        if (out.type=='numeric') result = as.numeric(x)/(24*60*60)  # here as.numeric gets you seconds of a day
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
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.like}} \code{\link{ez.age}} \code{\link{ez.time}}
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
#' ez.is.date.like(mydate, format = "%m/%d/%Y")  # T F F
#' @export
#' @seealso \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.like}} \code{\link{ez.age}} \code{\link{ez.time}}
ez.is.date.like = function(x,format="%m/%d/%Y",...) {
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
#' @note ez.2char only coverts factor, ez.str coverts all types, both use as.character
#' @seealso \code{\link{ez.2char}}
ez.str = function(x, col=NULL){
    if (is.data.frame(x) && is.null(col)){
        # result = dplyr::mutate_all(x, as.character)
        x[] = lapply(x,function(e){as.character(e)})
        result = x
    } else if (is.data.frame(x) && is.na(col)) {
        # convert all numeric NAs to character NAs, for bind_rows
        # https://github.com/tidyverse/dplyr/issues/2584
        # use ifelse, not if_else because we know we are going to deal with different data types
        # use & not &&, because we are vectorizing
        # result = dplyr::mutate_all(x,funs(ifelse(is.na(.)&is.numeric(.),NA_character_,.)))
        x[] = lapply( x, function(e){ifelse(is.na(e) & is.numeric(e), NA_character_, e)} )
        result = x
    } else if (is.data.frame(x) && !is.null(col)) {
        col=(ez.selcol(x,col))
        cols=col
        # for (col in cols) {
        #     x[[col]] = as.character(x[[col]])
        #     result=x
        # }
        x[cols] = lapply(x[cols],function(e){as.character(e)})
        result = x
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

#' compare if s1 exact the same as s2, but ignore case, space, punctuation
#' @description compare if s1 exact the same as s2, but ignore case, space, punctuation
#' @param s string, supports vectorization
#' @param i ignore case, space, punctuation
#' @note internally strip the space, punct, and convert case, then use "==" to compare
#' @return logic
#' @export
ez.strcomp <- function(s1,s2,icase=TRUE,ispace=TRUE,ipunctuation=TRUE) {
    if (ispace) {
        s1 = gsub('[[:space:]]','',s1,perl=TRUE)
        s2 = gsub('[[:space:]]','',s2,perl=TRUE)
    }
    if (ipunctuation){
        s1 = gsub('[[:punct:]]','',s1,perl=TRUE)
        s2 = gsub('[[:punct:]]','',s2,perl=TRUE)
    }
    if (icase){
        s1 = tolower(s1)
        s2 = tolower(s2)
    }
    return(s1==s2)
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
#' @param s a string or a data frame. if data frame, how will be ignored/forced to be 'both'
#' @param how a num 1=left only; 2=right only; 3=left and right; 4 (default)=left and right and merge middle
#' @examples  "Hi        buddy        what's up    Bro"  --> "Hi buddy what's up bro"
#' For portability, whitespace is taken as the character class [ \t\r\n] (space, horizontal tab, line feed, carriage return).
#' @seealso \code{\link{trimws}}
#' @export
ez.trim = function (s, how=4){
    if (is.data.frame(s)) {
        s[]=lapply(s, function(x) if (is.factor(x)) factor(trimws(x,'both')) else x)
        s[]=lapply(s, function(x) if(is.character(x)) trimws(x,'both') else(x))
        return(s)
    }

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
#' @seealso \code{\link{ez.recode}}, \code{\link{ez.recode2}}, \code{\link{ez.replace}}, \code{\link{ez.replacewhen}}, \code{\link{ez.2label}}, \code{\link{ez.factorname}}, \code{\link{ez.strreplace}}, \code{\link{ez.strrev}}, \code{\link{ez.regexprep}}, \code{\link{ez.regexprepi}}
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

age_calc <- function(dob, enddate=Sys.Date(), units='years', precise=TRUE) {
    if (!inherits(dob, "Date") | !inherits(enddate, "Date")) {
        stop("Both dob and enddate must be Date class objects")
    }
    if (any(enddate < dob)) {
        stop("End date must be a date after date of birth")
    }
    start <- as.POSIXlt(dob)
    end <- as.POSIXlt(enddate)
    if (precise) {
        start_is_leap <- ifelse(start$year %% 400 == 0, TRUE,
                                                        ifelse(start$year %% 100 == 0, FALSE,
                                                                     ifelse(start$year %% 4 == 0, TRUE, FALSE)))
        end_is_leap <- ifelse(end$year %% 400 == 0, TRUE,
                                                    ifelse(end$year %% 100 == 0, FALSE,
                                                                 ifelse(end$year %% 4 == 0, TRUE, FALSE)))
    }
    if (units == 'days') {
        result <- difftime(end, start, units = 'days')
    } else if (units == 'months') {
        months <- vapply(mapply(seq, as.POSIXct(start), as.POSIXct(end),
                                                        by = 'months', SIMPLIFY = FALSE),
                                         length, FUN.VALUE = length(start)) - 1
        # length(seq(start, end, by='month')) - 1
        if (precise) {
            month_length_end <- ifelse(end$mon == 1 & end_is_leap, 29,
                                                                 ifelse(end$mon == 1, 28,
                                                                                ifelse(end$mon %in% c(3, 5, 8, 10),
                                                                                             30, 31)))
            month_length_prior <- ifelse((end$mon - 1) == 1 & start_is_leap, 29,
                                                                     ifelse((end$mon - 1) == 1, 28,
                                                                                ifelse((end$mon - 1) %in% c(3, 5, 8, 10),
                                                                                                 30, 31)))
            month_frac <- ifelse(end$mday > start$mday,
                                                     (end$mday - start$mday) / month_length_end,
                                                     ifelse(end$mday < start$mday,
                                                                    (month_length_prior - start$mday) /
                                                                        month_length_prior +
                                                                        end$mday/month_length_end, 0.0))
            result <- months + month_frac
        }else{
            result <- months
        }
    } else if (units == 'years') {
        years <- vapply(mapply(seq, as.POSIXct(start), as.POSIXct(end),
                                                     by = 'years', SIMPLIFY = FALSE),
                                        length, FUN.VALUE = length(start)) - 1
        if (precise) {
            start_length <- ifelse(start_is_leap, 366, 365)
            end_length <- ifelse(end_is_leap, 366, 365)
            start_day <- ifelse(start_is_leap & start$yday >= 60,
                                                    start$yday - 1,
                                                    start$yday)
            end_day <- ifelse(end_is_leap & end$yday >= 60,
                                                end$yday - 1,
                                                end$yday)
            year_frac <- ifelse(start_day < end_day,
                                                    (end_day - start_day)/end_length,
                                                    ifelse(start_day > end_day,
                                                                 (start_length - start_day) / start_length +
                                                                     end_day / end_length, 0.0))
            result <- years + year_frac
        }else{
            result <- years
        }
    }else{
        stop("Unrecognized units. Please choose years, months, or days.")
    }
    return(result)
}

#' Function to calculate age from date of birth.
#' @description This function calculates age in days, months, or years from a
#' date of birth to another arbitrary date. This returns a numeric vector in
#' the specified units.
#' @param dob a vector of class \code{Date} representing the date of birth/start date
#' @param enddate a vector of class Date representing the when the observation's
#' age is of interest, defaults to current date.
#' @param units character, which units of age should be calculated? allowed values are
#' days, months, and years
#' @param precise logical indicating whether or not to calculate with leap year
#' and leap second precision. If F, returns an integer.
#' @return A numeric vector of ages the same length as the dob vector (if dob or enddate is NA, return NA as well--I hacked this)
#' @source This function was developed in part from this response on the R-Help mailing list.
#' @seealso See also \code{\link{difftime}} which this function uses and mimics
#' some functionality but at higher unit levels.  \code{\link{ez.date}} \code{\link{ez.is.date}} \code{\link{ez.is.date.like}} \code{\link{ez.age}}
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
ez.age <- function(dob, enddate=Sys.Date(), units='years', precise=TRUE){
    # age_calc cannot deal with NA
    # well sapply here is essentially a loop
    result = sapply(1:length(dob),function(i,dob,enddate,units,precise) {
            if (is.na(dob[i]) || is.na(enddate[i])) {
                return(NA)
            } else {
                return(age_calc(dob[i],enddate[i],units,precise))
            }
        }, dob=dob,enddate=enddate,units=units,precise=precise)
    return(result)
}

#' Replaces non-finite values (Inf, -Inf, NaN, NA) with \code{NA}s in array-like objects.
#' @description Replaces non-finite values (Inf, -Inf, NaN, NA) with \code{NA}s in array-like objects.
#' @param x An array-like object, or vector
#' @return Returns \code{x} with non-finite values (Inf, -Inf, NaN, NA) replaced with \code{NA}s.
#' @note is.na() returns TRUE for both NA and NaN, however is.nan() return TRUE for NaN (0/0) and FALSE for NA.
#' \cr mean(c(NA,NA),na.rm=T)  ---> NaN, mean(c(NA,NA),na.rm=F) ---> NA  sum(c(NA,NA),na.rm=T)  ---> 0, sum(c(NA,NA),na.rm=F) ---> NA
#' @examples
#' ez.nan2na(Inf)
#' ez.nan2na(rep(c(0, -Inf, Inf, NA, NaN), 3))
#' ez.nan2na(matrix(c(0, Inf, -Inf, 0), 2, 2))
#' ez.nan2na(array(c(0, -Inf, Inf, 1, NaN), dim = c(2, 3, 4)))
#' ez.nan2na(ts(rep(c(0, -Inf, Inf), 2), frequency = 5))
#' @seealso \code{\link{is.finite}}, \code{\link{NA}}
#' @export
ez.nan2na = function(x) {
    # https://cran.r-project.org/web/packages/cmrutils/index.html
    if(!is.null(x))
    {
        result <- x
        result[!is.finite(result)] <- NA
    }
    else {
        result <- NULL
    }
    return(result)
}

#' Replaces blank-ish elements of a factor or character vector to NA
#' @description Replaces blank-ish elements of a factor or character vector to NA
#' @param x a vector of factor or character or any type
#' @param na.strings case sensitive strings that will be coverted to NA. The function will do a trimws(x,'both') before conversion. If NULL, do only trimws, no conversion to NA.
#' @return Returns a vector trimws (always for factor, character) and NA converted (if matching na.strings). Attributes will also be kept ('label','labels', 'value.labels').
#' @seealso \code{\link{ez.nan2na}}, \code{\link{ez.is.numeric.like}}, \code{\link{ez.num}}
#' @export
ez.blank2na = function(x,na.strings=c('','.','NA','na','N/A','n/a','NaN','nan')) {
    if (is.factor(x)) {
        lab = attr(x, 'label', exact = T)
        labs1 <- attr(x, 'labels', exact = T)
        labs2 <- attr(x, 'value.labels', exact = T)

        # trimws will convert factor to character
        x = trimws(x,'both')
        # trimws(NULL) --> ''
        if (! is.null(lab)) lab = trimws(lab,'both')
        if (! is.null(labs1)) labs1 = trimws(labs1,'both')
        if (! is.null(labs2)) labs2 = trimws(labs2,'both')

        if (!is.null(na.strings)) {
            # convert to NA
            x[x %in% na.strings] = NA
            # also remember to remove na.strings from value labels
            labs1 = labs1[! labs1 %in% na.strings]
            labs2 = labs2[! labs2 %in% na.strings]
        }

        # the levels will be reset here
        x = factor(x)

        # NULL removes the attr, but '' results in empty attr
        attr(x, 'label') <- lab
        attr(x, 'labels') <- labs1
        attr(x, 'value.labels') <- labs2
    } else if (is.character(x)) {
        lab = attr(x, 'label', exact = T)
        labs1 <- attr(x, 'labels', exact = T)
        labs2 <- attr(x, 'value.labels', exact = T)

        # trimws will convert factor to character
        x = trimws(x,'both')
        if (! is.null(lab)) lab = trimws(lab,'both')
        if (! is.null(labs1)) labs1 = trimws(labs1,'both')
        if (! is.null(labs2)) labs2 = trimws(labs2,'both')

        if (!is.null(na.strings)) {
            # convert to NA
            x[x %in% na.strings] = NA
            # also remember to remove na.strings from value labels
            labs1 = labs1[! labs1 %in% na.strings]
            labs2 = labs2[! labs2 %in% na.strings]
        }

        # NULL removes the attr, but '' results in empty attr
        attr(x, 'label') <- lab
        attr(x, 'labels') <- labs1
        attr(x, 'value.labels') <- labs2
    } else {
        x = x
    }
    return(x)
}
