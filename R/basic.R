###**************************************************.
###*functions for simple data type.
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
ez.typeof = class

#' sort of a wrapper of \code{\link{type.convert}}
#' @description Convert a character vector to logical, integer, numeric, complex or factor as appropriate.
#' @param x a character vector, or a factor
#' @return returns a converted vector
#' \cr with \code{\link{ez.2value}} if x is a factor with chars, will be converted to 1 2 3 etc, see its example
#' \cr \code{\link{ez.num}} keeps the same char as is
#' @details see \url{http://stackoverflow.com/a/22701462/2292993}
#' @seealso \code{\link{ez.2value}}
#' @export
ez.num = function(x, ...){
    if (is.factor(x)) {
        # http://stackoverflow.com/a/22701462/2292993
        result = as.numeric(levels(x))[x]
    } else if (is.list(x)){
        result = utils::type.convert(as.character(unlist(x)), ...)
    } else {
        result = utils::type.convert(x, ...)
    }
    return(result)
}

#' alias of \code{\link{as.character}}
#' @export
ez.str = as.character

#' alias of \code{\link{ceiling}}
#' @param
#' @return
#' @examples
#' @export
ez.ceil = ceiling

#' alias of \code{\link{floor}}
#' @param
#' @return
#' @examples
#' @export
ez.floor = floor

#' alias of \code{\link{trunc}}
#' @param
#' @return
#' @examples
#' @export
ez.fix = trunc

#' alias of \code{\link{round}}
#' @param
#' @return
#' @examples
#' @export
ez.round = round

#' quotient
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
#' @param
#' @return returns a vector (both ends inclusive)
#' @examples
#' ez.range(1,3) # 1 2 3, equivalent to 1:3
#' @export
ez.range = function(start, stop, step=1){seq(start, stop, by=step)}

#' linspace
#' @param n number of points
#' @return
#' @examples
#' @export
ez.linspace = function(start, stop, n){
    seq(start, stop, length=n)
}

#' replicate a matrix, n * n (if m not provided) or n * m
#' @param
#' @return
#' @examples
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
ez.regexpi = function (s, pat, ignorecase = TRUE, once = FALSE, split = FALSE) {
    # A list with components start and end as numeric vectors indicating the start and end positions of the matches.
    # match contains each exact match, and split contains the character vector of splitted strings.
    # If no match is found all components will be NULL, except split that will contain the whole string if split = TRUE.
    # $start, $end, $match, $split
    ez.regexp(s, pat, ignorecase, once, split)
}

#' Merge Multiple spaces to single space, and remove trailing/leading spaces
#' @description underlying function is \code{\link{gsub}} with regular expression
#' @param s a string 
#' @param how a num 1=left only; 2=right only; 3=left and right; 4 (default)=left and right and merge middle
#' @return
#' @examples  "Hi        buddy        what's up    Bro"  --> "Hi buddy what's up bro"
#' \cr For portability, whitespace is taken as the character class [ \t\r\n] (space, horizontal tab, line feed, carriage return).
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
#' @param
#' @return
#' @examples
#' @seealso \code{\link{ez.regexprep}} \code{\link{ez.regexprepi}}
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
#' @param
#' @return
#' @examples
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
#' @param
#' @return
#' @examples
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
#' @param
#' @return reshape(a, n, m) returns the n-by-m matrix whose elements are taken column-wise from a.
#' \cr An error results if a does not have n*m elements. If m is missing, it will be calculated from n and the size of a.
#' @examples
#' @export
ez.reshape = function (a, n, m){
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
#' @param v logical input
#' @return returns index vector
#' @examples
#' ez.find(a>1)
#' @export
ez.find = function(v){
    which( if (is.logical(v)) v else v != 0 )
}

#' random number, alias of \code{\link{runif}}
#' @param
#' @return
#' @examples
#' runif(n, min = 0, max = 1)
#' @export
ez.rand = runif

#' mimic python join, supports vectorization, wrapper of \code{\link{paste}}
#' @param sep default is nothing
#' @examples
#' ez.join('','a',' x ','b') # "a x b"
#' @export
ez.join = function(sep='',...){
    paste(...,sep=sep)
}

#' wrapper of \code{\link{cat}}
#' @param ... could be char, num, vector, or mixed (no need to convert type) e.g., 
#' \cr        but be careful with factors (in number)--convert with as.character(factor variable)
#' \cr        ez.print('a=',3)
#' \cr        ez.print('a=',c(3,'b'))
#' \cr        ez.print(var,'\\t',p)
#' \cr        ez.print(c(var,'\\t',p))
#' @param sep default empty
#' @return each print generates a new line automatically
#' @examples
#' @details do not use R \code{\link{print}}, not actually printing \\n
#' @seealso \code{\link{sprintf}}, \code{\link{ez.log}}, \code{\link{ez.join}}
#' @export
ez.print = function(...,sep=''){
    cat(..., "\n", sep = sep)
}

#' wrapper of \code{\link{eval}}
#' @param cmd an R cmd in text, e.g., constructed with sprintf()
#' @return this function simply evaluates the cmd in the caller's envir, so the actual return depends on the caller/cmd
#' @examples
#' @export
ez.eval = function(cmd){
    # envir: 2 back in the stack to arrive at the "real" caller
    eval(parse(text = cmd),envir = parent.frame(2))
}