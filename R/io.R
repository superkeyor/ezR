# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# import/export data file
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' read csv table, wrapper of \code{\link{read.csv}}
#' @param tolower whether to convert all column names to lower case
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
z.read = function(..., tolower=FALSE){
    result = read.csv(...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{z.write}}, wrapper of \code{\link{write.csv}}
#' @param
#' @return
#' @examples
#' (x, file="RData.csv", row.names=FALSE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
z.save = function(x, file="RData.csv", row.names=FALSE, na = "", ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    x = data.frame(x)
    rownames(x) <- NULL
    write.csv(x=x, file=file, row.names=row.names, na=na, ...)
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{z.save}}, wrapper of \code{\link{write.csv}}
#' @param
#' @return
#' @examples
#' (x, file="RData.csv", row.names=FALSE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
z.write = z.save

#' read an xlsx file, wrapper of \code{\link[xlsx]{read.xlsx}}
#' @param tolower whether to convert all column names to lower case
#' @return
#' @examples
#' read.xlsx(file, sheetIndex, sheetName=NULL, rowIndex=NULL,
#'           startRow=NULL, endRow=NULL, colIndex=NULL,
#'           as.data.frame=TRUE, header=TRUE, colClasses=NA,
#'           keepFormulas=FALSE, encoding="unknown", ...)
#' colClasses: Only numeric, character, Date, POSIXct, column types are accepted
#' colClasses=c("Date", "character","integer", rep("numeric", 2),  "POSIXct")
#' @export
z.readx = function(file, sheetIndex=1, tolower=FALSE, ...){
    result = xlsx::read.xlsx(file, sheetIndex, ...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' wrapper of \code{\link[sjmisc]{read_spss}}
#' @description potentially keep variable labels and value labels.
#' @param tolower whether to convert all column names to lower case
#' @export
z.reads2 = function(..., tolower=FALSE){
    result = sjmisc::read_spss(...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' read spss .sav file with foreign package
#' @description
#' @param tolower whether to convert all column names to lower case
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
z.reads = function(file, valuelabel=TRUE, tolower=FALSE){
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

#' alias of \code{\link[sjmisc]{write_spss}}, \code{\link{z.writes}}
#' @description potentially keep variable labels and value labels
#' @export
z.saves = sjmisc::write_spss

#' alias of \code{\link[sjmisc]{write_spss}}, \code{\link{z.saves}}
#' @description potentially keep variable labels and value labels
#' @export
z.writes = sjmisc::write_spss

#' save an xlsx file, alias of \code{\link{z.writex}}, wrapper of \code{\link[xlsx]{write.xlsx}}
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
z.savex = function(x, file="RData.xlsx", sheetName="Sheet1", row.names=FALSE, showNA=FALSE, ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    # require('xlsx')
    x = data.frame(x)
    rownames(x) <- NULL
    xlsx::write.xlsx(x=x, file=file, sheetName=sheetName, ..., row.names=row.names, showNA=showNA)
    # detach("package:xlsx", unload=TRUE)
}

#' save an xlsx file, alias of \code{\link{z.savex}}, wrapper of \code{\link[xlsx]{write.xlsx}}
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
z.writex = z.savex

#' show the content of a file in read-only mode, wrapper of wrapper of \code{\link{file.show}}
#' @param
#' @return
#' @examples
#' @export
z.type = function(path){
    result = file.show(path,title='File (read-only)')
}

#' edit a file, wrapper of wrapper of \code{\link{file.edit}}
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
