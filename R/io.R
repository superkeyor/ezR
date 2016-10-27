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
ez.read = function(..., tolower=FALSE){
    result = read.csv(...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.write}}, wrapper of \code{\link{write.csv}}
#' @param
#' @return
#' @examples
#' (x, file="RData.csv", row.names=FALSE, col.names=TRUE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
ez.save = function(x, file="RData.csv", row.names=FALSE, col.names=TRUE, na = "", ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    x = data.frame(x)
    if (row.names==FALSE) {rownames(x) <- NULL}
    if (col.names==TRUE) {
        write.csv(x=x, file=file, row.names=row.names, na=na, ...)
    }else{
        # hack to not save col names http://stackoverflow.com/a/19227265/2292993
        # why not write.table for both? 
        # because write.table can be slow for data frames with large numbers (hundreds or more) of columns
        write.table(x=x, file=file, row.names=row.names, col.names=col.names, na=na, ...)
    }
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.save}}, wrapper of \code{\link{write.csv}}
#' @param
#' @return
#' @examples
#' (x, file="RData.csv", row.names=FALSE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
ez.write = ez.save

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
ez.readx = function(file, sheetIndex=1, tolower=FALSE, ...){
    result = xlsx::read.xlsx(file, sheetIndex, ...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' read an xlsx file, wrapper of \code{\link[openxlsx]{read.xlsx}}
#' @description uses openxlsx package which does not require java and is much faster, but has a slightly different interface/parameters from xlsx package.
#' @param tolower whether to convert all column names to lower case
#' @return
#' @examples
#' read.xlsx(xlsxFile, sheet = 1, startRow = 1, colNames = TRUE,
#'          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#'          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
#' @export
ez.readx2 = function(file, sheet=1, tolower=FALSE, ...){
    result = openxlsx::read.xlsx(file, sheet, ...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' read an xlsx file, returns and prints sheet names, wrapper of \code{\link[openxlsx]{getSheetNames}}
#' @param toprint print out sheet indices and names, default TRUE
#' @return a list of sheet names
#' @examples
#' readxlist(file)
#' @export
ez.readxlist = function(file, toprint=TRUE){
    sheetnames = openxlsx::getSheetNames(file)
    if (toprint){
        for (i in 1:length(sheetnames)){
            cat(i, '\t', sheetnames[i], '\n')
        }
    }
    return(sheetnames)
}

#' wrapper of \code{\link[sjmisc]{read_spss}}
#' @description potentially keep variable labels and value labels.
#' @param tolower whether to convert all column names to lower case
#' @export
ez.reads2 = function(..., tolower=FALSE){
    result = sjmisc::read_spss(...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' read spss .sav file with foreign package
#' @description see more at \code{\link[foreign]{read.spss}}
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
ez.reads = function(file, valuelabel=TRUE, tolower=FALSE){
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

#' alias of \code{\link[sjmisc]{write_spss}}, \code{\link{ez.writes}}
#' @description potentially keep variable labels and value labels
#' @export
ez.saves = sjmisc::write_spss

#' alias of \code{\link[sjmisc]{write_spss}}, \code{\link{ez.saves}}
#' @description potentially keep variable labels and value labels
#' @export
ez.writes = sjmisc::write_spss

#' save an xlsx file, alias of \code{\link{ez.writex}}, wrapper of \code{\link[xlsx]{write.xlsx}}
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
ez.savex = function(x, file="RData.xlsx", sheetName="Sheet1", row.names=FALSE, showNA=FALSE, ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    # require('xlsx')
    x = data.frame(x)
    if (row.names==FALSE) {rownames(x) <- NULL}
    xlsx::write.xlsx(x=x, file=file, sheetName=sheetName, ..., row.names=row.names, showNA=showNA)
    # detach("package:xlsx", unload=TRUE)
}

#' save an xlsx file, alias of \code{\link{ez.savex}}, wrapper of \code{\link[xlsx]{write.xlsx}}
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
ez.writex = ez.savex

#' show the content of a file in read-only mode, wrapper of wrapper of \code{\link{file.show}}
#' @param
#' @return
#' @examples
#' @export
ez.type = function(path){
    result = file.show(path,title='File (read-only)')
}

#' edit a file, wrapper of wrapper of \code{\link{file.edit}}
#' @param
#' @return
#' @examples
#' @export
ez.edit = function(path){
    result = file.edit(path)
}

#' Prints output to both terminal and a file (log.txt) globally.
#' @param mode a=append; w=overwrite
#' @return
#' @examples
#' @export
ez.log = function(file='log.txt',mode='a',status=TRUE){
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
