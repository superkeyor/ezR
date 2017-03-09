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

#' read an xlsx file, wrapper of \code{\link[xlsx]{read.xlsx}} from the xlsx package
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
ez.readx2 = function(file, sheetIndex=1, tolower=FALSE, ...){
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
ez.readx = function(file, sheet=1, tolower=FALSE, ...){
    result = openxlsx::read.xlsx(file, sheet, ...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' read an xlsx file, returns and prints sheet names, wrapper of \code{\link[openxlsx]{getSheetNames}} from the openxlsx package
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
#' @description would not convert value labels to factor levels (i.e., gender 1/2->male/female), instead keep variable labels and value labels as attributes; also internally trim string space
#' @param path File path to the data file
#' @param tofactor if TRUE, atomic to factor; if FALSE, keep as atomic (char always to factor regardless)
#' @param keepna if TRUE, user-defined missing values will be left as their original codes. If FALSE (default), corresponding values are converted to NA.
#' @param tolower whether to convert all column names to lower case
#' @export
ez.reads = function(path, tofactor=TRUE, keepna=FALSE, tolower=FALSE, ...){
    result = sjmisc::read_spss(path=path, atomic.to.fac=tofactor, keep.na=keepna, ...)
    if (tolower) names(result) = tolower(names(result))
    # the tofactor/atomic.to.fac seems only working for variable with numbers (gender 1/2) not stirng values (group control/patient)
    # here is a hack from http://stackoverflow.com/a/20638742/2292993
    result[sapply(result, is.character)] <- lapply(result[sapply(result, is.character)], as.factor)
    return(result)
}

#' read spss .sav file with foreign package
#' @description cannot trim string space (trim.factor.names, trim_values in read.spss not working??), 
#'              but can convert value labels to factor levels for easy viewing (i.e., gender 1/2->male/female) when valuelabel=TRUE (see below). 
#'              therefore, maybe for viewing purpose only, instead of processing. 
#'              see more at \code{\link[foreign]{read.spss}}
#' @param valuelabel logic
#' \cr               if True: gender=Male,Female, gender is a factor with two levels "Male/Female"
#' \cr               if False: gender=1,2, gender is a number with attributes "Male/Female"
#' \cr               (char will always be converted to factor regardless of valuelabel)
#' @param tolower whether to convert all column names to lower case
#' @return
#' @examples
#' (file, valuelabel=TRUE,tolower=FALSE)
#'
#' alternatively, one can use SPSS R plugin to pass data between SPSS and R.
#' @export
ez.reads2 = function(file, valuelabel=TRUE, tolower=FALSE){
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

#' save an xlsx file, alias of \code{\link{ez.writex2}}, wrapper of \code{\link[xlsx]{write.xlsx}} from the xlsx package
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
ez.savex2 = function(x, file="RData.xlsx", sheetName="Sheet1", row.names=FALSE, showNA=FALSE, ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    # require('xlsx')
    x = data.frame(x)
    if (row.names==FALSE) {rownames(x) <- NULL}
    xlsx::write.xlsx(x=x, file=file, sheetName=sheetName, ..., row.names=row.names, showNA=showNA)
    # detach("package:xlsx", unload=TRUE)
}

#' save an xlsx file, alias of \code{\link{ez.savex2}}, wrapper of \code{\link[xlsx]{write.xlsx}} from the xlsx package
#' @param
#' @return
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE, append=FALSE, showNA=TRUE)
#' @export
ez.writex2 = ez.savex2

#' save an xlsx file, alias of \code{\link{ez.writex}}, wrapper of \code{\link[openxlsx]{write.xlsx}} from the openxlsx package
#' @description uses openxlsx package which does not require java and is much faster, but has a slightly different interface/parameters from xlsx package.
#' @param creator A string specifying the workbook author
#' @param sheetName Name of the worksheet
#' @param gridLines A logical. If FALSE, the worksheet grid lines will be hidden.
#' @param startCol A vector specifiying the starting column(s) to write df
#' @param startRow A vector specifiying the starting row(s) to write df
#' @param xy An alternative to specifying startCol and startRow individually. A vector of the form c(startCol, startRow)
#' @param colNames or col.names If TRUE, column names of x are written.
#' @param rowNames or row.names If TRUE, row names of x are written.
#' @param headerStyle Custom style to apply to column names.
#' @param borders Either "surrounding", "columns" or "rows" or NULL. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.
#' @param borderColour Colour of cell border
#' @param borderStyle Border line style.
#' @param overwrite Overwrite existing file (Defaults to TRUE as with write.table)
#' @param asTable write using writeDataTable as opposed to writeData
#' @return nothing
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE)
#'
#' ## write to working directory
#' options("openxlsx.borderColour" = "#4F80BD") ## set default border colour
#' write.xlsx(iris, file = "writeXLSX1.xlsx", colNames = TRUE, borders = "columns")
#' write.xlsx(iris, file = "writeXLSX2.xlsx", colNames = TRUE, borders = "surrounding")
#' 
#' 
#' hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
#'                   fontName="Arial Narrow", fgFill = "#4F80BD")
#' 
#' write.xlsx(iris, file = "writeXLSX3.xlsx", colNames = TRUE, borders = "rows", headerStyle = hs)
#' 
#' ## Lists elements are written to individual worksheets, using list names as sheet names if available
#' l <- list("IRIS" = iris, "MTCATS" = mtcars, matrix(runif(1000), ncol = 5))
#' write.xlsx(l, "writeList1.xlsx")
#' 
#' ## different sheets can be given different parameters
#' write.xlsx(l, "writeList2.xlsx", startCol = c(1,2,3), startRow = 2,
#'            asTable = c(TRUE, TRUE, FALSE), withFilter = c(TRUE, FALSE, FALSE))
#' @export
ez.savex = function(x, file="RData.xlsx", sheetName="Sheet1", row.names=FALSE, col.names=TRUE, asTable=FALSE, ...){
    x = data.frame(x)
    openxlsx::write.xlsx(x=x, file=file, asTable=asTable, sheetName=sheetName, ..., row.names=row.names, col.names=col.names)
}

#' save an xlsx file, alias of \code{\link{ez.savex}}, wrapper of \code{\link[openxlsx]{write.xlsx}} from the openxlsx package
#' @description uses openxlsx package which does not require java and is much faster, but has a slightly different interface/parameters from xlsx package.
#' @param creator A string specifying the workbook author
#' @param sheetName Name of the worksheet
#' @param gridLines A logical. If FALSE, the worksheet grid lines will be hidden.
#' @param startCol A vector specifiying the starting column(s) to write df
#' @param startRow A vector specifiying the starting row(s) to write df
#' @param xy An alternative to specifying startCol and startRow individually. A vector of the form c(startCol, startRow)
#' @param colNames or col.names If TRUE, column names of x are written.
#' @param rowNames or row.names If TRUE, row names of x are written.
#' @param headerStyle Custom style to apply to column names.
#' @param borders Either "surrounding", "columns" or "rows" or NULL. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.
#' @param borderColour Colour of cell border
#' @param borderStyle Border line style.
#' @param overwrite Overwrite existing file (Defaults to TRUE as with write.table)
#' @param asTable write using writeDataTable as opposed to writeData
#' @return nothing
#' @examples
#' (x, file, sheetName="Sheet1", row.names=FALSE,
#'   col.names=TRUE)
#'
#' ## write to working directory
#' options("openxlsx.borderColour" = "#4F80BD") ## set default border colour
#' write.xlsx(iris, file = "writeXLSX1.xlsx", colNames = TRUE, borders = "columns")
#' write.xlsx(iris, file = "writeXLSX2.xlsx", colNames = TRUE, borders = "surrounding")
#' 
#' 
#' hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
#'                   fontName="Arial Narrow", fgFill = "#4F80BD")
#' 
#' write.xlsx(iris, file = "writeXLSX3.xlsx", colNames = TRUE, borders = "rows", headerStyle = hs)
#' 
#' ## Lists elements are written to individual worksheets, using list names as sheet names if available
#' l <- list("IRIS" = iris, "MTCATS" = mtcars, matrix(runif(1000), ncol = 5))
#' write.xlsx(l, "writeList1.xlsx")
#' 
#' ## different sheets can be given different parameters
#' write.xlsx(l, "writeList2.xlsx", startCol = c(1,2,3), startRow = 2,
#'            asTable = c(TRUE, TRUE, FALSE), withFilter = c(TRUE, FALSE, FALSE))
#' @export
ez.writex = ez.savex

#' Writes .mat files for exporting data to be used with Matlab, more similar to matlab save() syntax
#'
#' seems column in a data frame should be atomic if factor does not work well.
#' Writes .mat files to store R session data using the R.matlab package and
#' takes care that logicals and atomic vectors are saved properly: currently,
#' R.matlab does not write logicals and atomic vectors (not 1D arrays/ matrices)
#' in a way that they can be opened properly in Matlab (logicals will not be
#' stored and atomic vectors will be transposed in the Matlab session - but they
#' appear untransposed when read back from the .mat file into R using
#' R.matlab::readMat()). This function is a convenient wrapper for
#' R.matlab::writeMat() that stores logicals as 0 / 1 and that transposes atomic
#' vectors when saving the matfile.
#'
#' @param fn file name, a character string, with or without '.mat'
#'
#' @param vars character vector containing a comma separated listing of
#'   variables to be saved in the matfile. The variables have to exist in the
#'   environment where the function was called from. eg. 'var1,var2,   var3' with or without (extra) space
#'
#' @export
#'
#' @examples
#' A       <- matrix(c(2,3,4,2, 1,1,2,6, 8,3,9,7), 3, 4, byrow = TRUE)
#' b       <- c(3, 5, 1, 9, 18, 2) # atomic vector 1x6
#' cc      <- array(b, c(1, length(b))) # array vector 1x6
#' dd      <- t(cc) # array vector 6x1
#' myChar  <- c("from", "R", "to", "Matlab")
#' bool    <- TRUE # logical
#' k       <- FALSE
#' l       <- FALSE
#' fn      <- "mytestmat"
#' vars    <- "A, b, cc, dd, myChar, bool, k, l"
#' ez.savem(fn, vars)
#' unlink(paste(fn, ".mat", sep = ""))
#'
#' @author Christoph Schmidt <christoph.schmidt@@med.uni-jena.de>
# 17.12.15
ez.savem <- function(fn, vars){
   if(!stringr::str_detect(fn, "^.*\\.mat$")){
      fn <- paste(fn, ".mat", sep = "")
   }

   str_extractCommaSepArgs <- function(args_str){
      inp       <- list()
      ind_start <- 1
      k         <- 1
      bool      <- TRUE


      while(bool){
         args_str     <- stringr::str_sub( args_str, ind_start )
         ind_end    <- stringr::str_locate( args_str, "," )[1,1]

         if(is.na(ind_end)){
            bool    <- FALSE
            ind_end <- stringr::str_length(args_str) + 1
         }

         this_arg   <- stringr::str_sub( args_str, 1, ind_end-1 )
         inp[[k]]   <- stringr::str_trim(this_arg)
         ind_start  <- ind_end + 1
         k          <- k + 1
      }

      return(inp)
   }



   varsList <- str_extractCommaSepArgs(vars)
   saveVars <- ""

   # strange iterator name to circumvent manipulating a global variable that should
   # be saved to .mat, e.g. k, i, ...
   for(kqzuacrlk in 1:length(varsList)){
      tmpStr  <- varsList[[kqzuacrlk]]
      tmpStr_ <- paste(tmpStr, '_', sep = "") # for storing local copy of global variable from parent frame

      if( !exists(tmpStr, envir = parent.frame()) ){
         stop(paste("Input variable name: '", tmpStr,
                    "'\ndoes not correspond to a variable stored in the parent frame.", sep = ""))
      }


      tmpVal  <- get(tmpStr, envir = parent.frame())


      # is.vector(list) = TRUE, is.vector(logical) = TRUE, is.vector(array) = FALSE, is.list(vector) = FALSE
      # does not transpose one-dimensional matrices / arrays (they don't need to be transformed)
      if(is.vector(tmpVal) && !is.list(tmpVal) && !is.logical(tmpVal)){
         assign(tmpStr_, t(tmpVal)) # create new local variable containing the transpose of the corresponding parent.frame atomic vector
      }
      else if(is.logical(tmpVal)){
         if(tmpVal){
            assign(tmpStr_, 1) # create new local variable containing the corresponding integer encoding of the corresponding parent.frame logical
         }
         else {
            assign(tmpStr_, 0)
         }
      }
      else {
         assign(tmpStr_, tmpVal)
      }


      saveVars <- paste(saveVars, tmpStr, " = ", tmpStr_, ",", sep = "")
   }


   saveVars <- stringr::str_sub(saveVars, end = -2L) # deleting last comma



   ### saving .mat file ---
   f <- paste("R.matlab::writeMat('", fn, "', ", saveVars, ")", sep = "")
   eval(parse(text=f))
}

#' Writes .mat files for exporting data to be used with Matlab, more similar to matlab save() syntax
#'
#' seems column in a data frame should be atomic if factor does not work well.
#' Writes .mat files to store R session data using the R.matlab package and
#' takes care that logicals and atomic vectors are saved properly: currently,
#' R.matlab does not write logicals and atomic vectors (not 1D arrays/ matrices)
#' in a way that they can be opened properly in Matlab (logicals will not be
#' stored and atomic vectors will be transposed in the Matlab session - but they
#' appear untransposed when read back from the .mat file into R using
#' R.matlab::readMat()). This function is a convenient wrapper for
#' R.matlab::writeMat() that stores logicals as 0 / 1 and that transposes atomic
#' vectors when saving the matfile.
#'
#' @param fn file name, a character string, with or without '.mat'
#'
#' @param vars character vector containing a comma separated listing of
#'   variables to be saved in the matfile. The variables have to exist in the
#'   environment where the function was called from. eg. 'var1,var2,   var3' with or without (extra) space
#'
#' @export
#'
#' @examples
#' A       <- matrix(c(2,3,4,2, 1,1,2,6, 8,3,9,7), 3, 4, byrow = TRUE)
#' b       <- c(3, 5, 1, 9, 18, 2) # atomic vector 1x6
#' cc      <- array(b, c(1, length(b))) # array vector 1x6
#' dd      <- t(cc) # array vector 6x1
#' myChar  <- c("from", "R", "to", "Matlab")
#' bool    <- TRUE # logical
#' k       <- FALSE
#' l       <- FALSE
#' fn      <- "mytestmat"
#' vars    <- "A, b, cc, dd, myChar, bool, k, l"
#' ez.writem(fn, vars)
#' unlink(paste(fn, ".mat", sep = ""))
#'
#' @author Christoph Schmidt <christoph.schmidt@@med.uni-jena.de>
# 17.12.15
#' @export
ez.writem = ez.savem

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

#' Prints/Directs output to both terminal and a file (log.txt) globally.
#' @param mode a=append; w=overwrite
#' @param timestamp T=insert timestamp at the beginning and end, F=otherwise
#' @param status T=open the redirection/file, F=close the redirection
#' @return nothing
#' @examples
#' @seealso \code{\link{ez.print}}
#' @export
ez.log = function(file='log.txt',mode='a',status=TRUE,timestamp=TRUE){
    append = ifelse(mode=='a',TRUE,FALSE)
    if (status) {
        sink(file,append=append,split=TRUE)
        if (timestamp) {cat(sprintf('log on at %s...', date()))}
    }
    else {
        if (timestamp) {cat(sprintf('log off at %s...\n\n', date()))}
        sink()
    }
}
