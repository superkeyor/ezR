# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# import/export data file
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' read csv table, wrapper of \code{\link{read.csv}}
#' @description read csv table, wrapper of \code{\link{read.csv}}
#' @param tolower whether to convert all column names to lower case
#' @param skip.rows rows to skip (1 based) before read in, eg 1:3
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
ez.read = function(file, ..., skip.rows=NULL, tolower=FALSE){
    if (!is.null(skip.rows)) {
        tmp = readLines(file)
        tmp = tmp[-(skip.rows)]
        tmpFile = tempfile()
        on.exit(unlink(tmpFile))
        writeLines(tmp,tmpFile)
        file = tmpFile
    }
    result = read.csv(file, ...)
    if (tolower) names(result) = tolower(names(result))
    return(result)
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.write}}, wrapper of \code{\link{write.csv}}
#' @description wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.write}}, wrapper of \code{\link{write.csv}}
#' @return returns file path
#' @examples
#' (x, file="RData.csv", row.names=FALSE, col.names=TRUE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' sep: not applicable when file is csv
#' @export
ez.save = function(x, file="RData.csv", row.names=FALSE, col.names=TRUE, na = "", ...){
    # hack to remove row.names, http://stackoverflow.com/questions/12117629/
    x = data.frame(x)
    if (row.names==FALSE) {rownames(x) <- NULL}
    if (col.names==TRUE & tools::file_ext(file)=='csv') {
        write.csv(x=x, file=file, row.names=row.names, na=na, ...)
    }else{
        # hack to not save col names http://stackoverflow.com/a/19227265/2292993
        # why not write.table for both? 
        # because write.table can be slow for data frames with large numbers (hundreds or more) of columns
        write.table(x=x, file=file, row.names=row.names, col.names=col.names, na=na, ...)
    }
    return(invisible(file))
}

#' wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.save}}, wrapper of \code{\link{write.csv}}
#' @description wrapper of write.csv, but with row.names removed, alias of \code{\link{ez.save}}, wrapper of \code{\link{write.csv}}
#' @examples
#' (x, file="RData.csv", row.names=FALSE, append = FALSE, quote = TRUE, sep = ",",
#'             na = "NA", dec = ".",
#'             col.names = TRUE, qmethod = c("escape", "double"),
#'             fileEncoding = "")
#' dec: decimal point
#' @export
ez.write = ez.save

#' read an xlsx file, wrapper of \code{\link[xlsx]{read.xlsx}} from the xlsx package, internally trim (leading and trailing) string spaces
#' @description read an xlsx file, wrapper of \code{\link[xlsx]{read.xlsx}} from the xlsx package, internally trim (leading and trailing) string spaces
#' @param tolower whether to convert all column names to lower case
#' @param stringsAsFactors T/F 
#' @return when stringsAsFactors=T, in the returned data frame, string to factor
#' \cr number stored as text in excel (->string) -> factor
#' @examples
#' read.xlsx(file, sheetIndex, sheetName=NULL, rowIndex=NULL,
#'           startRow=NULL, endRow=NULL, colIndex=NULL,
#'           as.data.frame=TRUE, header=TRUE, colClasses=NA,
#'           keepFormulas=FALSE, encoding="unknown", ...)
#' colClasses: Only numeric, character, Date, POSIXct, column types are accepted
#' colClasses=c("Date", "character","integer", rep("numeric", 2),  "POSIXct")
#' @export
ez.readx2 = function(file, sheetIndex=1, tolower=FALSE, stringsAsFactors=TRUE, ...){
    result = xlsx::read.xlsx(file, sheetIndex, ...)
    if (tolower) names(result) = tolower(names(result))
    # char to factor
    if (stringsAsFactors) result[sapply(result, is.character)] <- lapply(result[sapply(result, is.character)], as.factor)
    # trim spaces
    result[]=lapply(result, function(x) if (is.factor(x)) factor(trimws(x,'both')) else x)
    result[]=lapply(result, function(x) if(is.character(x)) trimws(x,'both') else(x))
    return(result)
}

#' read an xlsx file, wrapper of \code{\link[openxlsx]{read.xlsx}}
#' @description uses openxlsx package which does not require java and is much faster, but has a slightly different interface/parameters from xlsx package. internally trim (leading and trailing) string spaces
#' @param tolower whether to convert all column names to lower case
#' @param stringsAsFactors T/F 
#' @return when stringsAsFactors=T, in the returned data frame, string to factor
#' \cr number stored as text in excel (->string) -> factor
#' @examples
#' read.xlsx(xlsxFile, sheet = 1, startRow = 1, colNames = TRUE,
#'          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#'          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
#' @export
ez.readx = function(file, sheet=1, tolower=FALSE, stringsAsFactors=TRUE, ...){
    result = openxlsx::read.xlsx(file, sheet, ...)
    if (tolower) names(result) = tolower(names(result))
    # char to factor
    if (stringsAsFactors) result[sapply(result, is.character)] <- lapply(result[sapply(result, is.character)], as.factor)
    # trim spaces
    result[]=lapply(result, function(x) if (is.factor(x)) factor(trimws(x,'both')) else x)
    result[]=lapply(result, function(x) if(is.character(x)) trimws(x,'both') else(x))
    return(result)
}

#' read an xlsx file, returns and prints sheet names, wrapper of \code{\link[openxlsx]{getSheetNames}} from the openxlsx package
#' @description read an xlsx file, returns and prints sheet names, wrapper of \code{\link[openxlsx]{getSheetNames}} from the openxlsx package
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

#' THE UNDERLYING HAVEN V0.2.1 ALWAYS CONVERTS USER MISSING TO NA, SO usrna HAS NO EFFECT HERE. OTHERWISE THE FUNCTION WORKS GENERALLY FINE and almost the same as ez.reads. For a more perfect function, use ez.reads. This function kept as an archive just in case.
#' wrapper of \code{\link{sjmisc_read_spss}}. read spss .sav file with haven package
#' @description Internally trim (leading and trailing) string spaces (The leading could be user written, the trailing could come from SPSS padding to Width). Will NOT auto replace col names as ez.reads will do (ie, keep them as is, @->@)
#' @param path File path to the data file
#' @param atm2fac c(1,2,3). atomic means logic,numeric/double,integer,character/string etc. Char to factor controlled separately by stringsAsFactors.
#' \cr 1: atomic with a value.label/attribute kept as is (eg, gender 1/2 numeric). SPSS value label kept as R attribute (Male/Female). 
#' \cr 2: atomic with a value.label/attribute converted to factor (eg, gender 1/2 factor). SPSS value label kept as R attribute (Male/Female). Should be desirable most of time.
#' \cr 3: atomic with a value.label/attribute converted to factor, also factor values replaced by value labels (eg, gender Male/Female factor). No R attribute. Useful for plotting.
#' @param usrna if TRUE, honor/convert user-defined missing values in SPSS to NA after reading into R; if FALSE, keep user-defined missing values in SPSS as their original codes after reading into R. 
#' @param tolower whether to convert all column names to lower case
#' @param stringsAsFactors T/F 
#' @export
ez.reads2 = function(path, atm2fac=2, usrna=TRUE, tolower=FALSE, stringsAsFactors=TRUE, ...){
    if (atm2fac==1) {
        result = sjmisc_read_spss(path=path, atomic.to.fac=FALSE, keep.na=!usrna, ...)
    } else if (atm2fac==2) {
        result = sjmisc_read_spss(path=path, atomic.to.fac=TRUE, keep.na=!usrna, ...)
    } else if (atm2fac==3) {
        result = sjmisc_read_spss(path=path, atomic.to.fac=TRUE, keep.na=!usrna, ...)
        result = ez.2label(result)
    }
    if (tolower) names(result) = tolower(names(result))
    # the atm2fac/atomic.to.fac only works for variable with numbers with labels/attributes (gender 1/2)
    # not string values (group control/patient)
    # here is a hack from http://stackoverflow.com/a/20638742/2292993
    if (stringsAsFactors) result[sapply(result, is.character)] <- lapply(result[sapply(result, is.character)], as.factor)
    # another hack to trim both leading and trailing spaces (sjmisc_read_spss only trims trailing)
    result[]=lapply(result, function(x) if (is.factor(x)) factor(trimws(x,'both')) else x)
    result[]=lapply(result, function(x) if(is.character(x)) trimws(x,'both') else(x))
    return(result)
}

#' read spss .sav file with foreign package
#' @description internally trim (leading and trailing) string spaces (The leading could be user written, the trailing could come from SPSS padding to Width). 
#' \cr SPSS numeric -> R numeric
#' \cr SPSS string (could be string of num) -> R character...then, when stringsAsFactors=T... -> R factor.
#' \cr SPSS Type (numeric, string) matters, but Measure (scale, ordinal, nominal) seems to not matter
#' \cr See param for more other details.
#' \cr Also somehow auto replace irregular chars in the col names (eg, @-->.)
#' @param atm2fac c(1,2,3). atomic means logic,numeric/double,integer,character/string etc. Char to factor controlled separately by stringsAsFactors.
#' \cr 1: atomic with a value.label/attribute kept as is (eg, gender 1/2 numeric). SPSS value label kept as R attribute (Male/Female). 
#' \cr 2: atomic with a value.label/attribute converted to factor (eg, gender 1/2 factor). SPSS value label kept as R attribute (Male/Female). Should be desirable most of time.
#' \cr 3: atomic with a value.label/attribute converted to factor, also factor values replaced by value labels (eg, gender Male/Female factor). No R attribute. Useful for plotting.
#' @param usrna if TRUE, honor/convert user-defined missing values in SPSS to NA after reading into R; if FALSE, keep user-defined missing values in SPSS as their original codes after reading into R. Should generally be TRUE, because most R stuff does not auto recognize attr well. 
#' @param tolower whether to convert all column names to lower case
#' @param stringsAsFactors T/F 
#' @note As of Nov, 2017, haven package eariler version is somewhat buggy, less powerful, but has been evolving a lot. I am not going to update haven right now. So stick with foreign. Potentially, one can also use SPSS R plugin to pass data between SPSS and R.
#' @export
ez.reads = function(file, atm2fac=2, usrna=TRUE, tolower=FALSE, stringsAsFactors=TRUE, ...){

    if (atm2fac==1) {
        atm2fac=FALSE
        lbl2val=FALSE
    } else if (atm2fac==2) {
        atm2fac=TRUE
        lbl2val=FALSE
    } else if (atm2fac==3) {
        atm2fac=TRUE  # T/F does not matter, essentially 'overwritten' by lbl2val
        lbl2val=TRUE
    }

    # can safely ignore the warnings about type 7 and etc; data is not lost
    # # http://stackoverflow.com/questions/3136293/read-spss-file-into-r

    # for variable label, it is possible to hack to make varialbe label->column name
    # because the function stores read-in info in attributes, see
    # http://www.r-bloggers.com/migrating-from-spss-to-r-rstats/
    # http://stackoverflow.com/questions/19914962/
    # but I do not wanna bother, because sometimes the variable label could have unusal strings (eg punctuation)
    
    # trick to provide an option (set to.data.frame=F to get a list)
    # foreign::read.spss auto string->factor (since 0.8-69 you are allowed to pass stringsAsFactors)
    # internally, foreign::read.spss calls as.data.frame which is udner the influence of stringsAsFactors
    oldOpts = options(stringsAsFactors=stringsAsFactors)
    result = suppressWarnings(foreign::read.spss(file, use.value.labels = lbl2val, to.data.frame = TRUE,
                                                 max.value.labels = Inf, trim.factor.names = TRUE,
                                                 trim_values = TRUE, reencode = NA, use.missings = usrna, ...))
    options(oldOpts)
    if (tolower) names(result) = tolower(names(result))
    # Important: cannot trim trailing (and leading) string space 
    # trim.factor.names, trim_values in \code{\link[foreign]{read.spss}} seems not working??? 
    # --Where does the trailing spaces come from --String var padded to Width in SPSS
    # hack to remove leading and trailing string spaces
    result[]=lapply(result, function(x) if (is.factor(x)) factor(trimws(x,'both')) else x)
    result[]=lapply(result, function(x) if(is.character(x)) trimws(x,'both') else(x))

    # hack begin: atomic with attributes to factor, do this before or after trimming spaces
    if (atm2fac) {
        atomic_w_attr_to_fac_foreign = function (data.spss) {
            # foreign packag uses 'value.labels' attr
            attr.string='value.labels'
            if (!is.null(attr.string)) {
                for (i in 1:ncol(data.spss)) {
                    x <- data.spss[[i]]
                    labs <- attr(x, attr.string, exact = T)
                    if (is.atomic(x) && !is.null(labs)) {
                        x <- as.factor(x)
                        attr(x, attr.string) <- labs
                        data.spss[[i]] <- x
                    }
                }
            }
            return(data.spss)
        }

        result = atomic_w_attr_to_fac_foreign(result)
    }
    # hack end: atomic with attributes to factor

    return(result)
}

#' alias of \code{\link{sjmisc_write_spss}}, \code{\link{ez.writes}}
#' @description potentially keep variable labels and value labels
#' @export
ez.saves = function(...){
    sjmisc_write_spss(...)
}

#' alias of \code{\link{sjmisc_write_spss}}, \code{\link{ez.saves}}
#' @description potentially keep variable labels and value labels
#' @export
ez.writes = function(...){
    sjmisc_write_spss(...)
}

#' save an xlsx file, alias of \code{\link{ez.writex2}}, wrapper of \code{\link[xlsx]{write.xlsx}} from the xlsx package
#' @description save an xlsx file, alias of \code{\link{ez.writex2}}, wrapper of \code{\link[xlsx]{write.xlsx}} from the xlsx package
#' @return returns file path
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
    return(invisible(file))
}

#' @rdname ez.savex2
#' @export
ez.writex2 = ez.savex2

#' save an xlsx file, alias of \code{\link{ez.writex}}, wrapper of \code{\link[openxlsx]{writeData}} from the openxlsx package
#' @description uses openxlsx package which does not require java and is much faster, but has a slightly different interface/parameters from xlsx package.
#' \cr Other parameters in \code{\link[openxlsx]{writeData}}. Overwrite existing file.
#' @param withFilter T/F auto add excel filter
#' @param sheetName Name of the worksheet
#' @param gridLines A logical. If FALSE, the worksheet grid lines will be hidden.
#' @param startCol A vector specifiying the starting column(s) to write df
#' @param startRow A vector specifiying the starting row(s) to write df
#' @param xy An alternative to specifying startCol and startRow individually. A vector of the form c(startCol, startRow)
#' @param colNames If TRUE, column names of x are written.
#' @param rowNames If TRUE, row names of x are written.
#' @param headerStyle Custom style to apply to column names.
#' @param borders Either "surrounding", "columns" or "rows" or NULL. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.
#' @param borderColour Colour of cell border
#' @param borderStyle Border line style.
#' @return returns file path
#' @examples
#' (x, file, sheetName="Sheet1", rowNames=FALSE,
#'   colNames=TRUE)
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
ez.savex = function(x, file="RData.xlsx", sheetName="Sheet1", withFilter=FALSE, rowNames=FALSE, colNames=TRUE, ...){
    x = data.frame(x)
    # # openxlsx::write.xlsx does not support (ie, ignore) withFilter, call underlying openxlsx::writeData
    # openxlsx::write.xlsx(x=x, file=file, asTable=FALSE, sheetName=sheetName, ..., rowNames=rowNames, colNames=colNames)
    cmd=sprintf('xlist = list("%s"=x)',sheetName)
    ez.eval(cmd)
    ez.savexlist(xlist,file=file,withFilter=withFilter,rowNames=rowNames,colNames=colNames,...)
    return(invisible(file))
}

#' @rdname ez.savex
#' @export
ez.writex = ez.savex

#' Save multiple data frames to multiple sheets individually
#' @description Save multiple data frames to multiple sheets individually
#' @param xlist a list of data frames. eg, list(sheetA=df1,sheetB=df2) where sheetA/B become sheet names; list(df1,df2) where it auto names Sheet1/2
#' \cr Other parameters in \code{\link[openxlsx]{writeData}}
#' @return returns file path
#' @export
ez.savexlist = function(xlist, file='RData.xlsx', withFilter=TRUE, rowNames = TRUE, colNames = TRUE, ...) {
    sheetNames = if (!is.null(names(xlist))) names(xlist) else paste0("Sheet",1:length(xlist))
    wb <- openxlsx::createWorkbook(creator = 'openxlsx')
    for (i in 1:length(xlist)) {
        sheet = data.frame(xlist[[i]])
        # the default rowNames=T has no col name for rowNames, and rowNames saved as string of numbers (not ideal for sorting, ie, '1', '10', '11', '2')
        # hack
        if (rowNames) {
            rowname=ez.num(rownames(sheet))
            sheet=dplyr::bind_cols(data.frame('rowname'=rowname),sheet)
        }

        # sheetName too long! Max length is 31 characters., truncate if so
        sheetName = toString(sheetNames[i],width=31)
        openxlsx::addWorksheet(wb, sheetName = sheetName)
        openxlsx::writeData(wb, sheetName, sheet,
          colNames = colNames, rowNames = FALSE, 
          withFilter = withFilter, ...)
    }
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    return(invisible(file))
}

#' Writes .mat files for exporting data to be used with Matlab, more similar to matlab save() syntax
#' @description Writes .mat files for exporting data to be used with Matlab, more similar to matlab save() syntax
#' \cr 
#' \cr seems column in a data frame should be atomic if factor does not work well.
#' \cr Writes .mat files to store R session data using the R.matlab package and
#' \cr takes care that logicals and atomic vectors are saved properly: currently,
#' \cr R.matlab does not write logicals and atomic vectors (not 1D arrays/ matrices)
#' \cr in a way that they can be opened properly in Matlab (logicals will not be
#' \cr stored and atomic vectors will be transposed in the Matlab session - but they
#' \cr appear untransposed when read back from the .mat file into R using
#' \cr R.matlab::readMat()). This function is a convenient wrapper for
#' \cr R.matlab::writeMat() that stores logicals as 0 / 1 and that transposes atomic
#' \cr vectors when saving the matfile.
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

#' @rdname ez.savem
#' @export
ez.writem = ez.savem

#' show the content of a file in read-only mode, wrapper of wrapper of \code{\link{file.show}}
#' @description show the content of a file in read-only mode, wrapper of wrapper of \code{\link{file.show}}
#' @export
ez.type = function(path){
    result = file.show(path,title='File (read-only)')
}

#' edit a file, wrapper of wrapper of \code{\link{file.edit}}
#' @description edit a file, wrapper of wrapper of \code{\link{file.edit}}
#' @export
ez.edit = function(path){
    result = file.edit(path)
}

#' Prints/Directs output to both terminal and a file (log.txt) globally, wrapper of \code{\link{sink}}
#' @description recommend to use logon(), logoff(), which are convenient shortcuts of log()
#' @param mode a=append; w=overwrite
#' @param timestamp T=insert timestamp at the beginning and end, F=otherwise
#' @param status T=open the redirection/file, F=close the redirection
#' @return nothing
#' @examples
#' # logging on: log("thelog.txt")
#' # logging off: log(status=False)  # no need to pass in the file parameter
#' @seealso \code{\link{ez.print}}
#' @export
ez.log = function(file='log.txt',mode='a',status=TRUE,timestamp=TRUE){
    append = ifelse(mode=='a',TRUE,FALSE)
    if (status) {
        sink(file,append=append,split=TRUE)
        if (timestamp) {cat(sprintf('...log on at %s...\n', date()))}
    }
    else {
        if (timestamp) {cat(sprintf('...log off at %s...\n\n', date()))}
        sink()
    }
}

#' Prints/Directs output to both terminal and a file (log.txt) globally, wrapper of \code{\link{ez.log}}
#' @description Prints/Directs output to both terminal and a file (log.txt) globally, wrapper of \code{\link{ez.log}}
#' @param mode a=append; w=overwrite
#' @param timestamp T=insert timestamp at the beginning and end, F=otherwise
#' @param status T=open the redirection/file, F=close the redirection
#' @return nothing
#' @examples
#' # logging on: logon()
#' # logging off: logoff()  # does not accept any parameter
#' @seealso \code{\link{ez.print}}
#' @export
ez.logon = function(file='log.txt',mode='a',status=TRUE,timestamp=TRUE){
    ez.log(file=file,mode=mode,status=status,timestamp=timestamp)
}

#' Prints/Directs output to both terminal and a file (log.txt) globally, wrapper of \code{\link{ez.log}}
#' @description does not accept any parameter
#' @return nothing
#' @examples
#' # logging on: logon()
#' # logging off: logoff()  # does not accept any parameter
#' @seealso \code{\link{ez.print}}
#' @export
ez.logoff = function(){
    ez.log(status=FALSE)
}