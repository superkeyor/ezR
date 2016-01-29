# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# environment, file, folder, os
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# options(warn=-1)  # turn off warning
# options(warn=0)  # turn back on warning

#' alias of \code{\link{stop}}
#' @param
#' @return
#' @examples
#' @export
z.error = stop

#' print the version of a package
#' @param pkg package name in quotes, default is NULL
#' @return if pkg not provided, prints only R version
#' @examples
#' @export
z.ver = function(pkg=NULL){
    if (!is.null(pkg)) {
        cat(sprintf("%s\n%s: %s", R.version.string, pkg, as.character(packageVersion(pkg))))
    } else {
        cat(R.version.string)
    }
}

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

#' alias of \code{\link{devtools::install_github}}
#' @param
#' @return
#' @examples
#' @export
z.github = devtools::install_github

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

#' clear, clean  console, workspace, plot
#' @param area 0 all, 1 console only, 2 workspace only, 3 plot only
#' @return
#' @examples
#' @export
z.clear = function(area=0) {
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

#' clear, clean  console, workspace, plot
#' @param area 0 all, 1 console only, 2 workspace only, 3 plot only
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

#' alias of \code{\link{sessionInfo}}, \code{\link{z.who}}
#' @param
#' @return Print version information about R, the OS and attached or loaded packages.
#' @examples
#' @export
#' @seealso \code{\link{objects}}
z.whos = sessionInfo

#' alias of \code{\link{sessionInfo}}, \code{\link{z.whos}}
#' @param
#' @return Print version information about R, the OS and attached or loaded packages.
#' @examples
#' @export
#' @seealso \code{\link{objects}}
z.who = z.whos

#' user path like in Matlab
#' @description alias of \code{\link{search}}
#' @param
#' @return
#' @examples
#' @export
z.path = search

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

#' splitpath
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
