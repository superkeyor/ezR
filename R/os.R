# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# environment, file, folder, os
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# options(warn=-1)  # turn off warning
# options(warn=0)  # turn back on warning

#' open a local file with default program (xlsx etc)
#' @description open a local file with default program (xlsx etc)
#' @note \code{\link{browseURL}} does the same
#' @export
ez.open <- function(filePath) {
    # https://stackoverflow.com/a/10969105/2292993
    f=filePath
    if (missing(f)) {
        stop('No file to open!')
    }
    f <- path.expand(f)
    if (!file.exists(f)) {
        stop('File not found!')
    }
    if (grepl('w|W', .Platform$OS.type)) {
        ## we are on Windows
        shell.exec(f) #nolint
    } else {
        if (grepl('darwin', version$os)) {
            ## Mac
            system(paste(shQuote('open'), shQuote(f)), wait = FALSE, ignore.stderr = TRUE)
        } else {
            ## Linux
            system(paste(shQuote('/usr/bin/xdg-open'), shQuote(f)), #nolint
                   wait = FALSE,
                   ignore.stdout = TRUE)
        }
    }
}

#' alias of \code{\link{stop}}
#' @description alias of \code{\link{stop}}
#' @export
ez.error = stop

#' exit without prompt to save workspace image
#' @description exit without prompt to save workspace image
#' @export
exit = function() { q("no") }

#' debug mode on/off 1/0 T/F. NULL-> getOption('debug')
#' @description debug mode on/off 1/0 T/F. NULL-> getOption('debug')
#' @export
ez.debug = function(debugMode=NULL) { 
    if (is.null(debugMode)) {
        opt = getOption('debug')
        if (is.null(opt)) ez.pprint('Debug Mode Status: Not set yet')
        else if (opt==TRUE) ez.pprint('Debug Mode Status: On')
        else if (opt==FALSE) ez.pprint('Debug Mode Status: Off')
    }
    else if (debugMode==1) {options(debug=T); ez.pprint('Debug Mode Status: On')}
    else if (debugMode==0) {options(debug=F); ez.pprint('Debug Mode Status: Off')}
    return(invisible(NULL))
}

#' warn error mode on/off 1/0 T/F. NULL-> getOption('warn')
#' @description warn error mode on/off 1/0 T/F. NULL-> getOption('warn')
#' @export
ez.warn = function(warnErrorMode=NULL) { 
    if (is.null(warnErrorMode)) {
        opt = getOption('warn')
        if (is.null(opt)) ez.pprint('Warn Error Mode Status: Not set yet')
        else if (opt==2) ez.pprint('Warn Error Mode Status: On')
        else if (opt==1) ez.pprint('Warn Error Mode Status: Off')
    }
    else if (warnErrorMode==1) {options(warn=2); ez.pprint('Warn Error Mode Status: On')}
    else if (warnErrorMode==0) {options(warn=1); ez.pprint('Warn Error Mode Status: Off')}
    return(invisible(NULL))
}

#' print out or set the repo
#' @description print out or set the repo
#' @param repo NULL=print out current repo; 'default'='https://cran.rstudio.com/'; else like '2016-08-01'='https://mran.revolutionanalytics.com/snapshot/2016-08-01'
#' @export
#' @return returns NULL
ez.repo = function(repo=NULL){
    if (is.null(repo)) {
        message(sprintf('The current repository is: %s\n',unname(getOption("repos"))))
        return(invisible(NULL))
    }

    if (repo=='default') {
        options(repos = c(CRAN = 'https://cran.rstudio.com/'))
    } else {
        options(repos = c(CRAN = sprintf('https://mran.revolutionanalytics.com/snapshot/%s',repo)))
    }
    cat(sprintf('The repository now set to: %s\n',unname(getOption("repos"))))
    return(invisible(NULL))
}

#' switch env
#' @description switch env
#' @param env NULL=print out current env (.libPaths()[1]); use symlink to trick (see code)
#' @export
#' @return returns NULL
ez.env=function(env=NULL){
    if (is.null(env)) {
        cat(readChar('~/Dropbox/Apps/RStudio/R3.3_library/note.txt', 1e5))
        message('\n',"Using library: ", .libPaths()[1])
        return(invisible(NULL))
    }

    # if existing library is a symlink
    if (Sys.readlink('/Library/Frameworks/R.framework/Versions/3.3/Resources/library') != '') {
        file.remove('/Library/Frameworks/R.framework/Versions/3.3/Resources/library')
    }

    file.symlink(sprintf('~/Dropbox/Apps/RStudio/R3.3_library/%s/', env),
        '/Library/Frameworks/R.framework/Versions/3.3/Resources/library')

    # restart r session (restart does not reset .libPaths, so do not use)
    # https://stackoverflow.com/questions/6313079/quit-and-restart-a-clean-r-session-from-within-r
    # .rs.restartR()
    message('Please restart RStudio to make the change take effect!')
    system('killall RStudio; open -a RStudio')
    return(invisible(NULL))
}

#' @rdname ez.env
#' @export
ez.envr = ez.env

#' update ez package itself
#' @description update ez package itself
#' @export
ez.updateself = function(force=F) {
    # file.exists works for folder as well
    if (file.exists('~/Dropbox/Apps/RStudio/ezR') & !force) {
        # system('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/ezR')
        # system('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/bzR')
        # system('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/mzR')
        system('bash ~/Dropbox/Apps/RStudio/ezR/publish.sh')
        system('bash ~/Dropbox/Apps/RStudio/bzR/publish.sh')
        system('bash ~/Dropbox/Apps/RStudio/mzR/publish.sh')
        cat('Please restart RStudio to make the change take effect!\n')
        cat('Enter will restore env. #+shift+Q simply restart.\n')
        ez.pause()
        savehistory("~/.Rhistory") # since to kill RStudio soon. save by hand, will be auto restored by RStudio
        save.image('~/Downloads/tmp.rda')
        system('killall RStudio && sleep 2 && open -a RStudio', wait=FALSE)
        # system('osascript -e \'tell application "RStudio" to quit\'; open -a RStudio', wait=FALSE)
    } else {
        ez.github('jerryzhujian9/ezR')
        ez.github('jerryzhujian9/bzR')
        ez.github('jerryzhujian9/mzR')
        cat('Please restart RStudio to make the change take effect!\n')
    }


    # system2 does not seem to work well on mac??
    # tryCatch({
    #     # seems that some errors thrown by system() cannot be caught in this way
    #     # https://stackoverflow.com/questions/25991973/suppress-system-error-with-trycatch-in-r
    #     system2('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/ezR', stdout=TRUE, stderr=TRUE)
    #     system2('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/bzR', stdout=TRUE, stderr=TRUE)
    #     system2('R --vanilla CMD INSTALL --no-multiarch --with-keep.source ~/Dropbox/Apps/RStudio/mzR', stdout=TRUE, stderr=TRUE)
    #     cat('Please restart RStudio to make the change take effect!\n')
    #     system2('killall RStudio; open -a RStudio', stdout=TRUE, stderr=TRUE)
    # }, warning = function(w) {
    #     ez.github('jerryzhujian9/ezR')
    #     ez.github('jerryzhujian9/bzR')
    #     ez.github('jerryzhujian9/mzR')
    #     cat('Please restart RStudio to make the change take effect!\n')
    # }, error = function(e) {
    #     ez.github('jerryzhujian9/ezR')
    #     ez.github('jerryzhujian9/bzR')
    #     ez.github('jerryzhujian9/mzR')
    #     cat('Please restart RStudio to make the change take effect!\n')
    # }
    # )

    return(invisible(NULL))
}

#' print the version of a package
#' @description print the version of a package
#' @param pkg package name in quotes, default is NULL
#' @return if pkg not provided, prints R version, installed packages/versions and etc
#' @export
ez.ver = function(pkg=NULL){
    if (!is.null(pkg)) {
        cat(sprintf("%s\n%s: %s", R.version.string, pkg, as.character(packageVersion(pkg))))
    } else {
        ip = data.frame(installed.packages()[,c(1,3:4)])
        # ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
        ip = ip[,1:2,drop=FALSE]
        ipnames = row.names(ip)
        rownames(ip) <- NULL
        print(ip)
        print(paste0(ipnames,collapse = ','))

        cat("\n")
        message(R.version.string)
        message(sprintf("Repository: %s",getOption("repos")))
        message(sprintf("Library: %s",paste0(.libPaths(),collapse = '; ')))
    }
}

#' @rdname ez.ver
#' @export
ez.verr = ez.ver

#' alias of \code{\link{library}}
#' @description alias of \code{\link{library}}
#' @export
ez.import = library

#' alias of \code{\link{library}}
#' @description alias of \code{\link{library}}
#' @export
ez.include = ez.import

#' alias of \code{\link{install.packages}}
#' @description alias of \code{\link{install.packages}}
#' @export
ez.install = install.packages

#' install many packages at the same time
#' @description install many packages at the same time
#' @param pkgs c()
#' @param repos "https://mran.revolutionanalytics.com/snapshot/2018-01-11"  (if NULL, getOption("repos"))
#' @param load if T also load pkgs: sapply(pkgs, require, character.only = TRUE)
#' @export
ez.installs = function(pkgs,load=FALSE,repos=NULL) {
    if (is.null(repos)) repos=getOption("repos")
    new.pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
    if (length(new.pkgs)) {
        install.packages(new.pkgs, dependencies = TRUE, repos=repos)
    }
    if (load) sapply(pkgs, require, character.only = TRUE)
}

#' require a package, if not exist auto install and auto load
#' @description require a package, if not exist auto install and auto load
#' @param pkg pkg name in string
#' @param autoload auto load or not (default=TRUE)
#' @export
ez.require = function(pkg, autoload=TRUE){
    tt = sprintf("require('%s')",pkg)
    if (!eval(parse(text = tt))) {
        tt = sprintf("install.packages('%s')",pkg)
        eval(parse(text = tt))
    }
    if (autoload) {
        tt = sprintf("require('%s')",pkg)
        eval(parse(text = tt))
    }
}

#' unload a package, wrapper of detach(pkg, unload=TRUE, character.only = TRUE)
#' @description unload a package, wrapper of detach(pkg, unload=TRUE, character.only = TRUE)
#' @param pkg pkg name in string
#' @export
ez.unload = function(pkg){
    character.only = TRUE
    if(!character.only){
        pkg <- deparse(substitute(pkg))
        }
    search_item <- paste("package", pkg, sep = ":")
    while(search_item %in% search()){
        detach(search_item, unload = TRUE, character.only = TRUE)
        cat(sprintf('unloaded %s',search_item))
    }
}

#' alias of \code{\link[devtools]{install_github}}
#' @description alias of \code{\link[devtools]{install_github}}
#' @export
ez.github = devtools::install_github

#' alias of \code{\link{remove.packages}}
#' @description alias of \code{\link{remove.packages}}
#' @export
ez.remove = remove.packages

#' alias of \code{\link{remove.packages}}
#' @description alias of \code{\link{remove.packages}}
#' @export
ez.uninstall = remove.packages

#' clear, clean  console, workspace, plot or variable(s)
#' @description clear, clean  console, workspace, plot or variable(s)
#' @param area 0 all, \cr
#'             1 console only \cr
#'             2 workspace only \cr
#'             3 plot only \cr
#'             'var' particular var \cr
#'             c('var1','var2') particular vars
#' @export
ez.clear = function(area=0) {
    # area[1] used, to work around when c('var1','var2') provided
    if (area[1] == 0) {console = TRUE; workspace = TRUE; plot = TRUE}
    else if (area[1] == 1) {console = TRUE; workspace = FALSE; plot = FALSE}
    else if (area[1] == 2) {console = FALSE; workspace = TRUE; plot = FALSE}
    else if (area[1] == 3) {console = FALSE; workspace = FALSE; plot = TRUE}
    else {rm(list=area, envir = .GlobalEnv); console = FALSE; workspace = FALSE; plot = FALSE}

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

#' @rdname ez.clear
#' @export
ez.clean = ez.clear

#' @rdname ez.clear
#' @export
ez.cl = ez.clear

#' alias of \code{\link{find}}
#' @description alias of \code{\link{find}}
#' @export
ez.which = find

#' alias of \code{\link{sessionInfo}}, \code{\link{ez.who}}
#' @description alias of \code{\link{sessionInfo}}, \code{\link{ez.who}}
#' @return Print version information about R, the OS and attached or loaded packages.
#' @export
#' @seealso \code{\link{objects}}
ez.whos = sessionInfo

#' alias of \code{\link{sessionInfo}}, \code{\link{ez.whos}}
#' @description alias of \code{\link{sessionInfo}}, \code{\link{ez.whos}}
#' @export
#' @seealso \code{\link{objects}}
ez.who = ez.whos

#' user path like in Matlab
#' @description user path like in Matlab
#' @description alias of \code{\link{search}}
#' @export
ez.path = search

#' alias of \code{\link{system}}
#' @description alias of \code{\link{system}}
#' @export
ez.execute = system

#' join path
#' @description join path
#' @note \code{\link{file.path}} cannot handle/ignore trailing slash,
#' internally trim double+ and trailing slashes, though some OS treats multiple slashes as one
#' alternative: paste(..., sep=.Platform$file.sep)
#' @export
ez.joinpath = function(...) {
    sep = .Platform$file.sep
    result = gsub(paste0(sep,"{2,}"), sep, file.path(...), fixed=FALSE, perl=TRUE)
    result = gsub(paste0(sep,"$"), '', result, fixed=FALSE, perl=TRUE)
    return(result)
}

#' @rdname ez.joinpath
#' @export
ez.jp = ez.joinpath

#' splitpath
#' @description splitpath
#' @examples
#' # ez.splitpath(path)
#' # returns
#' # $dir
#' # [1] "/Users/jerry/Downloads"
#'
#' # $file
#' # [1] "026999397379845a"
#'
#' # $ext
#' # '.pdf
#' @export
ez.splitpath = function(path){
    dir = dirname(path)
    file = basename(path)
    # file = strsplit(file, "\\.")[[1]][1]
    file = tools::file_path_sans_ext(file)
    # ext = tools::file_ext(path)  # does not have . i.e., 'pdf' instead of '.pdf'
    ext = substring(basename(path),nchar(file)+1)  # substring to extract ext, see ?substring
    return(list(dir=dir, file=file, ext=ext))
}

#' @rdname ez.splitpath
#' @export
ez.sp = ez.splitpath

#' parentdir
#' @description parentdir
#' @export
ez.parentdir = function(path){dirname(path)}

#' @rdname ez.parentdir
#' @export
ez.pr = ez.parentdir

#' alias of \code{\link{setwd}}
#' @description alias of \code{\link{setwd}}
#' @export
ez.cd = setwd;

#' alias of \code{\link{getwd}}
#' @description alias of \code{\link{getwd}}
#' @export
ez.cwd = getwd

#' alias of \code{\link{getwd}}
#' @description alias of \code{\link{getwd}}
#' @export
ez.pwd = getwd

#' current script file (in full path)
#' @description current script file (in full path)
#' @examples
#' works with Rscript, source() or in RStudio Run selection
#' @export
ez.csf <- function() {
    # http://stackoverflow.com/a/32016824/2292993
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName))
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                # http://stackoverflow.com/a/35842176/2292993
                return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}

#' current script dir (in full path)
#' @description current script dir (in full path)
#' @examples
#' works with Rscript, source() or in RStudio Run selection
#' @export
ez.csd <- function() {
    return(dirname(ez.csf()))
}

#' lsd
#' @description lsd
#' @note if hidden=FALSE, not include .folders
#' @return a char vector with 0, 1 or more elements
#' @export
ez.lsd = function(path='.', pattern=NULL, full=FALSE, hidden=FALSE){
    # all.files--hidden files, include.dirs--subdirs, no..--. and .. folders
    folders = dir(path=path, pattern=pattern, all.files=hidden, full.name=FALSE,
                  recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    fullFolders = file.path(path,folders)
    infos = file.info(fullFolders)
    isdirs = infos$isdir

    if (full) {
        result=folders[isdirs]
        result=file.path(path,result)
    } else {
        result=folders[isdirs]
    }
    return(result)
}

#' ls
#' @description ls
#' @return a char vector with 0, 1 or more elements
#' @export
ez.ls = function(path='.', pattern=NULL, full=TRUE, hidden=FALSE){
    files = list.files(path = path, pattern = pattern, all.files = hidden,
                       full.names = TRUE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    isfiles = !(file.info(files)$isdir)

    if (full) {
        result=files[isfiles]
    } else {
        result=files[isfiles]
        result=basename(result)
    }
    return(result)
}

#' fls
#' @description fls
#' @export
#' @return a char vector with 0, 1 or more elements
ez.fls = function(path='.', pattern=NULL, hidden=FALSE){
    files = list.files(path = path, pattern = pattern, all.files = hidden,
                       full.names = TRUE, recursive = TRUE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
    isfiles = !(file.info(files)$isdir)
    return(files[isfiles])
}

#' mkdir, no warning for exisiting folder
#' @description mkdir, no warning for exisiting folder
#' @export
ez.mkdir = function(path){
    result = dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

#' alias of \code{\link{file.exists}}
#' @description alias of \code{\link{file.exists}}
#' @export
ez.exists = file.exists

#' remove a file, wrapper of \code{\link{unlink}}
#' @description remove a file, wrapper of \code{\link{unlink}}
#' @examples
#' support c('a.txt','b.txt')
#' x, a character vector with the names of the file(s) or directories to be deleted.
#' Wildcards (normally ‘*’ and ‘?’) are allowed.
#' 0 for success, 1 for failure.
#' @export
ez.rm = function(x){
    result = unlink(x, recursive = TRUE, force = TRUE)
}

#' rename
#' @description rename
#' @examples
#' support c('a.txt','b.txt'), c('d.txt','e.txt')
#' to parent folder must exist already; otherwise error
#' in case new name exists
#'       if old and new both folders, move old to new as subfolder
#'       if old and new both files, overwrite the new file with old file without prompt
#' @export
ez.rn = function(from,to){
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) stop("Destination parent folder does not exist")
    # both from and to are exisiting folders
    if ((nchar(tools::file_ext(from)) == 0) && (nchar(tools::file_ext(to)) == 0)) {
        if ((isTRUE(file.info(from)$isdir)) && (isTRUE(file.info(to)$isdir))) {
            result = ez.mv(from,to)
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
#' @description copy
#' @examples
#' support c('a.txt','b.txt')
#' to folder does not have to exist already
#' e.g.,
#' 1) both works ez.cp('a.txt','folder'), ez.cp('a.txt','folder/b.txt')
#' the former copy still has the same name 'a.txt', the latter copy new name 'b.txt'
#' also ez.cp(c('a.txt','b.txt'),'folder')
#' 2) folder: ez.cp('a','b')-->if b not exists, cp contents of a to b; if b exist, a becomes subfolder of b
#' kinda combines rn and mv
#' 3) regular expression
#' flist <- list.files("patha", "^filea.+[.]csv$", full.names = TRUE)
#' file.copy(flist, "pathb")
#' @export
ez.cp = function(from,to){
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
#' @description move
#' @examples
#' support c('a.txt','b.txt')
#' to parent folder does not have to exist already
#' ez.mv('a.txt','folder'), ez.mv('a.txt','folder/a.txt'), ez.mv('a.txt','folder/b.txt')
#' ez.mv('a','b')-->get b/a, b now has a as subfolder, regardless of b exists or not
#'                  use ez.rn('a','b') to change name a->b
#' @export
ez.mv = function(from,to){
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

#' Send mail with Gmail
#' @description Send mail with Gmail
#' @param to email address, if multiple seperated by comma
#' @param subject 'Gmail from R'
#' @param htmlbody '<b>Gmailr</b> is a <i>very</i> handy package!' or 'Email using R.'
#' @param attachment default NULL, 'BazaarQueriesforURLData.txt'
#' @return returns nothing
#' @note first-time use will auto pop url to save secret file, or manually gmailr::gmail_auth('compose') to generate .httr-oauth
#' @export
ez.gmail = function(to,subject,htmlbody,attachment=NULL) {
    # https://cran.r-project.org/web/packages/gmailr/vignettes/sending_messages.html
    msg = gmailr::mime()
    msg = gmailr::to(msg, to)
    msg = gmailr::from(msg, "Memory Lab <fmrimemorylab@gmail.com>")
    msg = gmailr::subject(msg, subject)
    msg = gmailr::html_body(msg, htmlbody)
    if (!is.null(attachment)) {msg = gmailr::attach_file(msg, attachment)}
    gmailr::send_message(msg)
    cat('Mail sent!', "\n")
}

#' detect os
#' @description detect os
#' @return returns 'osx','linux', 'windows', test with ==
#' @export
ez.getos = function(){
    # https://stackoverflow.com/a/40212214/2292993
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
    if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os))
            os <- "linux"
    }
    tolower(os)
}
