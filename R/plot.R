###**************************************************.
###*plot.
###**************************************************.

###**************************************************.
###*without ez. in the function name.
###**************************************************.
#' Multiple plot function
#'
#' @param ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param cols:   Number of columns in layout
#' @param layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' \cr If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' \cr then plot 1 will go in the upper left, 2 will go in the upper right, and
#' \cr 3 will go all the way across the bottom.
#' @return returns nothing (NULL)
#' @export
#' @examples
#' plots <- list()  # new empty list
#' for (i in 1:6) {
#'     p1 = qplot(1:10, rnorm(10), main = i)
#'     plots[[i]] <- p1  # add each plot into plot list
#' }
#' ggmultiplot(plotlist = plots, cols = 3)
#'
#'
#'
#' plots <- list()
#' for (i in 1:5) {
#'     p1 = qplot(1:10, rnorm(10), main = i)
#'     plots[[i]] <- p1
#' }
#' layout <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
#' ggmultiplot(plotlist = plots, layout = layout)
#'
#' @references \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook R}
ggmultiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
        }
    }
}

#' Open Help Pages for ggplot2
#'
#' \code{gghelp} - Open Hadely Wickham's ggplot2
#' \href{http://docs.ggplot2.org/current/}{web page}.
#'
#' @param FUN A particular ggplot function to reference.  Default is the index
#' page.
#' @return Opens a help web page.
#' @rdname help
#' @export
#' @author \href{https://github.com/trinker/plotflow}{trinker/plotflow}
#' @seealso \code{\link[utils]{browseURL}}
#' @examples
#' \dontrun{
#' gghelp()
#' gghelp("theme")
#' ggcook()
#' }
gghelp <- function(FUN) {
    if(missing(FUN)) FUN <- "" else FUN <- paste0(FUN, ".html")
    browseURL(sprintf("http://docs.ggplot2.org/current/%s", FUN))
}

#' Open Help Pages for ggplot2
#'
#' \code{ggcook} - Open Winston Chang's ggplot2
#' \href{http://www.cookbook-r.com/Graphs/}{Cookbook for R page}.
#'
#' @rdname help
#' @export
ggcook <- function() {
    ## browseURL("http://www.cookbook-r.com/Graphs/#graphs-with-ggplot2")
    browseURL("http://www.cookbook-r.com/Graphs/")
}

#' ggplot2 Theme for APA Publications
#'
#' A ggplot2 theme with no background and Times New Roman font (legend size not affected).
#'
#' @param titlesize default 18
#' @param textsize default 16
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @note In order for R (at least on Mac) to recognize Times New Roman font, \href{https://github.com/wch/extrafont/}{extrafont} required
#' @seealso \code{\link[ggplot2]{theme}} \code{\link{theme_apa_nosize}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect 
#' @examples
#' @author Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
theme_apa <- function(plot.box = FALSE, titlesize = 18, textsize = 16){

    if (Sys.info()["sysname"] != "Windows") {
        windowsFonts <- NULL
    }

    if (Sys.info()["sysname"] == "Windows") {
        windowsFonts(RMN=windowsFont("Times New Roman"))
        RMN <- "RMN"
    } else {
        RMN <- "Times New Roman"
    }

    out <- theme(
        plot.title=element_text(family=RMN, size=titlesize, face="bold", colour="black"),
        legend.title = element_text(family=RMN, colour="black"),
        legend.text = element_text(family=RMN, colour="black"),
        strip.text.x = element_text(family=RMN, size=textsize, colour="black"),
        strip.text.y = element_text(family=RMN, size=textsize, colour="black"),
        axis.title.x=element_text(family=RMN, size=titlesize, face="bold", colour="black"),
        axis.title.y=element_text(family=RMN, size=titlesize, face="bold", angle=90, colour="black"),
        axis.text.x=element_text(family=RMN, size=textsize, colour="black"),
        axis.text.y=element_text(family=RMN, size=textsize, colour="black"),
        axis.ticks=element_line(colour="black"))

    if (!plot.box) {
        out <- out + theme(panel.background = element_rect(fill = "white",
                                                           colour = "black"), panel.border = element_rect(fill = NA,
                                                                                                          colour = "white"), axis.line = element_line())
    } else {
        out <- out + theme(panel.background = element_rect(fill = "white",
                                                           colour = "white"), panel.border = element_rect(fill = NA,
                                                                                                          colour = "grey50"))
    }
    out

}

#' ggplot2 Theme for APA Publications
#'
#' A ggplot2 theme with no background and Times New Roman font (do not change font size).
#'
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @note In order for R (at least on Mac) to recognize Times New Roman font, \href{https://github.com/wch/extrafont/}{extrafont} required
#' @seealso \code{\link[ggplot2]{theme}} \code{\link{theme_apa}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect 
#' @examples
#' @author Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
theme_apa_nosize <- function(plot.box = FALSE){

    if (Sys.info()["sysname"] != "Windows") {
        windowsFonts <- NULL
    }

    if (Sys.info()["sysname"] == "Windows") {
        windowsFonts(RMN=windowsFont("Times New Roman"))
        RMN <- "RMN"
    } else {
        RMN <- "Times New Roman"
    }

    out <- theme(
        plot.title=element_text(family=RMN, face="bold", colour="black"),
        legend.title = element_text(family=RMN, colour="black"),
        legend.text = element_text(family=RMN, colour="black"),
        strip.text.x = element_text(family=RMN, colour="black"),
        strip.text.y = element_text(family=RMN, colour="black"),
        axis.title.x=element_text(family=RMN, colour="black"),
        axis.title.y=element_text(family=RMN, angle=90, colour="black"),
        axis.text.x=element_text(family=RMN, colour="black"),
        axis.text.y=element_text(family=RMN, colour="black"),
        axis.ticks=element_line(colour="black"))

    if (!plot.box) {
        out <- out + theme(panel.background = element_rect(fill = "white",
                                                           colour = "black"), panel.border = element_rect(fill = NA,
                                                                                                          colour = "white"), axis.line = element_line())
    } else {
        out <- out + theme(panel.background = element_rect(fill = "white",
                                                           colour = "white"), panel.border = element_rect(fill = NA,
                                                                                                          colour = "grey50"))
    }
    out

}

#' show some help info on color
#' @examples
#' RColorBrewer::display.brewer.all()
#' # returns 8 colors from Set3 (which supports up to 12 colors)
#' cols <- RColorBrewer::brewer.pal(8,"Set3")
#' # generates 100 colors based on the 9 from the Blues palette
#' colorRampPalette(brewer.pal(9,"Blues"))(100)
#' scale_colour_manual(values = cols, breaks = c("4", "6", "8"), labels = c("four", "six", "eight"))
#' @export
ggcolor = function(){
    if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
    }
    RColorBrewer::display.brewer.all()
    cat('usage: \n
        RColorBrewer::display.brewer.all()\n

        # returns 8 colors from Set3 (which supports up to 12 colors)\n
        cols <- RColorBrewer::brewer.pal(8,"Set3")\n

        # generates 100 colors based on the 9 from the Blues palette\n
        colorRampPalette(brewer.pal(9,"Blues"))(100)\n

        # scale_color_manual (for points, lines, and outlines)\n
        # scale_fill_manual (for boxes, bars, and ribbons)\n
        # scale_color_manual(values = cols, breaks = c("4", "6", "8"), labels = c("four", "six", "eight"))\n

        # also visit http://colorbrewer2.org/\n
        # scale_color_manual(values=c("#fdae61","#2b83ba","#d7191c","#abdda4","#ffffbf")) #printer-friendly\n
        # scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")) #colorblind-friendly\n')
}

#' change plot continous color to matlab like
#' @param n how many colors, e.g., 100 (default)
#' @export
#' @rdname matlabcolor
#' @examples p + matlabcolor(), p + matlabcolor2()
matlabcolor <- function(n=100){
    out = scale_fill_gradientn(colors=colorRamps::matlab.like(n))
    return(out)
}

#' @rdname matlabcolor
#' @export
matlabcolor2 <- function(n=100){
    out = scale_fill_gradientn(colors=colorRamps::matlab.like2(n))
    return(out)
}

###**************************************************.
###*with ez. in the function name.
###**************************************************.
#' wrapper of \code{\link{dev.copy2pdf}}
#' @param
#' @return
#' @note internally also use \code{\link[extrafont]{embed_fonts}} to embed fonts
#' @seealso \code{\link{pdf}} \code{\link{ez.pdfon}} \code{\link{ez.pdfoff}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11 (portrait default ie, 11 x 8.5)
#' @export
ez.export = function(filename = "RPlot.pdf", pdf.width = 11, pdf.height = 8.5, ...) {
    dev.copy2pdf(file=filename, width = pdf.width, height = pdf.height, ...)
    extrafont::embed_fonts(filename)
    # If outfile is not specified, it will overwrite the original file
    cat('Image exported. Font embedded.\n')
}

#' wrapper of \code{\link{pdf}}, direct all plotting to a pdf file(s)
#' @param 
#' @return
#' @note additionally one can use \code{\link[extrafont]{embed_fonts}} to embed fonts after dev.off()
#' @seealso \code{\link{ez.export}} \code{\link{ez.pdfoff}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11
#' ez.pdfon("Living_All.pdf",width=10,height=10,onefile=T)
#' ez.plot(jd.result,"ageout_rural|group")
#' ez.plot(jd.result,"n_rural|group")
#' ez.pdfoff()
#' @export
ez.pdfon = pdf

#' wrapper of \code{\link{dev.off}}
#' @param
#' @return
#' @note additionally one can use \code{\link[extrafont]{embed_fonts}} to embed fonts after dev.off()
#' @seealso \code{\link{ez.export}} \code{\link{ez.pdfon}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11
#' @export
ez.pdfoff = dev.off

#' subplot, wrapper of \code{\link{par}}
#' @param
#' @return
#' @examples
#' subplot(n,m,...) divides the current figure into an n-by-m grid
#' A vector of the form c(n, m). Subsequent figures will be drawn in an n-by-m array on the device
#' by columns (mfcol), or rows (mfrow), respectively.
#' see more ?par
#' @export
ez.subplot = function(n, m, ...){
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
#'
#' plot(1:10, 1:10, main = "Plot 1")
#' # draw another arbitrary subplot
#' ez.embed(plot(10,10, xlab="", ylab=""), x=2, y=8, size = c(0.5, 0.5) )
#'
#' # demonstrates mfg usage, draw the third plot before the drawing the second
#' par(mfg=c(3,1) ); plot(11:20, 11:20, main = "Plot 3")
#' ez.embed(plot(10,10, xlab="", ylab=""), x=12, y=18, size = c(0.5, 0.5))
#'
#' par(mfg=c(2,1)); plot(21:30, 21:30, main = "Plot 2")
#' ez.embed(plot(10,10, xlab="", ylab=""), x=22, y=28, size = c(0.5, 0.5))
#' @export
ez.embed = function(fun, x, y=NULL, size=c(1,1), vadj=0.5, hadj=0.5,
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

#' plot a customized boxplot with jittered stripplot, violin, and mean
#' @param df data frame in long format
#' @param cmd like "y", "y|x z a", "y|x z" or "y|x" where y is continous, x z a are discrete
#' @param violin plot violin or not
#' @param show.n show n (number of samples for each level)  or not
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
ez.plot = function(df,cmd,violin=FALSE,show.n=TRUE){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    # also see trimws()--remove trailing/leading spaces
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    cmd = strsplit(cmd,"|",fixed=TRUE)[[1]]

    violin = ifelse(violin, 'geom_violin() +', '')
    # yy
    if (length(cmd)==1) {
        yy = cmd[1]
        xx = 'DummyDiscreteVariable'
        df['DummyDiscreteVariable'] = 1
        df=ez.dropna(df,yy)
        # hide x axis label in this case
        tt = sprintf('
                     fun_length <- function(x){return(data.frame(y=min(x),label= paste0(length(x)," (n)")))}  # http://stackoverflow.com/a/15720769/2292993
                     pp = ggplot2::ggplot(df, aes(x=%s, y=%s)) +
                     stat_boxplot(geom = "errorbar", width = 0.5) +
                     %s geom_boxplot(outlier.shape=NA) + # avoid plotting outliers twice from geom_jitter
                     geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                     stat_summary(fun.y=mean, color="darkred", geom="point", shape=18, size=3) +
                     theme(legend.position="none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
                     xlab("") +
                     ggtitle(paste0("N = ",nrow(df)))'
                     , xx, yy, violin
        )
        if (show.n) {tt = paste0(tt, ' + \nstat_summary(fun.data = fun_length, color="grey", geom="text",vjust=1.2)')}
        tt = paste0(tt, ' + \nstat_summary(fun.y=mean, color="darkred", geom="text",vjust=-0.7, aes(label=sprintf("%.2f (M)", ..y..)), alpha=1) # ..y.. internal variable computed mean')
    # yy|xx or yy|xx zz
    } else {
        yy = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        xx = strsplit(xx," ",fixed=TRUE)[[1]]
        # yy|xx
        if (length(xx)==1) {
            xx = xx[1]
            df=ez.dropna(df,c(yy,xx))

            if (!is.factor(df[[xx]])) {df = ez.2factor(df,col=xx)}
            eval(parse(text = sprintf('pvalue = summary(aov(df$%s ~ df$%s))[[1]][["Pr(>F)"]][[1]]', yy, xx)))
            if (pvalue<.001) {
                pvalue = sprintf(", p = %.2e", pvalue)
            } else if (pvalue<.01) {
                pvalue = sprintf(", p = %.3f", pvalue)
            } else {
                pvalue = sprintf(", p = %.2f", pvalue)
            }

            tt = sprintf('
                         fun_length <- function(x){return(data.frame(y=min(x),label= paste0(length(x)," (n)")))}  # http://stackoverflow.com/a/15720769/2292993
                         pp = ggplot2::ggplot(df, aes(x=%s, y=%s, fill=%s)) +
                         stat_boxplot(geom = "errorbar", width = 0.5) +
                         %s geom_boxplot(outlier.shape=NA) + # avoid plotting outliers twice from geom_jitter
                         geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                         stat_summary(fun.y=mean, color="royalblue", geom="point", shape=18, size=3) +
                         theme(legend.position="none") +
                         ggtitle(paste0("N = ",nrow(df), "%s"))'
                         , xx, yy, xx, violin, pvalue
            )
            if (show.n) {tt = paste0(tt, ' + \nstat_summary(fun.data = fun_length, color="grey", geom="text",vjust=1.2)')}
            tt = paste0(tt, ' + \nstat_summary(fun.y=mean, color="royalblue", geom="text",vjust=-0.7, aes(label=sprintf("%.2f (M)", ..y..)), alpha=1) # ..y.. internal variable computed mean')
        # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                df=ez.dropna(df,c(yy,xx,zz))
                if (!is.factor(df[[xx]])) {df = ez.2factor(df,col=xx)}
                if (!is.factor(df[[zz]])) {df = ez.2factor(df,col=zz)}

                tt = sprintf('
                             fun_length <- function(x){return(data.frame(y=min(x),label= paste0(length(x)," (n)")))}  # http://stackoverflow.com/a/15720769/2292993
                             pp = ggplot2::ggplot(df, aes(x=%s, y=%s, fill=%s)) +
                             stat_boxplot(geom = "errorbar", width = 0.5) +
                             %s geom_boxplot(outlier.shape=NA) + # avoid plotting outliers twice from geom_jitter
                             geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                             stat_summary(fun.y=mean, color="royalblue", geom="point", shape=18, size=3) +
                             facet_grid(~%s) +
                             theme(legend.position="none") +
                             ggtitle(paste0("N = ",nrow(df)))'
                             , xx, yy, xx, violin, zz
                )
                if (show.n) {tt = paste0(tt, ' + \nstat_summary(fun.data = fun_length, color="grey", geom="text",vjust=1.2)')}
                tt = paste0(tt, ' + \nstat_summary(fun.y=mean, color="royalblue", geom="text",vjust=-0.7, aes(label=sprintf("%.2f (M)", ..y..)), alpha=1) # ..y.. internal variable computed mean')
            # yy|xx zz aa
            } else {
                aa = xx[3]
                zz = xx[2]
                xx = xx[1]
                df=ez.dropna(df,c(yy,xx,zz,aa))
                if (!is.factor(df[[xx]])) {df = ez.2factor(df,col=xx)}
                if (!is.factor(df[[zz]])) {df = ez.2factor(df,col=zz)}
                if (!is.factor(df[[aa]])) {df = ez.2factor(df,col=aa)}

                tt = sprintf('
                             fun_length <- function(x){return(data.frame(y=min(x),label= paste0(length(x)," (n)")))}  # http://stackoverflow.com/a/15720769/2292993
                             pp = ggplot2::ggplot(df, aes(x=%s, y=%s, fill=%s)) +
                             stat_boxplot(geom = "errorbar", width = 0.5) +
                             %s geom_boxplot(outlier.shape=NA) + # avoid plotting outliers twice from geom_jitter
                             geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                             stat_summary(fun.y=mean, color="royalblue", geom="point", shape=18, size=3) +
                             facet_grid(%s~%s) +
                             theme(legend.position="none") +
                             ggtitle(paste0("N = ",nrow(df)))'
                             , xx, yy, xx, violin, zz, aa
                )
                if (show.n) {tt = paste0(tt, ' + \nstat_summary(fun.data = fun_length, color="grey", geom="text",vjust=1.2)')}
                tt = paste0(tt, ' + \nstat_summary(fun.y=mean, color="royalblue", geom="text",vjust=-0.7, aes(label=sprintf("%.2f (M)", ..y..)), alpha=1) # ..y.. internal variable computed mean')

            }        
        }
    }    
    cat(tt,"\n")
    eval(parse(text = tt))
    return(pp)
}

#' barplot with ggplot
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd like "y|x, y|x z, y|x z a" where y (axis) is continous, x (axis) z/a (legend) are discrete; during plot x z a ->x za(combined)
#' @param bar_color  "bw" or "color"  black/white or colorblind-friendly color
#' @param bar_gap  the gap between bars 
#' @param bar_width  the width of bar itself 
#' @param error_size  the thickness of error bar line 
#' @param error_gap  the location of errorbar, should be equal to bar_width(?) 
#' @param error_width the width of the bar of error
#' @param error_direction  "both", "max", "min"
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend_position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend_box  box of legend, T or F
#' @param legend_direction  horizontal or vertical
#' @param legend_size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @examples 
#' @export
ez.barplot = function(df,cmd,bar_color='color',bar_gap=0.7,bar_width=0.7,error_size=0.7,error_gap=0.7,error_width=0.3,error_direction='both',ylab=NULL,xlab=NULL,zlab=NULL,legend_position='top',legend_direction="horizontal",legend_box=T,legend_size=c(0,10),xangle=0,vjust=NULL,hjust=NULL) {
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    bar_color = ifelse(bar_color=='bw','scale_fill_grey(start=0,end=1)','scale_fill_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000"))')

    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend_size[1]==0) {legend_size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend_position = ifelse(is.character(legend_position), sprintf('theme(legend.position="%s")+',legend_position), sprintf('theme(legend.position=c(%s))+',paste(legend_position,collapse=',')))
    legend_box = ifelse(legend_box,'theme(legend.background = element_rect(color = "black"))+','')
    
    ymin = ifelse(error_direction %in% c('min','both'),'average-se','average')
    ymax = ifelse(error_direction %in% c('max','both'),'average+se','average')
    
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))

    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    # also see trimws()--remove trailing/leading spaces
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    cmd = strsplit(cmd,"|",fixed=TRUE)[[1]]
    if (length(cmd)==2) {
        # yy|xx or yy|xx zz
        yy = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        xx = strsplit(xx," ",fixed=TRUE)[[1]]
        # yy|xx
        if (length(xx)==1) {
            xx = xx[1]
            df=ez.dropna(df,c(yy,xx))
            # The width in geom_bar controls the bar width in relation to the x-axis 
            # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
            # color = outline color of bar
            # legend is ignored, but because lab might be empty, better to keep the legend commands here
            tt = sprintf('
                         pp=group_by(df,%s) %%>%% 
                         summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 
                         
                         ggplot2::ggplot(aes(x=%s,y=average,fill=%s)) +
                         geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                         %s +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                         
                         %s %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") + 
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, yy, yy, xx, xx, bar_width, bar_gap, bar_color, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, 'theme(legend.position="none")+', legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                         )
            # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                df=ez.dropna(df,c(yy,xx,zz))
                # The width in geom_bar controls the bar width in relation to the x-axis 
                # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
                # color = outline color of bar
                tt = sprintf('
                            pp=group_by(df,%s,%s) %%>%% 
                            summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 

                            ggplot2::ggplot(aes(x=%s,y=average,fill=%s)) +
                            geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                            %s + 
                            geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") + 
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, yy, yy, xx, zz, bar_width, bar_gap, bar_color, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                )
            }
            else {
                # yy|xx zz aa
                if (length(xx)==3) {
                    aa = xx[3]
                    zz = xx[2]
                    xx = xx[1]
                    df=ez.dropna(df,c(yy,xx,zz,aa))
                    # The width in geom_bar controls the bar width in relation to the x-axis 
                    # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
                    # color = outline color of bar
                    tt = sprintf('
                                pp=group_by(df,%s,%s,%s) %%>%% 
                                summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 
                                unite(zzaa,c(%s,%s)) %%>%% ez.2factor("zzaa") %%>%% ez.factorder("zzaa") %%>%%

                                ggplot2::ggplot(aes(x=%s,y=average,fill=zzaa)) +
                                geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                                %s + 
                                geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                                %s %s %s
                                %s %s
                                theme(axis.text.x=element_text(angle=%f %s %s)) +
                                theme(legend.direction="%s") + 
                                theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                                , xx, zz, aa, yy, yy, zz, aa, xx, bar_width, bar_gap, bar_color, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                    )
                }
            }        
        }
    }    
    cat(tt,"\n")
    eval(parse(text = tt))
    return(pp)
}

#' line plot with ggplot
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd like "y|x, y|x z, y|x z a" where y (axis) is continous, x (axis) z/a (legend) are discrete, during plot x z a ->x z(za combined)
#' @param line_size  the thickness of line, only applicable when there is z provided
#' @param error_size  the thickness of error bar line 
#' @param error_gap  the location of errorbar, should not be adjusted, 0 (parameter kept for reference)
#' @param error_width the width of the bar of error 
#' @param error_direction  'both', 'max', 'min'
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend_position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend_box  box of legend, T or F
#' @param legend_direction  horizontal or vertical
#' @param legend_size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot) , +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @examples 
#' @export
ez.lineplot = function(df,cmd,line_size=0.7,error_size=0.7,error_gap=0,error_width=0.3,error_direction='both',ylab=NULL,xlab=NULL,zlab=NULL,legend_position='top',legend_direction="horizontal",legend_box=T,legend_size=c(0,10),xangle=0,vjust=NULL,hjust=NULL) {
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))    
    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend_size[1]==0) {legend_size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend_position = ifelse(is.character(legend_position), sprintf('theme(legend.position="%s")+',legend_position), sprintf('theme(legend.position=c(%s))+',paste(legend_position,collapse=',')))
    legend_box = ifelse(legend_box,'theme(legend.background = element_rect(color = "black"))+','')

    ymin = ifelse(error_direction %in% c('min','both'),'average-se','average')
    ymax = ifelse(error_direction %in% c('max','both'),'average+se','average')
    
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))

    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    # also see trimws()--remove trailing/leading spaces
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    cmd = strsplit(cmd,"|",fixed=TRUE)[[1]]
    if (length(cmd)==2) {
        # yy|xx or yy|xx zz
        yy = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        xx = strsplit(xx," ",fixed=TRUE)[[1]]
        # yy|xx
        if (length(xx)==1) {
            xx = xx[1]
            df=ez.dropna(df,c(yy,xx))
            # The width in geom_line controls the bar width in relation to the x-axis 
            # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
            # color = outline color of bar
            # legend is ignored, but because lab might be empty, better to keep the legend commands here
            tt = sprintf('
                         pp=group_by(df,%s) %%>%% 
                         summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 
                         
                         ggplot2::ggplot(aes(x=%s,y=average,group=1)) +
                         geom_point() +
                         geom_line(size=%f) +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                         
                         %s %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") + 
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, yy, yy, xx, line_size, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, 'theme(legend.position="none")+', legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                         )
            # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                df=ez.dropna(df,c(yy,xx,zz))
                # The width in geom_line controls the bar width in relation to the x-axis 
                # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
                # color = outline color of bar
                tt = sprintf('
                            pp=group_by(df,%s,%s) %%>%% 
                            summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 

                            ggplot2::ggplot(aes(x=%s,y=average,group=%s)) +
                            geom_point(aes(shape=%s,color=%s)) +
                            geom_line(aes(linetype=%s,color=%s), size=%f) +
                            geom_errorbar(aes(ymin=%s, ymax=%s, linetype=%s, color=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                            scale_color_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000")) + 

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") + 
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, yy, yy, xx, zz, zz, zz, zz, zz, line_size, ymin, ymax, zz, zz, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                )
            } else {
                # yy|xx zz aa
                if (length(xx)==3) {
                    aa = xx[3]
                    zz = xx[2]
                    xx = xx[1]
                    df=ez.dropna(df,c(yy,xx,zz,aa))
                    tt = sprintf('
                            pp=group_by(df,%s,%s,%s) %%>%% 
                            summarise(average=mean(%s),se=sd(%s)/sqrt(n())) %%>%% 
                            unite(%s,c(%s,%s)) %%>%% ez.2factor("%s") %%>%% ez.factorder("%s") %%>%%

                            ggplot2::ggplot(aes(x=%s,y=average,group=%s)) +
                            geom_point(aes(shape=%s,color=%s)) +
                            geom_line(aes(linetype=%s,color=%s), size=%f) +
                            geom_errorbar(aes(ymin=%s, ymax=%s, linetype=%s, color=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                            scale_color_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000")) + 

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") + 
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, aa, yy, yy, zz, zz, aa, zz, zz, xx, zz, zz, zz, zz, zz, line_size, ymin, ymax, zz, zz, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                    )
                }
            }
        }
    }    
    cat(tt,"\n")
    eval(parse(text = tt))
    return(pp)
}

#' mimic xyplot with ggplot (slightly horizontally jittered)
#' @param df data frame in long format
#' @param cmd like "y|x,g", "y|x z,g", or "y|x z a,g" where y is continous, x z a are discrete, g is individual/grouping variable
#' \cr 'FinalMem|Attention, SubjectID'     'FinalMem|Attention Condition, SubjectID'
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @examples 
#' @export
ez.xyplot = function(df,cmd,ylab=NULL,xlab=NULL,xangle=0,vjust=NULL,hjust=NULL){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)

    cmd = strsplit(cmd,"[|,]")[[1]]
    yy = trimws(cmd[1])
    xx = trimws(cmd[2])
    gg = trimws(cmd[3])

    xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xx, perl=TRUE)
    xx = strsplit(xx," ",fixed=TRUE)[[1]]

    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))

    if (length(xx)==1) {
        # "y|x,g"
        xx = trimws(xx[1])
        df=ez.dropna(df,c(yy,xx,gg))
        # jitter solution (horizontal only) see: http://stackoverflow.com/questions/39533456/
        tt = sprintf("
                    pd = position_dodge(0.2)
                    pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                    geom_point(position=pd, size=1) + 
                    geom_line(position=pd, aes(color=%s)) + 
                    %s %s 
                    theme(axis.text.x=element_text(angle=%f %s %s)) +
                    theme(legend.position='none')"
             , xx,yy,gg,gg,xlab,ylab,xangle,vjust,hjust
        )
    } else {
        if (length(xx)==2) {
            # "y|x z,g"
            # assignment of zz should come first, because xx is going to be overwritten
            zz = trimws(xx[2])
            xx = trimws(xx[1])
            df=ez.dropna(df,c(yy,xx,gg,zz))
            tt = sprintf("
                        pd = position_dodge(0.2)
                        pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                        geom_point(position=pd, size=1) + 
                        geom_line(position=pd, aes(color=%s)) + 
                        %s %s 
                        theme(axis.text.x=element_text(angle=%f %s %s)) +
                        theme(legend.position='none') +
                        facet_grid(.~%s)"
                 , xx,yy,gg,gg,xlab,ylab,xangle,vjust,hjust,zz
            )
        } else {
            if (length(xx)==3) {
                # "y|x z a,g"
                # assignment of zz should come first, because xx is going to be overwritten
                zz = trimws(xx[2])
                aa = trimws(xx[3])
                xx = trimws(xx[1])
                df=ez.dropna(df,c(yy,xx,gg,zz,aa))
                tt = sprintf("
                            pd = position_dodge(0.2)
                            pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                            geom_point(position=pd, size=1) + 
                            geom_line(position=pd, aes(color=%s)) + 
                            %s %s 
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.position='none') +
                            facet_grid(%s~%s)"
                     , xx,yy,gg,gg,xlab,ylab,xangle,vjust,hjust,aa,zz
                )
            }
        }
    }

    cat(tt,"\n")
    eval(parse(text = tt))
    return(pp)
}

#' plot a heatmap with values shown
#' @param df data frame in wide format, like a correlation matrix
#' @param id a column name as id, will be shown as x axis, quoted ""
#' @param show.values whether to show values in addition to color in the plot
#' @param remove.zero remove the leading 0 as in correlation, 0.02->.02 (0.00 becomes "<.01")
#' \cr only works when show.value=T
#' @param angle the x axis label angle, default=270 (vertical), suggests 330 if label is not too long
#' @param colors low, middle, high
#' @param basesize base font size
#' @param xsize x axis label font relative size
#' @param ysize y axis label font relative size
#' @param legend.position "bottom", "top", "left", "right", "none"
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
ez.heatmap = function(df, id, show.values=F, remove.zero=T, angle=270, colors=c("blue", "white", "red"), basesize=9, xsize=1, ysize=1, legend.position="right"){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts)) 
    cmd = sprintf('tidyr::gather(df, key,value,-%s,factor_key = T) -> df
                  df$%s = factor(df$%s,rev(unique(as.character(df$%s))))
                  ',id,id,id,id)
    cat(cmd,"\n")
    eval(parse(text = cmd))

    x = "key"; y = id; z = "value"
    if (show.values) {
        t = sprintf('
                    p = ggplot(df, aes(%s, %s)) +
                    geom_tile(aes(fill = %s)) +
                    scale_fill_gradient2(low = "%s", mid = "%s", high = "%s") +
                    geom_text(aes(fill = %s, label = .remove0(%s,%s))) +
                    scale_x_discrete("", expand = c(0, 0)) +
                    scale_y_discrete("", expand = c(0, 0)) +
                    theme_grey(base_size=%f) +
                    theme(legend.position = "%s",
                    axis.ticks = element_blank(),
                    axis.text.x = element_text(angle = %d, size = rel(%f), hjust = 0),
                    axis.text.y = element_text(size = rel(%f)))'
                    , x, y, z, colors[1], colors[2], colors[3], z, z, remove.zero, basesize, legend.position, angle, xsize, ysize
        )
    } else {
        t = sprintf('
                    p = ggplot(df, aes(%s, %s)) +
                    geom_tile(aes(fill = %s)) +
                    scale_fill_gradient2(low = "%s", mid = "%s", high = "%s") +
                    scale_x_discrete("", expand = c(0, 0)) +
                    scale_y_discrete("", expand = c(0, 0)) +
                    theme_grey(base_size=%f) +
                    theme(legend.position = "%s",
                    axis.ticks = element_blank(),
                    axis.text.x = element_text(angle = %d, size = rel(%f), hjust = 0),
                    axis.text.y = element_text(size = rel(%f)))'
                    , x, y, z, colors[1], colors[2], colors[3], basesize, legend.position, angle, xsize, ysize
        )
    }
    eval(parse(text = t))
    cat(t,"\n")
    return(p)
}
# helper function to remove leading 0 in correlation
.remove0 <- function(value, remove.zero=T, prefix=""){  # format string more concisely
    if (remove.zero) {
        lst = c()
        for (item in value) {
            if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
                lst <- c(lst, '')
                next
            }
            item <- round(item, 2) # round to two digits
            if (item == 0) { # if rounding results in 0 clarify
                item = '<.01'
            }
            item <- as.character(item)
            item <- sub("(^[0])+", "", item)    # remove leading 0: 0.05 -> .05
            item <- sub("(^-[0])+", "-", item)  # remove leading -0: -0.05 -> -.05
            lst <- c(lst, paste(prefix, item, sep = ""))
        }
        return(lst)
    } else {
        return(value)
    }
}

#' plot a correlation matrix map
#' @description a wrapper of \code{\link[corrplot]{corrplot}}; the correlation and p values are calculated with \code{\link[Hmisc]{rcorr}}
#' @param df data frame in wide format, should be all numeric
#' @param corr.type "pearson" or "spearman", pairwise deletion for NA
#' @param sig.level sig.level
#' @param insig how to treat insig values, one of "pch"(show x),"p-value","blank", "n"(no change, as is)
#' @param ... see \code{\link[corrplot]{corrplot}} for more parameters
#' @return returns a list (r, p) r: a matrix representing the corrmap (p > sig.level, set to NA/blank), p: all raw p values
#' @examples
#' @export
ez.corrmap = function(df,corr.type="pearson",sig.level=0.05,insig="blank",
                     method ="color",tl.col = "black",tl.cex = 0.4,
                     col=NULL,...){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    corrmatrix = Hmisc::rcorr(as.matrix(df), type=corr.type)
    M = corrmatrix$r
    p.mat = corrmatrix$P

    if (is.null(col)){
        col1 <- colorRampPalette(rev(c("#7F0000", "red", "#FF7F00", "yellow", "grey", "cyan",
                                       "#007FFF", "blue", "#00007F")))
        col=col1(100)
    }

    corr = corrplot::corrplot(M, method = method, p.mat = p.mat, sig.level = sig.level,  insig = insig,
                       tl.col = tl.col, tl.cex = tl.cex, col = col, ...)

    ind = which(p.mat > sig.level, arr.ind = TRUE)
    corr[ind] = NA

    return(invisible(list(r=corr,p=p.mat)))
}


#' rescale a vector to 0-1
#' @return if x is not numeric, return original x (unchanged)
#' @examples
#' @export
ez.rescale01 = function (x) {
    if (is.numeric(x)) {
        # rng <- range(x, na.rm = TRUE)
        # result = (x - rng[1])/(rng[2] - rng[1])
        
        # get rid of negative values
        if (min(x, na.rm=TRUE)<0) {x = x - min(x, na.rm=TRUE)}
        # scale all postive to max of 1
        if (max(x, na.rm=TRUE)!=0) {x = x/max(x, na.rm=TRUE)}
        result = x
    } else {
        result = x
    }
    return(result)
}

#' define a new coordinate system
#' @return
#' @examples
#' @export
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x")
        "y"
    else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
            direction = sign(direction),
            is_linear = function(coord) TRUE)
}

#' draw a radar plot
#' @description variables/axes arranged in the shape of a radar map, scales are rescaled to 0-1
#' \cr best works with multivariates
#' @param df a data frame with a single variable as factor and all othere variables being numeric
#' @param id column name as id in quotes ""
#' @param stats perform stats (with NA removed) grouped by id, "mean", "median", "sd", "var", "none"; if "none", assume df already has summary stats
#' @param lwd line width
#' @param angle angle to show the variable names
#' @param fontsize fontsize of variable names, if 0, no variable names shown
#' @param facet if F show each id level separately; if T show all levels in one plot
#' @param facetfontsize fontsize of id level names (only valid when facet=T)
#' @param color color for different id levels, if NULL, remain the same for different id levels
#' @param linetype linetype for different id levels, if NULL, remain the same for different id levels
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @note As a reminder, the returned ggplot object can be modified post-hoc
#' @export
#' @references \href{http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html}{Erwan Le Pennec - CMAP}
ez.radarmap = function(df, id, stats="mean", lwd=1, angle=0, fontsize=0.8, facet=FALSE, facetfontsize=1, color=id, linetype=NULL){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    df.ori = df

    # 1) summarise
    if (stats!="none") {
        cmd = sprintf('df.stats = dplyr::summarise_each(dplyr::group_by(df.ori,%s),
                      funs(%s(.,na.rm=T)))
                      ',id,stats)
        cat(cmd,"\n")
        eval(parse(text = cmd))
    } else {
        df.stats = df.ori
    }

    # 2) rescale
    df.stats = as.data.frame(lapply(df.stats, ez.rescale01))

    # 3) to long format
    cmd = sprintf('tidyr::gather(df.stats, variable,value,-%s,factor_key = T) -> df
                  ',id)
    cat(cmd,"\n")
    eval(parse(text = cmd))

    # 4) plot
    # geom_polygon connects together, no fill
    # geom_line draw legend (the legend by polygon is hallow)
    if (!facet) {
        cmd = sprintf('
                      p=ggplot(df, aes(x = variable, y = value, group = %s, color=%s, linetype=%s)) +
                      geom_polygon(aes(), fill = NA, size = %f, show.legend = FALSE) +
                      geom_line(aes(), size = %f) +
                      theme(axis.text.x = element_text(size = rel(%f), angle = %d),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank()) +
                      xlab("") + ylab("") +
                      guides(color = guide_legend(ncol=2)) +
                      coord_radar()
                      ', id, id, id, lwd, lwd, fontsize, angle)

    } else {
        cmd = sprintf('
                      p=ggplot(df, aes(x = variable, y = value, group = %s, color = %s, linetype = %s)) +
                      geom_polygon(aes(), fill = NA, size = %f, show.legend = FALSE) +
                      facet_wrap(~ %s) +
                      theme(strip.text.x = element_text(size = rel(%f)),
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_text(size = rel(%f), angle = %d),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank()) +
                      xlab("") + ylab("") +
                      guides(color = "none") +
                      coord_radar()
                      ', id, id, id, lwd, id, facetfontsize, fontsize, angle)

    }
    cat(cmd,"\n")
    eval(parse(text = cmd))

    # 5) hack: couldn't pass NULL to color, linetype
    if (is.null(color)) {
        cmd = sprintf('p = p + scale_color_manual(values=rep("black",nlevels(factor(df$%s))))
                      ',id)
        cat(cmd,"\n")
        eval(parse(text = cmd))
    }
    if (is.null(linetype)) {
        cmd = sprintf('p = p + scale_linetype_manual(values=rep("solid",nlevels(factor(df$%s))))
                      ',id)
        cat(cmd,"\n")
        eval(parse(text = cmd))
    }

    return(p)
}

#' visualize where all the NAs of a dataframe are
#' @param df data frame in wide format
#' @param id a column name as id, will be shown as y axis, quoted "", eg, subject ID; if id not given, internally add row# as id
#' @param angle the x axis label angle, default=270 (vertical), suggests 330 if label is not too long
#' @param color color of missing values, eg, "red"
#' @param basesize base font size
#' @param xsize x axis label font relative size
#' @param ysize y axis label font relative size
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
ez.wherena = function(df,id=NULL,color="red",angle=270,basesize=9,xsize=1,ysize=1){
    
    ########################################################
    ## from package amelia
    ## missmap() - draws a missingness heatmap to show patterns. reorders the
    ##             variables to put the most missing on the left. reorders the
    ##             units to unit-period if ts/cs are present.
    ##
    ## INPUTS: obj - amelia output (class "amelia")
    ##         legend - draw a legend? (above the map)
    ##         col - [1] is the missing color, [2] is observed color
    ##         main - main title of the plot.
    ##
    ## OUPUTS: none
    ##

    missmap <- function(obj, legend = TRUE, col = c("wheat", "darkred"), main,
                        y.cex = 0.8, x.cex = 0.8, y.labels, y.at, csvar = NULL,
                        tsvar = NULL, rank.order = TRUE, ...) {

      if (class(obj) == "amelia") {
        vnames <- colnames(obj$imputations[[1]])
        n <- nrow(obj$missMatrix)
        p <- ncol(obj$missMatrix)
        percent.missing <- colMeans(obj$missMatrix)
        r1 <- obj$missMatrix
      } else {
        vnames <- colnames(obj)
        n <- nrow(obj)
        p <- ncol(obj)
        percent.missing <- colMeans(is.na(obj))
        r1 <- 1*is.na(obj)
      }


      if (!missing(y.labels) &&
          (missing(y.at) && (length(y.labels) != n))) {
        stop("y.at must accompany y.labels if there is less than onefor each row")
      }

      if (is.null(csvar)) csvar <- obj$arguments$cs
      if (is.null(tsvar)) tsvar <- obj$arguments$ts

      if (missing(y.labels)) {
        if (!is.null(csvar)) {
          if (class(obj) == "amelia") {
            cs <- obj$imputations[[1]][,csvar]
          } else {
            cs <- obj[,csvar]
          }
          y.labels <- cs
          if (is.factor(y.labels)) y.labels <- levels(y.labels)[unclass(y.labels)]

          cs.names <- y.labels


          if (!is.numeric(cs)) cs <- as.numeric(as.factor(cs))
          if (!is.null(tsvar)) {
            if (class(obj) == "amelia") {
              ts <- as.numeric(obj$imputations[[1]][,tsvar])
            } else {
              ts <- as.numeric(obj[,tsvar])
            }
            unit.period <- order(cs, ts)
          } else {
            unit.period <- 1:n
          }

          y.labels <- y.labels[unit.period]
          r1 <- r1[unit.period,]

          brks <- c(TRUE,rep(FALSE, times = (n-1)))
          for (i in 2:n) {
            brks[i] <- (cs[unit.period][i]!=cs[unit.period][i-1])
          }
          y.at <- which(brks)

          y.labels <- y.labels[brks]
        } else {
          y.labels <- row.names(obj$imputations[[1]])
          y.at <- seq(1, n, by=15)
          y.labels <- y.labels[y.at]
        }
      } else {
        if (missing(y.at))
          y.at <- n:1
      }
      missrank <- rev(order(percent.missing))
      if (rank.order) {
        chess <- t(!r1[n:1, missrank])
        vnames <- vnames[missrank]
      } else {
        chess <- t(!r1[n:1,])
      }
      y.at <- (n:1)[y.at]

      if (missing(main))
        main <- "Missingness Map"

      ## here we fork for data/tscs type plots. users cant set this yet.
      type <- "data"
      if (type == "data") {

        col.fix <- col
        if (sum(!chess) == 0) {
          col.fix <- col[2]
        }
        image(x = 1:(p), y = 1:n, z = chess, axes = FALSE,
              col = col.fix, xlab="", ylab="", main = main)

        axis(1, lwd = 0, labels = vnames, las = 2, at = 1:p, padj = .5,
             pos = 4, cex.axis = x.cex)
        axis(2, lwd = 0, labels = y.labels, las =2, at = y.at, pos =
             .7, hadj = 1, cex.axis = y.cex)


        if (legend) {
          par(xpd = TRUE)
          legend(x = p*1.07, y = n*1.07, col = col, bty = "n", xjust = 1,
                 legend = c("Missing", "Observed"), fill = col, horiz = TRUE)

        }
      } else {
        tscsdata <- data.frame(cs.names, ts, rowMeans(r1))
        tscsdata <- reshape(tscsdata, idvar = "cs.names", timevar = "ts",
                            direction = "wide")
        rownames(tscsdata) <- tscsdata[,1]
        colnames(tscsdata) <- unique(ts)
        tscsdata <- as.matrix(tscsdata[,-1])

        cols <- rev(heat.colors(5))

        image( z=t(tscsdata), axes
              = FALSE, col = cols, main = main, ylab="", xlab="")
        axis(1, labels = unique(ts), at = seq(from = 0, to = 1, length =
                                       ncol(tscsdata)), tck = 0, lwd = 0, las
             = 2)
        axis(2, labels = rownames(tscsdata), at = seq(from = 0, to = 1, length =
                                               nrow(tscsdata)), tck = 0, lwd =
             0, las = 1, cex.axis = .8)

        if (legend) {
          par(xpd = TRUE)
          legend(x = 0.95, y = 1.01, col = cols, bty = "n",
                 xjust = 1, legend = c("0-0.2",
                              "0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"), fill =cols, horiz = TRUE)
        }
      }

      invisible(NULL)

    }

    missmap(df, main='Missingness Map (most-->least)')
    # cat('\nIn "Missingness Map", the order of the variables along the the x-axis is sorted by the percent missing (from highest to lowest)\n')
    # ez.pause()
    cat('See another plot view in a moment...\n')
    Sys.sleep(3) # in seconds
    ########################################################


    # logic:
    # change all non-NAs to 0, all NAs to 1 then show on heatmap

    # https://stackoverflow.com/a/8317303/2292993
    # print at end as summary
    NAs = sapply(df, function(x) sum(is.na(x)))

    if (is.null(id)) {
        AutoRowID = data.frame(AutoRowID=1:nrow(df))
        df = dplyr::bind_cols(df,AutoRowID)
        id = 'AutoRowID'
    }

    # get information from df before changing df
    cmd = sprintf('theID = df$%s',id)
    eval(parse(text = cmd))

    indx.value = !is.na(df)
    indx.na = is.na(df)
    # you might get In apply(df, 2, as.numeric) : NAs introduced by coercion
    # but it does not matter, the purpose here is to change everything to numeric
    # so that 0 or 1 can be assigned for plotting
    df = suppressWarnings(as.data.frame(apply(df,2,as.numeric)))
    df[indx.value] = 0
    df[indx.na] = 1

    cmd = sprintf('df$%s = theID',id)
    eval(parse(text = cmd))

    cmd = sprintf('p = ez.heatmap(df, "%s", colors=c("blue", "white", "%s"),
                  legend.position="none", angle=%d, basesize=%f, xsize=%f, ysize=%f)'
                  , id, color, angle, basesize, xsize, ysize)
    # cat(cmd,"\n")
    eval(parse(text = cmd))

    cat('\nNumber of NAs in the data frame:')
    print(NAs)
    cat('=====================Done!=====================\n')
    return(p)
}

#' change factor level order in a df
#' @description does not change factor label; only changes the order of printing out
#' @param df data frame
#' @param col a factor column name, quoted "", eg, "group"
#' @param ord "az","za"--alphabetic;
#' \cr "as"--as is, appearance;
#' \cr c("small","medium","large")--specified level order
#' \cr "col2" --another column in az
#' \cr "col2:az" --another column in az
#' \cr "col2:za" --another column in za
#' @return returns a new df
#' @examples
#' @export
ez.factorder = function(df, col, ord="as"){
    # [[]] is the programmable form of $
    if (length(ord)==1) {
        if (ord=="as"){
            df[[col]] = factor(df[[col]], unique(as.character(df[[col]])))
        } else if (ord=="az") {
            df[[col]] = factor(df[[col]], levels(factor(df[[col]])))
        } else if (ord=="za") {
            df[[col]] = factor(df[[col]], rev(levels(factor(df[[col]]))))
        } else if (ord %in% names(df)) {
            df[[col]] = factor(df[[col]], unique(df[order(df[[ord]]),col]))
        } else if (grepl(":",ord,fixed=TRUE)) {
            # remove spaces
            ord = gsub(" ","",ord,fixed=TRUE)
            decreasing = strsplit(ord,":")[[1]][2]
            ord = strsplit(ord,":")[[1]][1]
            if (decreasing=="az") {
                decreasing=FALSE
            } else if (decreasing=="za") {
                decreasing=TRUE
            }
            df[[col]] = factor(df[[col]], unique(df[order(df[[ord]],decreasing=decreasing),col]))
        }
    } else {
        df[[col]]= factor(df[[col]],ord)
    }
    return(df)
}

#' change factor level names in a df
#' @param df data frame
#' @param col a factor column name, quoted "", eg, "group"
#' @param newLevelNames new level names coresponding to levels(x), eg, c("one","two","three")
#' @return returns a new df
#' @examples
#' @references \href{http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/}{Cookbook R: Renaming levels of a factor}
#' @export
ez.factorname = function(df, col, newLevelNames){
    cat('initial level names: ', levels(df[[col]]), '\n')
    levels(df[[col]]) = newLevelNames
    cat('renamed level names: ', newLevelNames, '\n')
    return(df)
}

#' reset factor levels in a df
#' @description reset factor levels in a df after its levels have been modified
#' relevel a factor in order to reflect its new levels
#' does not change factor label (set factor order as is)
#' @param df data frame
#' @param col a factor column name, quoted "", eg, "group"
#' @return returns a new df
#' @examples
#' @export
ez.relevelfactor = function(df, col){
    df[[col]] = factor(df[[col]], unique(as.character(df[[col]])))
    return(df)
}

#' scatter plot with ggplot
#' @param df data frame
#' @param cmd like "y~x", "y~x|z", "y~x||z" where y x are continous, z discrete (| one regression line, || multiple regression lines by levels of z)
#' @param point.alpha  if overplot for points, reduce alpha 
#' @param point.size if less point, increase size
#' @param rug.size rug size
#' @param rp.size  r p values font size, ignored if rp=FALSE
#' @param rp.x  r p values x position (relative to max of x value), ignored if rp=FALSE
#' @param rp.y  r p values y position (relative to min of y value), ignored if rp=FALSE
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/fill/legend label, only applicable when there is z provided NULL
#' @param legend_position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' @param legend_box  box of legend, T or F
#' @param legend_direction  horizontal or vertical
#' @param legend_size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param rp show r squared and p values 
#' @param se standard error of linear regression line
#' @param rug marginal rug indicating univariate distribution
#' @param ellipse draw confidence ellipses, powered by stat_ellipse()
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples 
#' @export
ez.scatterplot = function(df,cmd,rp.size=5,rp.x=0.95,rp.y=0.95,point.alpha=0.95,point.size=3,rug.size=0.5,ylab=NULL,xlab=NULL,zlab=NULL,legend_position='top',legend_direction="horizontal",legend_box=T,legend_size=c(0,10),rp=TRUE,se=TRUE,rug=TRUE,ellipse=FALSE){
    
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    # play a trick
    cmd = gsub("||","*",cmd,fixed=TRUE)

    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend_size[1]==0) {legend_size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend_position = ifelse(is.character(legend_position), sprintf('theme(legend.position="%s")+',legend_position), sprintf('theme(legend.position=c(%s))+',paste(legend_position,collapse=',')))
    legend_box = ifelse(legend_box,'theme(legend.background = element_rect(color = "black"))+','')

    tt = '
        # theme() cannot change geom_text(), geom_label(), annotate()
        # side note: geom_text() seems not to render text well on screen, geom_label() adds a box around text, annote() is based on geom_text but cannot include grouping variable
        if (Sys.info()["sysname"] != "Windows") {
            windowsFonts <- NULL
        }

        if (Sys.info()["sysname"] == "Windows") {
            windowsFonts(RMN=windowsFont("Times New Roman"))
            RMN <- "RMN"
        } else {
            RMN <- "Times New Roman"
        }
        ####################################################### subfunction
        # https://gist.github.com/kdauria/524eade46135f6348140
        # http://stackoverflow.com/a/7549819/2292993
        # http://stackoverflow.com/a/13451587/2292993
        lmrp = function(m) {
            rvalue = sign(coef(m)[2])*sqrt(summary(m)$r.squared)
            rvalue = ifelse(abs(rvalue)>=.005, sprintf("%.2f",rvalue), sprintf("%.2e", rvalue))
            pvalue = summary(m)$coefficients[2,4]
            if (pvalue<.001) {
                pvalue = sprintf("%.2e", pvalue)
            } else if (pvalue<.01) {
                pvalue = sprintf("%.3f", pvalue)
            } else {
                pvalue = sprintf("%.2f", pvalue)
            }

            eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue,list(rvalue = rvalue,pvalue = pvalue))
            as.character(as.expression(eq));                 
        }

        lmrp2 = function(y,x,z,df) {
            m = eval(parse(text=sprintf("lme4::lmList(%s ~ %s|%s, df)",y,x,z)))
            rvalue = sign(coef(m)[2])*sqrt(summary(m)$r.squared)
            rvalue = apply(rvalue,1,function(rval) ifelse(abs(rval)>=.005, sprintf("%.2f",rval), sprintf("%.2e", rval)))
            
            # separate p values
            # pvalue = summary(m)$coefficients[,4,2]
            # pvalue = sapply(pvalue,function(pval) {
            #                 if (pval<.001) {
            #                 sprintf("%.2e", pval)
            #                 } else if (pval<.01) {
            #                 sprintf("%.3f", pval)
            #                 } else {
            #                 sprintf("%.2f", pval)
            #                 }
            #                 })
            # eq <- substitute(italic(r[levs])~"="~rvalue*","~italic(p)~"="~pvalue,list(levs=paste0("(", paste(names(rvalue),collapse=", "), ")"),rvalue = paste0("(", paste(rvalue,collapse=", "), ")"),pvalue = paste0("(", paste(pvalue,collapse=", "), ")")))
            
            # interaction p value
            mm = eval(parse(text=sprintf("lm(%s ~ %s*%s, df)",y,x,z)))
            pvalue = summary(mm)$coefficients[4,4]
            if (pvalue<.001) {
                pvalue = sprintf("%.2e", pvalue)
            } else if (pvalue<.01) {
                pvalue = sprintf("%.3f", pvalue)
            } else {
                pvalue = sprintf("%.2f", pvalue)
            }

            eq <- substitute(italic(r[levs])~"="~rvalue*","~italic(p)~"="~pvalue,list(levs=paste0("(", paste(names(rvalue),collapse=", "), ")"),rvalue = paste0("(", paste(rvalue,collapse=", "), ")"),pvalue = pvalue))
            as.character(as.expression(eq));
        }
        ####################################################### subfunction /
        '
    cat(tt,"\n")    
    eval(parse(text = tt))

    if (grepl("|",cmd,fixed=TRUE)) {
      cmd = strsplit(cmd,"[~|]")[[1]]
      # y~x|z
      yy = trimws(cmd[1])
      xx = trimws(cmd[2])
      zz = trimws(cmd[3])
      df=ez.dropna(df,c(yy,xx,zz))

      rp.x = max(df[[xx]])*rp.x
      rp.y = min(df[[yy]])*rp.y
      # http://stackoverflow.com/a/27959418/2292993
      rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp(lm(%s ~ %s, df))), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx),'')
      se = ifelse(se,'TRUE','FALSE')
      rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f,aes(color=%s)) +',rug.size,zz),'')
      ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm",aes(color=%s)) +',zz),'')
      tt = sprintf('
                  pp=ggplot(df, aes(x=%s, y=%s)) +
                  geom_point(alpha=%f,size=%f,aes(color=%s,shape=%s)) + %s 
                  geom_smooth(method=lm,se=%s) + %s %s
                  scale_color_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000")) +
                  %s %s %s %s
                  theme(legend.direction="%s") + 
                  theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                  ,xx,yy,point.alpha,point.size,zz,zz,rp,se,rug,ellipse,ylab,xlab,zlab,legend_position,legend_direction,legend_size[1],legend_size[2],legend_size[2]
      )

    } else {
      cmd = strsplit(cmd,"[~*]")[[1]]
      if (length(cmd)==2) {
      # y~x
      yy = trimws(cmd[1])
      xx = trimws(cmd[2])
      df=ez.dropna(df,c(yy,xx))
      rp.x = max(df[[xx]])*rp.x
      rp.y = min(df[[yy]])*rp.y
      # http://stackoverflow.com/a/27959418/2292993
      rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp(lm(%s ~ %s, df))), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx),'')
      se = ifelse(se,'TRUE','FALSE')
      rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f) +',rug.size),'')
      ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm") +'),'')
      # legend is ignored, but because lab might be empty, better to keep the legend commands here
      tt = sprintf('
                  pp=ggplot(df, aes(x=%s, y=%s)) +
                  geom_point(alpha=%f,size=%f) + %s 
                  geom_smooth(method=lm,se=%s) + %s %s
                  scale_color_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000")) +
                  %s %s %s
                  theme(legend.direction="%s") + 
                  theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                  ,xx,yy,point.alpha,point.size,rp,se,rug,ellipse,ylab,xlab,legend_position,legend_direction,legend_size[1],legend_size[2],legend_size[2]
      )
      } else {
          # y~x||z
          yy = trimws(cmd[1])
          xx = trimws(cmd[2])
          zz = trimws(cmd[3])
          df=ez.dropna(df,c(yy,xx,zz))
          rp.x = max(df[[xx]])*rp.x
          rp.y = min(df[[yy]])*rp.y
          rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp2("%s","%s","%s",df)), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx,zz),'')
          se = ifelse(se,'TRUE','FALSE')
          rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f,aes(color=%s)) +',rug.size,zz),'')
          ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm",aes(color=%s)) +',zz),'')
          tt = sprintf('
                      pp=ggplot(df, aes(x=%s, y=%s)) +
                      geom_point(alpha=%f,size=%f,aes(color=%s,shape=%s)) + %s 
                      geom_smooth(method=lm,se=%s,aes(color=%s)) + %s %s
                      scale_color_manual(values=c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00","#cc79a7","#000000")) +
                      %s %s %s %s
                      theme(legend.direction="%s") + 
                      theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                      ,xx,yy,point.alpha,point.size,zz,zz,rp,se,zz,rug,ellipse,ylab,xlab,zlab,legend_position,legend_direction,legend_size[1],legend_size[2],legend_size[2]
          )
      }
    }
    cat(tt,"\n")
    eval(parse(text = tt))
    return(pp)
}
