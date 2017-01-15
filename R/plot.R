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

#' change plot continous color to matlab like
#' @param n how many colors, e.g., 100 (default)
#' @export
#' @rdname matlabcolor
#' @examples p + matlabcolor(), p + matlabcolor2()
matlabcolor <- function(n=100){
    out = scale_fill_gradientn(colors=colorRamps::matlab.like(n))
    return(out)
}

#' change plot continous color to matlab like
#' @export
#' @rdname matlabcolor
#' @examples
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

#' wrapper of \code{\link{pdf}}
#' @param
#' @return
#' @note additionally one can use \code{\link[extrafont]{embed_fonts}} to embed fonts after dev.off()
#' @seealso \code{\link{ez.export}} \code{\link{ez.pdfoff}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11
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

#' show some help info on color
#' @examples
#' RColorBrewer::display.brewer.all()
#' # returns 8 colors from Set3 (which supports up to 12 colors)
#' cols <- RColorBrewer::brewer.pal(8,"Set3")
#' # generates 100 colors based on the 9 from the Blues palette
#' colorRampPalette(brewer.pal(9,"Blues"))(100)
#' scale_colour_manual(values = cols, breaks = c("4", "6", "8"), labels = c("four", "six", "eight"))
#' @export
ez.color = function(){
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

        # scale_colour_manual(values = cols, breaks = c("4", "6", "8"), labels = c("four", "six", "eight"))\n')
}

#' plot a customized boxplot with jittered stripplot, violin, and mean
#' @param df data frame in long format
#' @param cmd like "y", "y|x z a", "y|x z" or "y|x" where y is continous, x z a are discrete
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
ez.describe = function(df,cmd){
    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    # also see trimws()--remove trailing/leading spaces
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    cmd = strsplit(cmd,"|",fixed=TRUE)[[1]]
    # yy
    if (length(cmd)==1) {
        yy = cmd[1]
        xx = 'DummyDiscreteVariable'
        df['DummyDiscreteVariable'] = 1
        # hide x axis label in this case
        tt = sprintf('
                     pp = ggplot2::ggplot(df, aes(x=%s, y=%s)) +
                     geom_violin() +
                     geom_boxplot(outlier.shape=NA, alpha=0.7) + # avoid plotting outliers twice from geom_jitter
                     geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                     stat_summary(fun.y=mean, color="darkred", geom="point", shape=18, size=3) +
                     theme(legend.position="none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
                     xlab("")'
                     , xx, yy
        )
        tt = paste0(tt, ' + stat_summary(fun.y=mean, color="darkred", geom="text",vjust=-0.7, aes(label=sprintf("%.2f", ..y..)), alpha=1) # ..y.. internal variable computed mean')
    # yy|xx or yy|xx zz
    } else {
        yy = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        xx = strsplit(xx," ",fixed=TRUE)[[1]]
        # yy|xx
        if (length(xx)==1) {
            xx = xx[1]
            tt = sprintf('
                         pp = ggplot2::ggplot(df, aes(x=%s, y=%s)) +
                         geom_violin() +
                         geom_boxplot(outlier.shape=NA, alpha=0.7) + # avoid plotting outliers twice from geom_jitter
                         geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                         stat_summary(fun.y=mean, color="darkred", geom="point", shape=18, size=3) +
                         theme(legend.position="none")'
                         , xx, yy
            )
            tt = paste0(tt, ' + stat_summary(fun.y=mean, color="darkred", geom="text",vjust=-0.7, aes(label=sprintf("%.2f", ..y..)), alpha=1) # ..y.. internal variable computed mean')
        # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                tt = sprintf('
                             pp = ggplot2::ggplot(df, aes(x=%s, y=%s, color=%s)) +
                             geom_violin() +
                             geom_boxplot(outlier.shape=NA, alpha=0.7) + # avoid plotting outliers twice from geom_jitter
                             geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                             stat_summary(fun.y=mean, color="darkred", geom="point", shape=18, size=3) +
                             facet_grid(~%s) +
                             theme(legend.position="none")'
                             , xx, yy, zz, zz
                )
                tt = paste0(tt, ' + stat_summary(fun.y=mean, color="darkred", geom="text",vjust=-0.7, aes(label=sprintf("%.2f", ..y..)), alpha=1) # ..y.. internal variable computed mean')
            # yy|xx zz aa
            } else {
                aa = xx[3]
                zz = xx[2]
                xx = xx[1]
                tt = sprintf('
                             pp = ggplot2::ggplot(df, aes(x=%s, y=%s, color=%s)) +
                             geom_violin() +
                             geom_boxplot(outlier.shape=NA, alpha=0.7) + # avoid plotting outliers twice from geom_jitter
                             geom_point(position=position_jitter(width=0.2, height=0), size=1) +
                             stat_summary(fun.y=mean, color="darkred", geom="point", shape=18, size=3) +
                             facet_grid(%s~%s) +
                             theme(legend.position="none")'
                             , xx, yy, zz, zz, aa
                )
                tt = paste0(tt, ' + stat_summary(fun.y=mean, color="darkred", geom="text",vjust=-0.7, aes(label=sprintf("%.2f", ..y..)), alpha=1) # ..y.. internal variable computed mean')

            }        
        }
    }    
    eval(parse(text = tt))
    cat(tt,"\n")
    return(pp)
}

#' barplot with ggplot
#' @param df data frame in long format
#' @param cmd like "y|x, y|x z" where y (axis) is continous, x (axis) z (legend) are discrete
#' @para bar_gap  the gap between bars 
#' @para bar_width  the width of bar itself 
#' @para error_size  the thickness of error bar line 
#' @para error_gap  the location of errorbar, should be equal to bar_width(?) 
#' @para error_width the width of the bar of error
#' @para error_direction  'both', 'max', 'min'
#' @para ylab  y label NULL
#' @para xlab  x label NULL
#' @para zlab  z/fill label, only applicable when there is z provided NULL
#' @para legend_position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
#' @para legend_box  box of legend, T or F
#' @para legend_direction  horizontal or vertical
#' @para legend_size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @para xangle  angle of x text 0
#' @para vjust  vjust of x text NULL
#' @para hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples 
#' @export
ez.barplot = function(df,cmd,bar_gap=0.7,bar_width=0.7,error_size=0.7,error_gap=0.7,error_width=0.3,error_direction='both',ylab=NULL,xlab=NULL,zlab=NULL,legend_position='top',legend_direction="horizontal",legend_box=T,legend_size=c(0,10),xangle=0,vjust=NULL,hjust=NULL) {
    
    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend_position = ifelse(is.character(legend_position), sprintf('theme(legend.position="%s")+',legend_position), sprintf('theme(legend.position=c(%s))+',paste(legend_position,collapse=',')))
    legend_box = ifelse(legend_box,'theme(legend.background = element_rect(color = "black"))+','')
    
    ymin = ifelse(error_direction %in% c('min','both'),'mean-se','mean')
    ymax = ifelse(error_direction %in% c('max','both'),'mean+se','mean')
    
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
            # The width in geom_bar controls the bar width in relation to the x-axis 
            # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
            # color = outline color of bar
            
            tt = sprintf('
                         pp=group_by(df,%s) %%>%% 
                         summarise(mean=mean(value),se=sd(value)/sqrt(n())) %%>%% 
                         
                         ggplot2::ggplot(aes(x=%s,y=mean)) +
                         geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                         
                         %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") + 
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, xx, bar_width, bar_gap, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                         )
            # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                # The width in geom_bar controls the bar width in relation to the x-axis 
                # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
                # color = outline color of bar
                tt = sprintf('
                            pp=group_by(df,%s,%s) %%>%% 
                            summarise(mean=mean(value),se=sd(value)/sqrt(n())) %%>%% 

                            ggplot2::ggplot(aes(x=%s,y=mean,fill=%s)) +
                            geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                            geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                            scale_fill_grey(start=0,end=1) + 

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") + 
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, xx, zz, bar_width, bar_gap, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                )
            }        
        }
    }    
    eval(parse(text = tt))
    cat(tt,"\n")
    return(pp)
}

#' line plot with ggplot
#' @param df data frame in long format
#' @param cmd like "y|x, y|x z" where y (axis) is continous, x (axis) z (legend) are discrete
#' @para line_size  the thickness of line, only applicable when there is z provided
#' @para error_size  the thickness of error bar line 
#' @para error_gap  the location of errorbar, should not be adjusted, 0 (parameter kept for reference)
#' @para error_width the width of the bar of error 
#' @para error_direction  'both', 'max', 'min'
#' @para ylab  y label NULL
#' @para xlab  x label NULL
#' @para zlab  z/fill label, only applicable when there is z provided NULL
#' @para legend_position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
#' @para legend_box  box of legend, T or F
#' @para legend_direction  horizontal or vertical
#' @para legend_size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @para xangle  angle of x text 0
#' @para vjust  vjust of x text NULL
#' @para hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples 
#' @export
ez.lineplot = function(df,cmd,line_size=0.7,error_size=0.7,error_gap=0,error_width=0.3,error_direction='both',ylab=NULL,xlab=NULL,zlab=NULL,legend_position='top',legend_direction="horizontal",legend_box=T,legend_size=c(0,10),xangle=0,vjust=NULL,hjust=NULL) {
    
    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend_position = ifelse(is.character(legend_position), sprintf('theme(legend.position="%s")+',legend_position), sprintf('theme(legend.position=c(%s))+',paste(legend_position,collapse=',')))
    legend_box = ifelse(legend_box,'theme(legend.background = element_rect(color = "black"))+','')

    ymin = ifelse(error_direction %in% c('min','both'),'mean-se','mean')
    ymax = ifelse(error_direction %in% c('max','both'),'mean+se','mean')
    
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
            # The width in geom_line controls the bar width in relation to the x-axis 
            # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
            # color = outline color of bar
            
            tt = sprintf('
                         pp=group_by(df,%s) %%>%% 
                         summarise(mean=mean(value),se=sd(value)/sqrt(n())) %%>%% 
                         
                         ggplot2::ggplot(aes(x=%s,y=mean)) +
                         geom_point() +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                         
                         %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") + 
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, xx, ymin, ymax, error_size, error_width, error_gap, ylab, xlab, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                         )
            # yy|xx zz
        } else {
            if (length(xx)==2) {
                zz = xx[2]
                xx = xx[1]
                # The width in geom_line controls the bar width in relation to the x-axis 
                # while the width in position_dodge control the width of the space given to both bars also in relation to the x-axis.
                # color = outline color of bar
                tt = sprintf('
                            pp=group_by(df,%s,%s) %%>%% 
                            summarise(mean=mean(value),se=sd(value)/sqrt(n())) %%>%% 

                            ggplot2::ggplot(aes(x=%s,y=mean,group=%s)) +
                            geom_point(aes(shape=%s,color=%s)) +
                            geom_line(aes(linetype=%s,color=%s), size=%f) +
                            geom_errorbar(aes(ymin=%s, ymax=%s, linetype=%s, color=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") + 
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, xx, zz, zz, zz, zz, zz, line_size, ymin, ymax, zz, zz, error_size, error_width, error_gap, ylab, xlab, zlab, legend_position, legend_box, xangle, vjust, hjust, legend_direction, legend_size[1], legend_size[2], legend_size[2]
                )
            }        
        }
    }    
    eval(parse(text = tt))
    cat(tt,"\n")
    return(pp)
}

#' mimic xyplot with ggplot (slightly horizontally jittered)
#' @param df data frame in long format
#' @param cmd like "y|x,g", "y|x z,g", or "y|x z a,g" where y is continous, x z a are discrete, g is individual/grouping variable
#' \cr 'FinalMem|Attention, SubjectID'     'FinalMem|Attention Condition, SubjectID'
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples 
#' @export
ez.xyplot = function(df,cmd){
    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces 
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)

    cmd = strsplit(cmd,"[|,]")[[1]]
    yy = trimws(cmd[1])
    xx = trimws(cmd[2])
    gg = trimws(cmd[3])

    xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xx, perl=TRUE)
    xx = strsplit(xx," ",fixed=TRUE)[[1]]
    if (length(xx)==1) {
        # "y|x,g"
        xx = trimws(xx[1])
        # jitter solution (horizontal only) see: http://stackoverflow.com/questions/39533456/
        tt = sprintf("
                    pd = position_dodge(0.2)
                    pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                    geom_point(position=pd, size=1) + 
                    geom_line(position=pd, aes(color=%s)) + 
                    theme(legend.position='none')"
             , xx,yy,gg,gg
        )
    } else {
        if (length(xx)==2) {
            # "y|x z,g"
            # assignment of zz should come first, because xx is going to be overwritten
            zz = trimws(xx[2])
            xx = trimws(xx[1])
            tt = sprintf("
                        pd = position_dodge(0.2)
                        pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                        geom_point(position=pd, size=1) + 
                        geom_line(position=pd, aes(color=%s)) + 
                        theme(legend.position='none') +
                        facet_grid(.~%s)"
                 , xx,yy,gg,gg,zz
            )
        } else {
            if (length(xx)==3) {
                # "y|x z a,g"
                # assignment of zz should come first, because xx is going to be overwritten
                zz = trimws(xx[2])
                aa = trimws(xx[3])
                xx = trimws(xx[1])
                tt = sprintf("
                            pd = position_dodge(0.2)
                            pp = ggplot2::ggplot(df,aes(x=%s,y=%s,group=%s)) + 
                            geom_point(position=pd, size=1) + 
                            geom_line(position=pd, aes(color=%s)) + 
                            theme(legend.position='none') +
                            facet_grid(%s~%s)"
                     , xx,yy,gg,gg,aa,zz
                )
            }
        }
    }

    eval(parse(text = tt))
    cat(tt,"\n")
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
    cmd = sprintf('tidyr::gather(df, key,value,-%s,factor_key = T) -> df
                  df$%s = factor(df$%s,rev(unique(as.character(df$%s))))
                  ',id,id,id,id)
    eval(parse(text = cmd))
    cat(cmd,"\n")

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
#' @return returns a matrix representing the corrmap
#' @examples
#' @export
ez.corrmap = function(df,corr.type="pearson",sig.level=0.05,insig="blank",
                     method ="color",tl.col = "black",tl.cex = 0.4,
                     col=NULL,...){

    corrmatrix = Hmisc::rcorr(as.matrix(df), type=corr.type)
    M = corrmatrix$r
    p.mat = corrmatrix$P

    if (is.null(col)){
        col1 <- colorRampPalette(rev(c("#7F0000", "red", "#FF7F00", "yellow", "grey", "cyan",
                                       "#007FFF", "blue", "#00007F")))
        col=col1(100)
    }

    corrplot::corrplot(M, method = method, p.mat = p.mat, sig.level = sig.level,  insig = insig,
                       tl.col = tl.col, tl.cex = tl.cex, col = col, ...)
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

    df.ori = df

    # 1) summarise
    if (stats!="none") {
        cmd = sprintf('df.stats = dplyr::summarise_each(dplyr::group_by(df.ori,%s),
                      funs(%s(.,na.rm=T)))
                      ',id,stats)
        eval(parse(text = cmd))
        cat(cmd,"\n")
    } else {
        df.stats = df.ori
    }

    # 2) rescale
    df.stats = as.data.frame(lapply(df.stats, ez.rescale01))

    # 3) to long format
    cmd = sprintf('tidyr::gather(df.stats, variable,value,-%s,factor_key = T) -> df
                  ',id)
    eval(parse(text = cmd))
    cat(cmd,"\n")

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
    eval(parse(text = cmd))
    cat(cmd,"\n")

    # 5) hack: couldn't pass NULL to color, linetype
    if (is.null(color)) {
        cmd = sprintf('p = p + scale_color_manual(values=rep("black",nlevels(factor(df$%s))))
                      ',id)
        eval(parse(text = cmd))
        cat(cmd,"\n")
    }
    if (is.null(linetype)) {
        cmd = sprintf('p = p + scale_linetype_manual(values=rep("solid",nlevels(factor(df$%s))))
                      ',id)
        eval(parse(text = cmd))
        cat(cmd,"\n")
    }

    return(p)
}

#' visualize where all the NAs of a dataframe are
#' @param df data frame in wide format
#' @param id a column name as id, will be shown as y axis, quoted "", eg, subject ID
#' @param angle the x axis label angle, default=270 (vertical), suggests 330 if label is not too long
#' @param color color of missing values, eg, "red"
#' @param basesize base font size
#' @param xsize x axis label font relative size
#' @param ysize y axis label font relative size
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
ez.wherena = function(df,id,color="red",angle=270,basesize=9,xsize=1,ysize=1){
    # logic:
    # change all non-NAs to 0, all NAs to 1 then show on heatmap

    # get information from df before changing df
    cmd = sprintf('theID = df$%s',id)
    eval(parse(text = cmd))
    cat(cmd,"\n")

    indx.value = !is.na(df)
    indx.na = is.na(df)
    df = as.data.frame(apply(df,2,as.numeric))
    df[indx.value] = 0
    df[indx.na] = 1

    cmd = sprintf('df$%s = theID',id)
    eval(parse(text = cmd))
    cat(cmd,"\n")

    cmd = sprintf('p = ez.heatmap(df, "%s", colors=c("blue", "white", "%s"),
                  legend.position="none", angle=%d, basesize=%f, xsize=%f, ysize=%f)'
                  , id, color, angle, basesize, xsize, ysize)
    eval(parse(text = cmd))
    cat(cmd,"\n")

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

#' reset factor levels in a df after its levels have been modified, relevel a factor in order to reflect its new levels
#' @description does not change factor label (set factor order as is)
#' @param df data frame
#' @param col a factor column name, quoted "", eg, "group"
#' @return returns a new df
#' @examples
#' @export
ez.relevelfactor = function(df, col){
    df[[col]] = factor(df[[col]], unique(as.character(df[[col]])))
    return(df)
}