###**************************************************.
###*plot.
###**************************************************.
#' wrapper of \code{\link{dev.copy2pdf}}
#' @param
#' @return
#' @seealso \code{\link{pdf}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11
#' @export
z.export = function(filename = "RPlot.pdf", pdf.width = 8.5, pdf.height = 11, ...) {
    dev.copy2pdf(file=filename, width = pdf.width, height = pdf.height, ...)
    cat('Image exported.\n')
}

#' subplot, wrapper of \code{\link{par}}
#' @param
#' @return
#' @examples
#' subplot(n,m,...) divides the current figure into an n-by-m grid
#' A vector of the form c(n, m). Subsequent figures will be drawn in an n-by-m array on the device
#' by columns (mfcol), or rows (mfrow), respectively.
#' see more ?par
#' @export
z.subplot = function(n, m, ...){
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
#' z.embed(plot(10,10, xlab="", ylab=""), x=2, y=8, size = c(0.5, 0.5) )
#'
#' # demonstrates mfg usage, draw the third plot before the drawing the second
#' par(mfg=c(3,1) ); plot(11:20, 11:20, main = "Plot 3")
#' z.embed(plot(10,10, xlab="", ylab=""), x=12, y=18, size = c(0.5, 0.5))
#'
#' par(mfg=c(2,1)); plot(21:30, 21:30, main = "Plot 2")
#' z.embed(plot(10,10, xlab="", ylab=""), x=22, y=28, size = c(0.5, 0.5))
#' @export
z.embed = function(fun, x, y=NULL, size=c(1,1), vadj=0.5, hadj=0.5,
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
#' cols <- RColorBrewer::brewer.pal(8,"Set3")
#' colorRampPalette(brewer.pal(9,"Blues"))(100)
#' @export
z.color = function(){
    if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
        library(RColorBrewer)
    }
    RColorBrewer::display.brewer.all()
    cat('usage: \n
    RColorBrewer::display.brewer.all()\n
    cols <- RColorBrewer::brewer.pal(8,"Set3")\n
    colorRampPalette(brewer.pal(9,"Blues"))(100)\n')
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
#' A ggplot2 theme with no background and Times New Roman font.
#'
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @examples
#' @author \href{https://github.com/trinker/plotflow}{trinker/plotflow}
#' \dontrun{
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_apa() +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total")
#'
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_apa(plot.box=T) +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total")
#'
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_basic() +
#'     theme_apa() +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total")
#' }
theme_apa <- function(plot.box = FALSE){

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
        plot.title=element_text(family=RMN, size=14, face="bold", colour="black"),
        axis.title.x=element_text(family=RMN, size=14, colour="black"),
        axis.title.y=element_text(family=RMN, size=14, angle=90, colour="black"),
        axis.text.x=element_text(family=RMN, size=11, colour="black"),
        axis.text.y=element_text(family=RMN, size=11, colour="black"),
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

#' plot a customized boxplot with jittered stripplot, violin, and mean
#' @param df data frame in long format
#' @param cmd like "y|x z" or "y|x" where y is continous, x z are discrete
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
z.describe = function(df,cmd){
    # yy|xx zz
    cmd = strsplit(cmd,"|",fixed=TRUE)[[1]]
    yy = cmd[1]
    xx = strsplit(cmd[2]," ",fixed=TRUE)[[1]]
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
    } else {
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
    }
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
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @examples
#' @export
z.heatmap = function(df, id, show.values=F, remove.zero=T, angle=270, colors=c("blue", "white", "red")){
    cmd = sprintf('tidyr::gather(df, key,value,-%s,factor_key = T) -> df
                  df$%s = factor(df$%s,rev(unique(as.character(df$%s))))
                  ',id,id,id,id)
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
                    theme_grey(base_size = 9) +
                    theme(legend.position = "right",
                    axis.ticks = element_blank(),
                    axis.text.x = element_text(angle = %d, hjust = 0))'
                    , x, y, z, colors[1], colors[2], colors[3], z, z, remove.zero, angle
)
    } else {
        t = sprintf('
                    p = ggplot(df, aes(%s, %s)) +
                    geom_tile(aes(fill = %s)) +
                    scale_fill_gradient2(low = "%s", mid = "%s", high = "%s") +
                    scale_x_discrete("", expand = c(0, 0)) +
                    scale_y_discrete("", expand = c(0, 0)) +
                    theme_grey(base_size = 9) +
                    theme(legend.position = "right",
                    axis.ticks = element_blank(),
                    axis.text.x = element_text(angle = %d, hjust = 0))'
                    , x, y, z, colors[1], colors[2], colors[3], angle
        )
    }
    eval(parse(text = t))
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
#' @return
#' @examples
#' @export
z.corrmap = function(df,corr.type="pearson",sig.level=0.05,insig="blank",
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
z.rescale01 = function (x) {
    if (is.numeric(x)) {
        rng <- range(x, na.rm = TRUE)
        result = (x - rng[1])/(rng[2] - rng[1])
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
z.radarmap = function(df, id, stats="mean", lwd=1, angle=0, fontsize=0.8, facet=FALSE, facetfontsize=1, color=id, linetype=NULL){

    df.ori = df

    # 1) summarise
    if (stats!="none") {
        cmd = sprintf('df.stats = dplyr::summarise_each(dplyr::group_by(df.ori,%s),
                      funs(%s(.,na.rm=T)))
                      ',id,stats)
        eval(parse(text = cmd))
    } else {
        df.stats = df.ori
    }

    # 2) rescale
    df.stats = as.data.frame(lapply(df.stats, z.rescale01))

    # 3) to long format
    cmd = sprintf('tidyr::gather(df.stats, variable,value,-%s,factor_key = T) -> df
                  ',id)
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
    eval(parse(text = cmd))

    # 5) hack: couldn't pass NULL to color, linetype
    if (is.null(color)) {
        cmd = sprintf('p = p + scale_color_manual(values=rep("black",nlevels(factor(df$%s))))
                      ',id)
        eval(parse(text = cmd))
    }
    if (is.null(linetype)) {
        cmd = sprintf('p = p + scale_linetype_manual(values=rep("solid",nlevels(factor(df$%s))))
                      ',id)
        eval(parse(text = cmd))
    }

    return(p)
}

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