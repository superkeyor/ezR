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
#' \crf only works when show.value=T
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
#' @param corr.type "pearson" or "spearman"
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
        col1 <- colorRampPalette(rev(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan",
                                       "#007FFF", "blue", "#00007F")))
        col=col1(100)
    }

    corrplot::corrplot(M, method = method, p.mat = p.mat, sig.level = sig.level,  insig = insig,
                       tl.col = tl.col, tl.cex = tl.cex, col = col, ...)
}
