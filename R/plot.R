###**************************************************.
###*plot.
###**************************************************.
#' alias of pdf
#' @param
#' @return
#' @examples
#' first z.export(filename) to create a new canvas
#' then plot all following graphs to it and
#' finally dev.off()/z.clean(3) to unlink

#' pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
#' width, height, onefile, family, title, fonts, version,
#' paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
#' useDingbats, useKerning, fillOddEven, compress)

#' i = 100
#' fileName = z.join('plot', i, '.pdf')
#' @export
z.export = pdf


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

#' plot(1:10, 1:10, main = "Plot 1")
#' # draw another arbitrary subplot
#' z.embed(plot(10,10, xlab="", ylab=""), x=2, y=8, size = c(0.5, 0.5) )

#' # demonstrates mfg usage, draw the third plot before the drawing the second
#' par(mfg=c(3,1) ); plot(11:20, 11:20, main = "Plot 3")
#' z.embed(plot(10,10, xlab="", ylab=""), x=12, y=18, size = c(0.5, 0.5))

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
