###**************************************************.
###*plot.
###**************************************************.

###**************************************************.
###*without ez. in the function name.
###**************************************************.
#' creat shorthand for Times New Roman cross-platform
#' @description creat shorthand for Times New Roman cross-platform
#' @export
# from https://github.com/trinker/plotflow/
RMN = if (Sys.info()["sysname"]=="Windows") {windowsFonts(RMN=windowsFont("Times New Roman")); "RMN"} else "Times New Roman"

#' show shape
#' @description show shape
#' @export
show.shape = function(){
    # http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
    print(ez.sprintf('
    Use the pch= option to specify symbols to use when plotting points.
    Specify border color (col=) and fill color (bg=, for symbols 21 through 25).
    ggplot2 default shapes (only 6): c(16,17,15,3,7,8)
    '))
    opar<-par(font=2, mar=c(0.5,0,0,0))
    on.exit(par(opar))
    y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
    x=c(rep(1:5,5),6)
    plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5),
       axes=FALSE, xlab="", ylab="", col='red', bg="green")
    text(x, y, labels=0:25, pos=3)
    par(mar=opar$mar,font=opar$font)
}

#' show line
#' @description show line
#' @export
show.line = function(){
    print(ez.sprintf('
    Use the lty= option to specify line types, lwd= width.
    '))
    # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
    opar = par(mar=c(0,0,0,0))
    on.exit(par(opar))
    # Set up the plotting area
    plot(NA, xlim=c(0,1), ylim=c(6.5, -0.5),
        xaxt="n", yaxt="n",
        xlab=NA, ylab=NA )
    # Draw the lines
    for (i in 0:6) {
        points(c(0.25,1), c(i,i), lty=i, lwd=2, type="l")
    }
    # Add labels
    text(0, 0, "0. 'blank'"   ,  adj=c(0,.5))
    text(0, 1, "1. 'solid'"   ,  adj=c(0,.5))
    text(0, 2, "2. 'dashed'"  ,  adj=c(0,.5))
    text(0, 3, "3. 'dotted'"  ,  adj=c(0,.5))
    text(0, 4, "4. 'dotdash'" ,  adj=c(0,.5))
    text(0, 5, "5. 'longdash'",  adj=c(0,.5))
    text(0, 6, "6. 'twodash'" ,  adj=c(0,.5))
}

#' show color
#' @description show color
#' @param v color vector to show, if not provided, show all colors in ez.palette
#' @param name display name for the color v (optional)
#' @param label T/F display actual color code for each v
#' @export
#' @examples
#' show.color(ez.palette('Zhu'),'Current')
show.color = function(v,name,label=T){
    if (!missing(v)) {
        # codes modified from https://github.com/karthik/wesanderson/blob/master/R/colors.R
        n <- length(v)
        opar <- par(mar = c(0.5, 0.5, 0.5, 0.5))
        on.exit(par(opar))
        image(1:n, 1, as.matrix(1:n), col = v,
        xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
        if (label) text(1:n, 0.955, labels = v, cex = 1, family = "serif", col="dark grey")
        if (!missing(name)) text((n + 1) / 2, 1, labels = name, cex = 1, family = "serif")
        return(invisible(NULL))
    }

    print(ez.sprintf('
    Tips on color:
        # list all color names                      | # to see the color in a gg plot
            colors()                                |     ggplot_build(p)$data
        # rgb 2 hex                                 | # hex 2 rgb
            rgb(r,g,b,maxColorValue=255)            |     col2rgb("red"); col2rgb("#000d00")
        # Generates/Interpolates n colors           | # rgb to grey
            colorRampPalette(c("black","white"))(36)|     col2grey(col)

        # scale_color_manual (for points, lines, and outlines)
        # scale_fill_manual (for boxes, bars, and ribbons)
        scale_color_manual(values=cols,breaks=c("4","6","8"),labels=c("four","six","eight"))

        # RColorBrewer or visit http://colorbrewer2.org/
        RColorBrewer::display.brewer.all()
        # Y1OrRd...Blues (Sequential palettes): interval data
        # Set3...Accent (Qualitative palettes): nominal or categorical data
        # Spectral...BrBG (Diverging palettes):
        # Returns 8 colors from Set3 (which supports up to 12 colors)
        cols <- RColorBrewer::brewer.pal(8,"Set3")

        # Finally, ez.palette for my own color collections!
        # note the "n" scale_fill_gradientn
        p + scale_fill_gradientn(colors=ez.palette("matlabcolor2",nn=100))
    '))

    print(ez.palette())
    return(invisible(NULL))
}

#' palette generator
#' @description palette generator
#' @param name Name of desired palette. To see all colors, call ez.palette() with no parameter.
#' @param n Number of colors desired. Unfortunately most palettes now only
#'   have 4 or 5 colors. But hopefully we'll add more palettes soon. All color
#'   schemes are derived from the most excellent Tumblr blog:
#'   \href{http://wesandersonpalettes.tumblr.com/}{Wes Anderson Palettes}.
#'   If omitted, uses all colors
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colors
#' @param nn For a few palettes that natively support color interpolatation. See example below.
#' @return A vector of colors. To see the colors, call show.color()
#' @export
#' @examples
#' ez.palette("Royal1")
#' ez.palette("GrandBudapest1")
#' ez.palette("Cavalcanti1")
#' ez.palette("Cavalcanti1", 3)
#' show.color(ez.palette("Cavalcanti1", 3))
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- ez.palette(name = "Zissou1", 21, type = "continuous")
#' image(volcano, col = pal)
#'
#' ez.palette('matlabcolor',nn=100)  # do not specify n and type
#' ez.palette('matlabcolor2',nn=100)
#' ez.palette('ggcolor',nn=10)
#' rep(ez.palette('ggcolor',nn=10),2)
#'
#' # in action
#' qplot(Sepal.Length,
#'       Petal.Length,
#'       data = iris,
#'       color = Species,
#'       size = Petal.Width)+
#' theme_bw()+
#' scale_color_manual(values = ez.palette("GrandBudapest1",3))
ez.palette = function(name, n, type = c("discrete", "continuous"), nn=10){
    # codes modified from https://github.com/karthik/wesanderson/blob/master/R/colors.R
    palettes <- list(
        BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
        BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
        Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
        Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
        Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
        Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
        Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
        Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
        Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
        FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
        Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
        Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
        Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
        Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
        GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
        GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"),
        IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680"),
        IsleofDogs2 = c("#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718"),
        matlab1 = colorRamps::matlab.like(nn),
        matlab2 = colorRamps::matlab.like2(nn),
        # default ggplot color
        ggplot2 = hcl(h=seq(15,375,length=nn+1),l=65,c=100)[1:nn],
        # from RColorBrewer
        # f <- function(pal) RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pal, "maxcolors"], pal)
        # f("Set1")
        Set3 = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#D9D9D9', '#BC80BD', '#CCEBC5', '#FFED6F'),
        Set2 = c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3'),
        Set1 = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#999999'),
        Pastel2 = c('#B3E2CD', '#FDCDAC', '#CBD5E8', '#F4CAE4', '#E6F5C9', '#FFF2AE', '#F1E2CC', '#CCCCCC'),
        Pastel1 = c('#FBB4AE', '#B3CDE3', '#CCEBC5', '#DECBE4', '#FED9A6', '#FFFFCC', '#E5D8BD', '#FDDAEC', '#F2F2F2'),
        Paired = c('#A6CEE3', '#1F78B4', '#B2DF8A', '#33A02C', '#FB9A99', '#E31A1C', '#FDBF6F', '#FF7F00', '#CAB2D6', '#6A3D9A', '#FFFF99', '#B15928'),
        Dark2 = c('#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666'),
        Accent = c('#7FC97F', '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F', '#BF5B17', '#666666'),
        Set1Remix = c("#984EA3","#377EB8","#4DAF4A","#FF7F00","#E41A1C","#FFFF33","#A65628","#F781BF"),
        # Jerry remixed from Set 3
        Zhu = c("#B3DE69","#80B1D3","#BC80BD","#FB8072","#FDB462"),
        # well, I tested with col2grey(), not so true. Not sure about actual bw printer
        Printer = c("#FDAE61","#2B83BA","#D7191C","#ABDDA4","#FFFFBF"),
        Colorblind = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7","#000000")
    )
    if (missing(name)){
        palnames = names(palettes); palcolors = palettes
        # codes borrowed from ColorBrewer
        # nr row, nc col
        nr = length(palcolors); n = sapply(palcolors,length); nc = max(n)
        opar <- par(mgp=c(2,0.25,0))
        on.exit(par(opar))
        plot(1, 1, xlim=c(0,(nc+1)), ylim=c(0,nr), type="n", axes=FALSE, bty="n", xlab="",ylab="")
        for(i in 1:nr){
        nj <- n[i]
        color = palcolors[[i]]
        rect(xleft=1:nj, ybottom=i-1, xright=2:(nj+1), ytop=i-0.2, col=color, border="light grey")
        }
        # (x,nr); (1:nr)-y, where y between ybottom and ytop
        text(rep(0.8,nr),(1:nr)-0.6, labels=palnames, xpd=TRUE, adj=1)
        return(palnames)
    }

    # use first one
    type <- match.arg(type)

    pal <- palettes[[name]]
    if (is.null(pal))
    stop("Palette not found.")

    if (missing(n)) {
    n <- length(pal)
    }

    if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
    }

    out <- switch(type,
    continuous = grDevices::colorRampPalette(pal)(n),
    discrete = pal[1:n]
    )
    out
    # structure(out, class = "palette", name = name)
}

#' Convert colors to grey/grayscale so that you can see how your plot will look after photocopying or printing to a non-color printer.
#' @description Convert colors to grey/grayscale so that you can see how your plot will look after photocopying or printing to a non-color printer.
#' @param col vector of any of the three kind of R colors, i.e., either a color name (an element of colors()), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or an integer i meaning palette()[i]. Non-string values are coerced to integer.
#' @return A vector of colors (greys) corresponding to the input colors.
#' @note Converts colors to greyscale using the formula grey = 0.3*red + 0.59*green + 0.11*blue. This allows you to see how your color plot will approximately look when printed on a non-color printer or photocopied.
#' @examples
#' par(mfcol=c(2,2))
#' tmp <- 1:3
#' names(tmp) <- c('red','green','blue')
#'
#' barplot(tmp, col=c('red','green','blue'))
#' barplot(tmp, col=ColToGrey(c('red','green','blue')))
#'
#' barplot(tmp, col=c('red','#008100','#3636ff'))
#' barplot(tmp, col=ColToGrey(c('red','#008100','#3636ff')))
#' @export
col2grey <- function(col){
    # https://github.com/cran/DescTools/blob/master/R/DescTools.r
    rgb <- col2rgb(col)
    g <- rbind( c(0.3, 0.59, 0.11) ) %*% rgb
    rgb(g, g, g, maxColorValue=255)
}

#' @rdname col2grey
#' @export
col2gray = col2grey

#' print out a ggplot object's history, cat(pp$gghistory)
#' @description print out a ggplot object's history, cat(pp$gghistory)
#' @export
gghistory=function(pp){
  cat(pp$gghistory)
  return(invisible(pp$gghistory))
}

#' pass through a ggplot object, see examples
#' @description pass through a ggplot object, see examples
#' @note Assuming pp has $df (preferred to $data), or $data. Make sure $df/$data is what you intend to plot
#' @examples df %>% ggplot1() %>% ggpass() %>% ggplot2()
#' #however, if "+" is used in ggplot, use the following format
#' df %>% {ggplot1(.,)+xlab('')} %>% ggpass() %>% {ggplot2(.,)+ylab('')}
#' @export
ggpass=function(pp){
    if ('ggplot' %in% class(pp)) {
        df=if (is.null(pp[['df']])) pp[['data']] else pp[['df']]
        print(pp)
        return(df)
    }
}

#' Open Help Pages for ggplot2
#' @description Open Help Pages for ggplot2
#'
#' \code{gghelp} - Open Hadely Wickham's ggplot2
#' \href{https://ggplot2.tidyverse.org/reference/}{web page}.
#'
#' @param ggfun string, A particular ggplot function to reference.  Default is the index
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
gghelp <- function(ggfun) {
    if(missing(ggfun)) ggfun <- "" else ggfun <- paste0(ggfun, ".html")
    browseURL(sprintf("https://ggplot2.tidyverse.org/reference/%s", ggfun))
}

#' Open Help Pages for ggplot2
#' @description Open Help Pages for ggplot2
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

#' Multiple plot function, accepts a list of ggplot (not plot) objects
#' @description Multiple plot function, accepts a list of ggplot (not plot) objects
#' @param plotlist objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' (p1,p2,p3), (plotlist=list(p1,p2,p3)), or (p1,plotlist=list(p2,p3))
#' @param cols   Number of columns in layout.. If present, 'cols' is ignored. If both cols and layout NULL, auto calculate
#' @param layout A matrix specifying the layout.
#' @param title string, NULL or '' both fine
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
#' ggmultiplot(p1,p2,p3, cols = 3)
#'
#' plots <- list()
#' for (i in 1:5) {
#'     p1 = qplot(1:10, rnorm(10), main = i)
#'     plots[[i]] <- p1
#' }
#' layout <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
#' ggmultiplot(plotlist = plots, layout = layout)
#' layout <- matrix(c(1, NA, 2,
#'                    3, 4, 5), nrow = 2, byrow = TRUE)  # NA for placeholder
#' ggmultiplot(plotlist = plots, layout = layout)
#'
#' @references \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook R}
ggmultiplot <- function(..., plotlist=NULL, cols=NULL, layout=NULL, title='', title.size=20) {
    # Make a list from the ... arguments and plotlist
    # remove NULL objects from ..., see https://stackoverflow.com/a/48519190/2292993
    plots <- c(Filter(Negate(is.null), list(...)), plotlist)

    if (!ggplot2::is.ggplot(plots[[1]])) {return(multiplot(...,plotlist=plotlist,cols=cols,layout=layout,title=title,title.size=title.size))}

    numPlots = length(plots)
    if (is.null(cols)) {cols=floor(sqrt(length(plots)))}

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
        # alternative solution with grid
        # # Set up the page
        # grid::grid.newpage()
        # grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        # # Make each plot, in the correct location
        # for (i in 1:numPlots) {
        #     # Get the i,j matrix positions of the regions that contain this subplot
        #     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        #     print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
        #                                           layout.pos.col = matchidx$col))
        # }

        # wow, gridExtra works for ggplot objects, just with one function call!
        gridExtra::grid.arrange(grobs=plots,layout_matrix=layout,
            top=grid::textGrob(title,gp=grid::gpar(fontsize=title.size,fontface=2,fontfamily=RMN)))
    }
}

#' Multiple plot function, accepts a list of lattice plot (not ggplot) objects
#' @description Multiple plot function, accepts a list of lattice plot (not ggplot) objects
#' @param plotlist objects can be passed in ..., or to plotlist (as a list of plot objects)
#' (p1,p2,p3), (plotlist=list(p1,p2,p3)), or (p1,plotlist=list(p2,p3))
#' @param cols:   Number of columns in layout. If present, 'cols' is ignored. If both cols and layout NULL, auto calculate
#' @param layout: A matrix specifying the layout.
#' \cr If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' \cr then plot 1 will go in the upper left, 2 will go in the upper right, and
#' \cr 3 will go all the way across the bottom.
#' @param title string, NULL or '' both fine
#' @return returns nothing (NULL)
#' @export
#' @examples
#' # examples for ggmultiplot, as reference here
#' plots <- list()  # new empty list
#' for (i in 1:6) {
#'     p1 = qplot(1:10, rnorm(10), main = i)
#'     plots[[i]] <- p1  # add each plot into plot list
#' }
#' ggmultiplot(plotlist = plots, cols = 3)
#'
#' ggmultiplot(p1,p2,p3, cols = 3)
#'
#' plots <- list()
#' for (i in 1:5) {
#'     p1 = qplot(1:10, rnorm(10), main = i)
#'     plots[[i]] <- p1
#' }
#' layout <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
#' ggmultiplot(plotlist = plots, layout = layout)
#' layout <- matrix(c(1, NA, 2,
#'                    3, 4, 5), nrow = 2, byrow = TRUE)  # NA for placeholder
#' ggmultiplot(plotlist = plots, layout = layout)
#'
#' @references inspired by \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook R}
multiplot <- function(..., plotlist=NULL, cols=NULL, layout=NULL, title='', title.size=20) {
    # Make a list from the ... arguments and plotlist
    # remove NULL objects from ..., see https://stackoverflow.com/a/48519190/2292993
    plots <- c(Filter(Negate(is.null), list(...)), plotlist)

    if (ggplot2::is.ggplot(plots[[1]])) {return(ggmultiplot(...,plotlist=plotlist,cols=cols,layout=layout,title=title,title.size=title.size))}

    numPlots = length(plots)
    if (is.null(cols)) {cols=floor(sqrt(length(plots)))}

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
        gridExtra::grid.arrange(grobs=plots,layout_matrix=layout,
            top=grid::textGrob(title,gp=grid::gpar(fontsize=title.size,fontface=2,fontfamily=RMN)))

        # # other example usage for reference:
        # gridExtra::grid.arrange(p1,p2,p3,layout_matrix=matrix(c(NA,1,2,3,4,5), nrow=2, byrow=TRUE) )
        # gridExtra::grid.arrange(p1,p2,p3,ncol=2)
        # gridExtra::grid.arrange(p1,p2,p3,nrow=2)
        # plist <- list(p1,p2,p3)
        # gridExtra::grid.arrange(grobs=plist,ncol=floor(sqrt(length(plist))))
    }
}

#' ggplot2 Theme for APA Publications
#' @description ggplot2 Theme for APA Publications
#'
#' A ggplot2 theme with no background and Times New Roman font (legend size not affected).
#'
#' @param lab.size default 18
#' @param text.size default 16
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @note In order for R (at least on Mac) to recognize Times New Roman font, \href{https://github.com/wch/extrafont/}{extrafont} required
#' @seealso \code{\link[ggplot2]{theme}} \code{\link{theme_apa_nosize}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @author Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
theme_apa <- function(plot.box = FALSE, lab.size = 18, text.size = 16){
    out <- theme(
        plot.title=element_text(family=RMN, size=lab.size+2, face="bold", colour="black"),
        legend.title = element_text(family=RMN, colour="black"),
        legend.text = element_text(family=RMN, colour="black"),
        strip.text.x = element_text(family=RMN, size=text.size, colour="black"),
        strip.text.y = element_text(family=RMN, size=text.size, colour="black"),
        axis.title.x=element_text(family=RMN, size=lab.size, face="bold", colour="black"),
        axis.title.y=element_text(family=RMN, size=lab.size, face="bold", angle=90, colour="black"),
        axis.text.x=element_text(family=RMN, size=text.size, colour="black"),
        axis.text.y=element_text(family=RMN, size=text.size, colour="black"),
        axis.ticks=element_line(colour="black"))
    if (plot.box) {
        # panel.border without axis.line
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = "black"))
    } else {
        # no panel.border but axis.line
        out <- out + theme(panel.background = element_rect(fill = NA,colour = NA),
                           panel.border = element_rect(fill = NA, colour = NA),
                           axis.line = element_line(colour = "black"))
    }
    out
}

#' ggplot2 Theme for APA Publications
#' @description ggplot2 Theme for APA Publications
#'
#' A ggplot2 theme with no background and Times New Roman font (do not change font size).
#'
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @note In order for R (at least on Mac) to recognize Times New Roman font, \href{https://github.com/wch/extrafont/}{extrafont} required
#' @seealso \code{\link[ggplot2]{theme}} \code{\link{theme_apa}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @author Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
theme_apa_nosize <- function(plot.box = FALSE){
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
    if (plot.box) {
        # panel.border without axis.line
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = "black"))
    } else {
        # no panel.border but axis.line
        out <- out + theme(panel.background = element_rect(fill = NA,colour = NA),
                           panel.border = element_rect(fill = NA, colour = NA),
                           axis.line = element_line(colour = "black"))
    }
    out
}

#' A ggplot2 theme with no background and no gridlines.
#' @description A ggplot2 theme with no background and no gridlines.
#' @param lab.size default 18
#' @param text.size default 16
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @author Jon Lefcheck (\url{http://jonlefcheck.net}) Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
#' @references \url{http://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2} \url{https://gist.github.com/jslefche/eff85ef06b4705e6efbc}
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_grey theme element_blank element_text element_line element_rect %+replace%
#' @examples
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill="white") + theme_black()
#' dat <- data.frame(y = c(austres), time = time(austres))
#' ggplot(dat, aes(time, y)) + scale_x_continuous() +
#'     geom_line(color="lightblue", size=1) + theme_black()
#'
#' \dontrun{
#' library(maps)
#' crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#' states_map <-map_data("state")
#'
#' ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     theme_black() +
#'     scale_fill_gradient(low="grey10", high="white")
#' }
theme_blackapa <- function(plot.box = TRUE, lab.size = 18, text.size = 16) {
    out <- theme(
            # Specify axis options
            axis.text.x=element_text(family=RMN, size=text.size, colour="grey55"),
            axis.text.y=element_text(family=RMN, size=text.size, colour="grey55"),
            axis.ticks=element_line(colour="grey55"),
            axis.title.x=element_text(family=RMN, size=lab.size, face="bold", colour="grey55"),
            axis.title.y=element_text(family=RMN, size=lab.size, face="bold", angle=90, colour="grey55"),
            # Specify legend options
            legend.background=element_rect(color=NA, fill="black"),
            legend.key=element_rect(color=NA, fill="black"),
            legend.title = element_text(family=RMN, colour="grey55"),
            legend.text = element_text(family=RMN, colour="grey55"),
            legend.box.background = element_rect(colour = "grey55"),
            # Specify panel options
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            # Specify facetting options
            strip.background=element_rect(fill="grey30", color="grey10"),
            strip.text.x = element_text(family=RMN, size=text.size, colour="grey55"),
            strip.text.y = element_text(family=RMN, size=text.size, colour="grey55"),
            # Specify plot options
            plot.background=element_rect(color="black", fill="black"),
            plot.title=element_text(family=RMN, size=lab.size+2, face="bold", colour="grey55")
    )
    if (plot.box) {
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = "grey55"))
    } else {
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = NA),
                           axis.line = element_line(colour = "grey55"))
    }
    ez.pprint("geom_point/line/errorbar/bar color cannot be changed with theme; modify original codes: color='grey55'")
    out

}

#' A ggplot2 theme with no background and no gridlines.
#' @description A ggplot2 theme with no background and no gridlines.
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @author Jon Lefcheck (\url{http://jonlefcheck.net}) Jerry modified from \href{https://github.com/trinker/plotflow}{trinker/plotflow}
#' @references \url{http://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2} \url{https://gist.github.com/jslefche/eff85ef06b4705e6efbc}
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_grey theme element_blank element_text element_line element_rect %+replace%
#' @examples
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill="white") + theme_black()
#' dat <- data.frame(y = c(austres), time = time(austres))
#' ggplot(dat, aes(time, y)) + scale_x_continuous() +
#'     geom_line(color="lightblue", size=1) + theme_black()
#'
#' \dontrun{
#' library(maps)
#' crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#' states_map <-map_data("state")
#'
#' ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     theme_black() +
#'     scale_fill_gradient(low="grey10", high="white")
#' }
theme_blackapa_nosize <- function(plot.box = TRUE) {
    out <- theme(
            # Specify axis options
            axis.text.x=element_text(family=RMN, colour="grey55"),
            axis.text.y=element_text(family=RMN, colour="grey55"),
            axis.ticks=element_line(colour="grey55"),
            axis.title.x=element_text(family=RMN, face="bold", colour="grey55"),
            axis.title.y=element_text(family=RMN, face="bold", angle=90, colour="grey55"),
            # Specify legend options
            legend.background=element_rect(color=NA, fill="black"),
            legend.key=element_rect(color=NA, fill="black"),
            legend.title = element_text(family=RMN, colour="grey55"),
            legend.text = element_text(family=RMN, colour="grey55"),
            legend.box.background = element_rect(colour = "grey55"),
            # Specify panel options
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            # Specify facetting options
            strip.background=element_rect(fill="grey30", color="grey10"),
            strip.text.x = element_text(family=RMN, colour="grey55"),
            strip.text.y = element_text(family=RMN, colour="grey55"),
            # Specify plot options
            plot.background=element_rect(color="black", fill="black"),
            plot.title=element_text(family=RMN, face="bold", colour="grey55")
    )
    if (plot.box) {
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = "grey55"))
    } else {
        out <- out + theme(panel.background = element_rect(fill = NA, colour = NA),
                           panel.border = element_rect(fill = NA, colour = NA),
                           axis.line = element_line(colour = "grey55"))
    }
    ez.pprint("geom_point/line/errorbar/bar color cannot be changed with theme; modify original codes: color='grey55'")
    out
}

###**************************************************.
###*with ez. in the function name.
###**************************************************.
#' wrapper of \code{\link{dev.copy2pdf}}
#' @description wrapper of \code{\link{dev.copy2pdf}}
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
#' @description wrapper of \code{\link{pdf}}, direct all plotting to a pdf file(s)
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
#' @description wrapper of \code{\link{dev.off}}
#' @note additionally one can use \code{\link[extrafont]{embed_fonts}} to embed fonts after dev.off()
#' @seealso \code{\link{ez.export}} \code{\link{ez.pdfon}}
#' @examples
#' A4:     width 7(inches) height = 5
#' Letter: 8.5 x 11
#' @export
ez.pdfoff = dev.off

#' subplot, wrapper of \code{\link{par}}
#' @description subplot, wrapper of \code{\link{par}}
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
#' @description embed a new plot within an existing plot at the coordinates specified (in user units of the existing plot)
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

#' barplot with ggplot
#' @description barplot with ggplot
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd like "y|x, y|x z, y|x z a", where y (axis) is continous, x (axis) z/a (legend) are discrete; during plot x z a ->x za(combined)
#' \cr or "y|x+covar1+covar2+..." (currently only supports anova1b)
#' @param color  "bw" or "color"  black/white or colorblind-friendly color
#' @param bar.gap  the gap between bars
#' @param bar.width  the width of bar itself
#' @param error.size  the thickness of error bar (vertical and horizontal lines)
#' @param error.gap  the location of errorbar, should be equal to bar.width(?)
#' @param error.width the width of error bar (horizontal lines), could be 0 to show only vertical lines
#' @param error.direction  "both", "max", "min"
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.barplot = function(df,cmd,color='color',colors=ez.palette("Zhu"),bar.gap=0.7,bar.width=0.7,error.size=0.7,error.gap=0.7,error.width=0.3,error.direction='both',ylab='Mean',xlab=NULL,zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),xangle=0,vjust=NULL,hjust=NULL,print2scr=TRUE,
    point=FALSE,point.jitter=0.15,point.size=1.5,point.alpha=1,point.color='grey55',theme.apa=TRUE,...) {
    if (print2scr & !grepl('+',cmd,fixed=TRUE) & !grepl('[\\w\\.]+\\s+[\\w\\.]',cmd,perl=TRUE)) {ez.anovas1b(df,cmd,report=T,view=F,plot=F,error=T)}

    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    color = ifelse(color=='bw','scale_fill_grey(start=0,end=0.8)','scale_fill_manual(values=colors)')

    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')

    ymin = ifelse(error.direction %in% c('min','both'),'avgAverage-se','avgAverage')
    ymax = ifelse(error.direction %in% c('max','both'),'avgAverage+se','avgAverage')

    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))

####************************************************************************************************
                                     ####*ancova start*####
####************************************************************************************************
    # y|x+covar1+covar2 (ancova)
    if (grepl('+',cmd,fixed=TRUE)) {
        cmd = ez.trim(cmd)
        cmd = strsplit(cmd,'[~|]')[[1]]
        yy = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        xx = strsplit(xx,"+",fixed=TRUE)[[1]] %>% sapply(trimws) %>% unname()
        covar = xx[-1]
        xx = xx[1]

        if (point) {
            points = ez.sprintf('geom_point(aes(x={xx},y={yy}),data=df,position=position_jitter(width={point.jitter}, height=0),size={point.size},alpha={point.alpha},color="{point.color}")+')
            ez.pprint('Attention: Point values are not adjusted! Mean (SE) are.','red')
        } else {
            points = ''
        }

        tt = sprintf('
                    pp=aov(%s~%s%s,data=df) %%>%%
                    effects::effect("%s",.) %%>%%
                    data.frame() %%>%% ez.rncol(c("fit"="avgAverage")) %%>%% ez.factorder("%s",ord="as") %%>%%

                    ggplot2::ggplot(aes(x=%s,y=avgAverage,fill=%s)) +
                    geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                    %s +
                    geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                    %s %s %s %s %s
                    theme(axis.text.x=element_text(angle=%f %s %s)) +
                    theme(legend.direction="%s") +
                    theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                    ,yy, xx, paste('+',covar,sep='',collapse=''), xx, xx, xx, xx, bar.gap, bar.width, color, ymin, ymax, error.size, error.width, error.gap, ylab, xlab, 'theme(legend.position="none")+', legend.box, points, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                    )
        gghistory=paste(gghistory,
                  sprintf('df=ez.dropna(df,c("%s","%s", "%s"))',yy,xx,paste(covar,sep='',collapse='","')),
                  tt,sep='\n')

        if (theme.apa) tt = paste0(tt,'+theme_apa()')
        eval(parse(text = tt))
        pp$gghistory=paste0(gghistory,'\nprint(pp)')
        pp$df=df.bak
        return(pp)
    }
####************************************************************************************************
                                     ####*ancova end*####
####************************************************************************************************

    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces
    # also see trimws()--remove trailing/leading spaces
    cmd = ez.trim(cmd)
    cmd = strsplit(cmd,'[~|]')[[1]]
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

            if (point) {
                points = ez.sprintf('geom_point(aes(x={xx},y={yy}),data=df,position=position_jitter(width={point.jitter}, height=0),size={point.size},alpha={point.alpha},color="{point.color}")+')
            } else {
                points = ''
            }

            tt = sprintf('
                         pp=group_by(df,%s) %%>%%
                         summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%

                         ggplot2::ggplot(aes(x=%s,y=avgAverage,fill=%s)) +
                         geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                         %s +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                         %s %s %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") +
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, yy, yy, xx, xx, bar.gap, bar.width, color, ymin, ymax, error.size, error.width, error.gap, ylab, xlab, 'theme(legend.position="none")+', legend.box, points, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                         )
            gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s"))',yy,xx),
                         tt,sep='\n')
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
                            summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%

                            ggplot2::ggplot(aes(x=%s,y=avgAverage,fill=%s)) +
                            geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                            %s +
                            geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") +
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, yy, yy, xx, zz, bar.gap, bar.width, color, ymin, ymax, error.size, error.width, error.gap, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                )
                gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s"))',yy,xx,zz),
                         tt,sep='\n')
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
                                summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%
                                unite(zzaa,c(%s,%s)) %%>%% ez.2factor("zzaa") %%>%% ez.factorder("zzaa") %%>%%

                                ggplot2::ggplot(aes(x=%s,y=avgAverage,fill=zzaa)) +
                                geom_bar(position=position_dodge(width=%f), stat="identity", width=%f, color="black") +
                                %s +
                                geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                                %s %s %s
                                %s %s
                                theme(axis.text.x=element_text(angle=%f %s %s)) +
                                theme(legend.direction="%s") +
                                theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                                , xx, zz, aa, yy, yy, zz, aa, xx, bar.gap, bar.width, color, ymin, ymax, error.size, error.width, error.gap, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                    )
                    gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s","%s"))',yy,xx,zz,aa),
                         tt,sep='\n')
                }
            }
        }
    }
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' line plot with ggplot
#' @description line plot with ggplot
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd like "y|x, y|x z, y|x z a" where y (axis) is continous, x (axis) z/a (legend) are discrete, during plot x z a ->x z(za combined)
#' @param line.size  the thickness of line, only applicable when there is z provided
#' @param error.size  the thickness of error bar line
#' @param error.gap  the location of errorbar, should not be adjusted, 0 (parameter kept for reference)
#' @param error.width the width of the bar of error
#' @param error.direction  'both', 'max', 'min'
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot) , +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.lineplot = function(df,cmd,colors=ez.palette("Zhu"),line.size=0.7,error.size=0.7,error.gap=0,error.width=0.3,error.direction='both',ylab='Mean',xlab=NULL,zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),xangle=0,vjust=NULL,hjust=NULL,theme.apa=TRUE) {
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')

    ymin = ifelse(error.direction %in% c('min','both'),'avgAverage-se','avgAverage')
    ymax = ifelse(error.direction %in% c('max','both'),'avgAverage+se','avgAverage')

    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))

    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces
    # also see trimws()--remove trailing/leading spaces
    cmd = ez.trim(cmd)
    cmd = strsplit(cmd,'[~|]')[[1]]
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
                         summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%

                         ggplot2::ggplot(aes(x=%s,y=avgAverage,group=1)) +
                         geom_point() +
                         geom_line(size=%f) +
                         geom_errorbar(aes(ymin=%s, ymax=%s), size=%f, width=%f, position=position_dodge(width=%f)) +

                         %s %s %s %s
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") +
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                         , xx, yy, yy, xx, line.size, ymin, ymax, error.size, error.width, error.gap, ylab, xlab, 'theme(legend.position="none")+', legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                         )
            gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s"))',yy,xx),
                         tt,sep='\n')
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
                            summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%

                            ggplot2::ggplot(aes(x=%s,y=avgAverage,group=%s)) +
                            geom_point(aes(shape=%s,color=%s)) +
                            geom_line(aes(linetype=%s,color=%s), size=%f) +
                            geom_errorbar(aes(ymin=%s, ymax=%s, linetype=%s, color=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                            scale_color_manual(values=colors) +

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") +
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, yy, yy, xx, zz, zz, zz, zz, zz, line.size, ymin, ymax, zz, zz, error.size, error.width, error.gap, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                )
                gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s"))',yy,xx,zz),
                         tt,sep='\n')
            } else {
                # yy|xx zz aa
                if (length(xx)==3) {
                    aa = xx[3]
                    zz = xx[2]
                    xx = xx[1]
                    df=ez.dropna(df,c(yy,xx,zz,aa))
                    tt = sprintf('
                            pp=group_by(df,%s,%s,%s) %%>%%
                            summarise(avgAverage=mean(%s),se=sd(%s)/sqrt(n())) %%>%%
                            unite(%s,c(%s,%s)) %%>%% ez.2factor("%s") %%>%% ez.factorder("%s") %%>%%

                            ggplot2::ggplot(aes(x=%s,y=avgAverage,group=%s)) +
                            geom_point(aes(shape=%s,color=%s)) +
                            geom_line(aes(linetype=%s,color=%s), size=%f) +
                            geom_errorbar(aes(ymin=%s, ymax=%s, linetype=%s, color=%s), size=%f, width=%f, position=position_dodge(width=%f)) +
                            scale_color_manual(values=colors) +

                            %s %s %s
                            %s %s
                            theme(axis.text.x=element_text(angle=%f %s %s)) +
                            theme(legend.direction="%s") +
                            theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                            , xx, zz, aa, yy, yy, zz, zz, aa, zz, zz, xx, zz, zz, zz, zz, zz, line.size, ymin, ymax, zz, zz, error.size, error.width, error.gap, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
                    )
                    gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s","%s"))',yy,xx,zz,aa),
                         tt,sep='\n')
                }
            }
        }
    }
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' mimic xyplot with ggplot (slightly horizontally jittered)
#' @description mimic xyplot with ggplot (slightly horizontally jittered)
#' @param df data frame in long format
#' @param cmd like "y|x,g", "y|x z,g", or "y|x z a,g" where y is continous, x z a are discrete, g is individual/grouping variable
#' \cr 'FinalMem|Attention, SubjectID'     'FinalMem|Attention Condition, SubjectID'
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @param facet  one of 'cols', 'rows', 'wrap'
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.xyplot = function(df,cmd,ylab=NULL,xlab=NULL,xangle=0,vjust=NULL,hjust=NULL,facet='cols',theme.apa=TRUE){
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    # http://stackoverflow.com/a/25734388/2292993
    # Merge Multiple spaces to single space, and remove trailing/leading spaces
    cmd = ez.trim(cmd)

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
        gghistory=paste(gghistory,
                 sprintf('df=ez.dropna(df,c("%s","%s","%s"))',yy,xx,gg),
                 tt,sep='\n')
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
                        %s"
                 , xx,yy,gg,gg,xlab,ylab,xangle,vjust,hjust,sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),zz)
            )
            gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s","%s"))',yy,xx,gg,zz),
                         tt,sep='\n')
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
                gghistory=paste(gghistory,
                         sprintf('df=ez.dropna(df,c("%s","%s","%s","%s","%s"))',yy,xx,gg,zz,aa),
                         tt,sep='\n')
            }
        }
    }

    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' plot a heatmap with values shown
#' @description plot a heatmap with values shown
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
#' @export
ez.heatmap = function(df, id, show.values=F, remove.zero=T, angle=270, colors=c("blue", "white", "red"), basesize=9, xsize=1, ysize=1, legend.position="right",theme.apa=TRUE){
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    cmd = sprintf('tidyr::gather(df, key,value,-%s,factor_key = T) -> df
                  df$%s = factor(df$%s,rev(unique(as.character(df$%s))))
                  ',id,id,id,id)
    eval(parse(text = cmd))
    gghistory=paste(gghistory,cmd,sep='\n')

    x = "key"; y = id; z = "value"
    if (show.values) {
        tt = sprintf('
                    # helper function to remove leading 0 in correlation
                    remove0 <- function(value, remove.zero=T, prefix=""){  # format string more concisely
                        if (remove.zero) {
                            lst = c()
                            for (item in value) {
                                if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
                                    lst <- c(lst, "")
                                    next
                                }
                                item <- round(item, 2) # round to two digits
                                if (item == 0) { # if rounding results in 0 clarify
                                    item = "<.01"
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

                    p = ggplot(df, aes(%s, %s)) +
                    geom_tile(aes(fill = %s)) +
                    scale_fill_gradient2(low = "%s", mid = "%s", high = "%s") +
                    geom_text(aes(fill = %s, label = remove0(%s,%s))) +
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
        tt = sprintf('
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
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    gghistory=paste(gghistory,tt,sep='\n')
    pp=p
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' plot a correlation matrix map
#' @description a wrapper of \code{\link[corrplot]{corrplot}}; the correlation and p values are calculated with \code{\link[Hmisc]{rcorr}}, in which Missing values are deleted in pairs rather than deleting all rows of x having any missing variables
#' @param df data frame in wide format, should be all numeric
#' @param corr.type "pearson" or "spearman", pairwise deletion for NA
#' @param sig.level sig.level. make it 1 to essentially ignore param insig
#' @param insig how to treat insig values, one of "pch"(show x, corrplot default),"p-value","blank", "n"(no change, as is)
#' @param type Character, "full" (default), "upper" or "lower", display full matrix, lower triangular or upper triangular matrix.
#' @param tl.col color of text label
#' @param tl.cex size of text label (can still be increased to gain some margins for the map when tl.pos='n')
#' @param tl.pos 'n' for no text label. NULL=corrplot default=auto(??). Character or logical, position of text labels. If character, it must be one of "lt", "ld", "td", "d" or "n". "lt"(default if type=="full") means left and top, "ld"(default if type=="lower") means left and diagonal, "td"(default if type=="upper") means top and diagonal(near), "d" means diagonal, "n" means don't add textlabel.
#' @param tl.srt Numeric, for text label string rotation in degrees.
#' @param addgrid.col grid color. The color of the grid. If NA, don't add grid. If NULL the default value is chosen. The default value depends on method, if method is color or shade, the color of the grid is NA, that is, not draw grid; otherwise "grey".
#' @param addCoef.col Color of coefficients added on the graph (eg, 'black'). If NULL (default), add no coefficients.
#' @param diag Logical, whether display the correlation coefficients on the principal diagonal
#' @param method "circle", "square", "ellipse", "number", "pie", "shade" and "color". The areas of circles or squares show the absolute value of corresponding correlation coefficients. "color" = same areas
#' @param order "original" for original order (default).
#' \cr "AOE" for the angular order of the eigenvectors.
#' \cr "FPC" for the first principal component order.
#' \cr "hclust" for the hierarchical clustering order.
#' \cr "alphabet" for alphabetical order.
#' @param col Vector, the color of glyphs. NULL=corrplot default. Jerry auto generates a default one if NULL.
#' @param ... see \code{\link[corrplot]{corrplot}} for more parameters
#' @return returns a list (r, p) r: a matrix representing the corrmap (p > sig.level, set to NA/blank), p: all raw p values
#' @export
ez.corrmap = function(df,corr.type="pearson",sig.level=0.05,insig="blank",type="lower",
                     method="ellipse",tl.col="black",tl.cex=0.4,tl.pos=NULL,tl.srt=45,
                     addgrid.col=rgb(1,1,1,.01),addCoef.col=NULL,diag=FALSE,
                     order=c("original", "AOE", "FPC", "hclust", "alphabet"),
                     col=NULL,...){
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))

    if (is.null(col)){
        # col1 <- colorRampPalette(rev(c("#7F0000", "red", "#FF7F00", "yellow", "grey", "cyan",
        #                                "#007FFF", "blue", "#00007F")))
        # col=col1(100)

        cols <- c(rev(RColorBrewer::brewer.pal(7, "Blues")), RColorBrewer::brewer.pal(7, "Reds"))
        col  <- colorRampPalette(cols)(200)
    }

    corrmatrix = Hmisc::rcorr(data.matrix(df), type=corr.type)
    M = corrmatrix$r
    p.mat = corrmatrix$P

    # increase tl.cex a bit to gain some margins for the map when tl.pos='n'
    if (!is.null(tl.pos)) {if (tl.pos=='n' & tl.cex < 1) {tl.cex = 1.5}}
    # add grid is like a mask/cover over the map (??)
    corr = corrplot::corrplot(M, method = method, p.mat = p.mat, sig.level = sig.level,  insig = insig,
                       tl.col = tl.col, tl.cex = tl.cex, tl.pos = tl.pos,
                       order = order,
                       col = col, addgrid.col = addgrid.col, type=type, tl.srt=tl.srt, addCoef.col=addCoef.col, diag=diag, ...)

    ind = which(p.mat > sig.level, arr.ind = TRUE)
    corr[ind] = NA

    return(invisible(list(r=corr,p=p.mat)))
}

#' corrmap with scatterplot
#' @description corrmap with scatterplot
#' @param x a data.frame or matrix (pairwise deletion for NA)
#' @param group factor col name, eg 'Species', to show different groups with different colors in scatterplot. This col will be excluded in the calcuation of r and scatterplot
#' @param cex.cor If this is specified, this will change the size of the text in the correlations. this allows one to also change the size of the points in the plot by specifying the normal cex values. If just specifying cex, it will change the character size, if cex.cor is specified, then cex will function to change the point size.
#' @param lm Plot the linear fit rather than the LOESS smoothed fits.
#' @param histogram Plot the middle diagonal histogram or show only variable name
#' @param smooth TRUE draws loess smooths
#' @param scale  TRUE scales the correlation font by the size of the absolute correlation.
#' @param density TRUE shows the density plots as well as histograms
#' @param ellipses TRUE draws correlation ellipses
#' @param digits  the number of digits to show
#' @param method method parameter for the correlation ("pearson","spearman","kendall")
#' @param pch The plot character (defaults to 16 which is a '.').
#' @param cor If plotting regressions, should correlations be reported?
#' @param jiggle Should the points be jittered before plotting?
#' @param factor factor for jittering (1-5)
#' @param hist.col What color should the histogram on the diagonal be? eg, cyan, light grey, "#00AFBB"
#' @param show.points If FALSE, do not show the data points, just the data ellipses and smoothed functions
#' @param rug if TRUE (default) draw a rug under the histogram, if FALSE, don't draw the rug
#' @param breaks If specified, allows control for the number of breaks in the histogram (see the hist function)
#' @param smoother If TRUE, then smooth.scatter the data points  -- slow but pretty with lots of subjects
#' @param stars For those people who like to show the significance of correlations by using magic astricks, set stars=TRUE p-values(0, 0.001, 0.01, 0.05, 1) <=> symbols("***", "**", "*", ".", " ")
#' @param ci Draw confidence intervals for the linear model or for the loess fit, defaults to ci=FALSE. If confidence intervals are not drawn, the fitting function is lowess.
#' @param alpha The alpha level for the confidence regions, defaults to .05
#' @param ... other options for pairs
#' @export
ez.scatterplots = function(x,group=NULL,histogram=TRUE,smooth=TRUE,scale=TRUE,density=TRUE,ellipses=FALSE,digits=2,method="pearson",pch=16,colors=ez.palette("Zhu"),
lm=FALSE,cor=TRUE,jiggle=FALSE,factor=2,hist.col="light grey",show.points=TRUE,rug=TRUE,breaks="Sturges",cex.cor=2,smoother=FALSE,stars=TRUE,ci=FALSE,alpha=.05,...) {
    # To show different groups with different colors, use a plot character (pch) between 21 and 25 and then set the background color to vary by group
    bg = 'black'  # color for data point
    if (!is.null(group)) {

        bg=colors[1:nlevels(as.factor(x[[group]]))][as.factor(x[[group]])]
        pch = 20+as.numeric(x[[group]])
        x[group] <- NULL
    }

    ####************************************************************************************************
                                     ####*begin*####
    ####************************************************************************************************
    # removed wt parameter
    # modified stars
    # added histogram=T/F
    # swapped upper and lower panels
    # modified from https://cran.r-project.org/src/contrib/psych_1.8.12.tar.gz
    "fisherz" <-
    function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z

    "fisherz2r" <-
      function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again

    "r.con" <-
      function(rho,n,p=.95,twotailed=TRUE) {
       z <- fisherz(rho)
       if(n<4) {stop("number of subjects must be greater than 3")}
       se <- 1/sqrt(n-3)
       p <- 1-p
       if(twotailed) p<- p/2
       dif <- qnorm(p)
       zlow <- z + dif*se
       zhigh <- z - dif*se
       ci <- c(zlow,zhigh)
       ci <- fisherz2r(ci)
       return(ci)
       }

     "r.test" <-
     function(n,r12, r34=NULL, r23=NULL,r13=NULL,r14=NULL,r24=NULL,n2=NULL,pooled=TRUE, twotailed=TRUE) {
      cl <- match.call()
      if(is.null(r34) & is.null(r13) & is.null(r23)) {  #test for significance of r

         t <- r12*sqrt(n-2)/sqrt(1-r12^2)
         p <- 1-pt(abs(t),n-2)
         if(twotailed) p <- 2*p
         ci <- r.con(r12,n)
          result <-  list(Call=cl,Test="Test of significance of a  correlation",t=t,p=p,ci=ci)
      } else {if(is.null(r23)) { #compare two independent correlations
          if(is.null(r34)) {stop("You seem to be testing two dependent correlations, but have not specified the other correlation(s)  correctly.")}
           if(!is.null(r13)) {stop("You seem to be testing two dependent correlations, but have not specified the correlation(s)  correctly.")}
            xy.z <- 0.5*log((1+r12)/(1-r12))
            xz.z <- 0.5*log((1+r34)/(1-r34))
            if(is.null(n2)) n2 <- n
            se.diff.r <- sqrt(1/(n-3) + 1/(n2-3))
            diff <- xy.z - xz.z
            z <- abs(diff/se.diff.r)
             p <- (1-pnorm(z ))
              if(twotailed) p <- 2*p
          result <-  list(Call=cl,Test="Test of difference between two independent correlations",z=z,p=p)
                                 }  else { if (is.null(r14)) {#compare two dependent correlations case 1


            #here we do two tests of dependent correlations
           #figure out whether correlations are being specified by name or order
            if(!is.null(r34)) {if(is.null(r13)) {r13 <- r34} }
            if(is.null(r13)) {stop("You seem to be trying to test two dependent correlations, but have not specified the other correlation(s)")}
           diff <- r12-r13
           determin=1-r12*r12 - r23*r23 - r13*r13 + 2*r12*r23*r13
           av=(r12+r13)/2
           cube= (1-r23)*(1-r23)*(1-r23)
           t2 = diff * sqrt((n-1)*(1+r23)/(((2*(n-1)/(n-3))*determin+av*av*cube)))
           p <- pt(abs(t2),n-3,lower.tail=FALSE)  #changed to n-3 on 30/11/14
            if(twotailed) p <- 2*p
            #the call is ambiguous, we need to clarify it
            cl <- paste("r.test(n = ",n, ",  r12 = ",r12,",  r23 = ",r23,",  r13 = ",r13, ")")
          result <- list(Call=cl,Test="Test of difference between two correlated  correlations",t=t2,p=p)
          } else { #compare two dependent correlations, case 2
           z12 <- fisherz(r12)
        z34 <- fisherz(r34)
        pooledr <- (r12+r34)/2
        if (pooled) { r1234=  1/2 * ((r13 - pooledr*r23)*(r24 - r23*pooledr) + (r14 - r13*pooledr)*(r23 - pooledr*r13)   +(r13 - r14*pooledr)*(r24 - pooledr*r14) + (r14 - pooledr*r24)*(r23 - r24*pooledr))
                   z1234 <- r1234/((1-pooledr^2)*(1-pooledr^2))} else {

         r1234=  1/2 * ((r13 - r12*r23)*(r24 - r23*r34) + (r14 - r13*r34)*(r23 - r12*r13)   +(r13 - r14*r34)*(r24 - r12*r14) + (r14 - r12*r24)*(r23 - r24*r34))
         z1234 <- r1234/((1-r12^2)*(1-r34^2))}
          ztest  <- (z12-z34)* sqrt(n-3) /sqrt(2*(1-z1234))
          z <- ztest
           p <- (1-pnorm(abs(z) ))
              if(twotailed) p <- 2*p
           result <-  list(Call=cl,Test="Test of difference between two dependent correlations",z=z,p=p)

                                  }
               }
        }
        class(result) <- c("psych", "r.test")
        return(result)
       }
       #Modified August 8, 2018 to flag improper input


    "my.pairs.panels" <-
    function (x, smooth = TRUE, scale = FALSE, histogram=TRUE, density=TRUE,ellipses=TRUE,digits = 2, method="pearson",pch = 20,lm=FALSE,cor=TRUE,jiggle=FALSE,factor=2,hist.col="cyan",show.points=TRUE,rug=TRUE, breaks="Sturges", cex.cor = 1 ,wt=NULL,smoother=FALSE,stars=FALSE,ci=FALSE,alpha=.05,...)   #combines a splom, histograms, and correlations
    {

    #First define all the "little functions" that are internal to pairs.panels.  This allows easier coding later


    "panel.hist.density" <-
    function(x,...) {
     if (!histogram) {return(NULL)}
     usr <- par("usr"); on.exit(par(usr))
      # par(usr = c(usr[1]-abs(.05*usr[1]) ,usr[2]+ abs(.05*usr[2])  , 0, 1.5) )
         par(usr = c(usr[1] ,usr[2]  , 0, 1.5) )
       tax <- table(x)
       if(length(tax) < 11) {breaks <- as.numeric(names(tax))
        y <- tax/max(tax)
        interbreak <- min(diff(breaks))*(length(tax)-1)/41
        rect(breaks-interbreak,0,breaks + interbreak,y,col=hist.col)
        } else {

        h <- hist(x,breaks=breaks, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y,col=hist.col)
        }
     if(density) { tryd <- try( d <- density(x,na.rm=TRUE,bw="nrd",adjust=1.2),silent=TRUE)
      if(class(tryd) != "try-error") {
         d$y <- d$y/max(d$y)
       lines(d)}}
      if(rug) rug(x)
    }


    "panel.cor" <-
    function(x, y, prefix="",...)  {

             usr <- par("usr"); on.exit(par(usr))
             par(usr = c(0, 1, 0, 1))
            if(is.null(wt)) { r  <- cor(x, y,use="pairwise",method=method)} else {
            r <- cor.wt(data.frame(x,y),w=wt[,c(1:2)])$r[1,2]}
             txt <- format(c(round(r,digits), 0.123456789), digits=digits)[1]
             txt <- paste(prefix, txt, sep="")
             # if(stars) {pval <- r.test(sum(!is.na(x*y)),r)$p
             #          symp <- symnum(pval, corr = FALSE,cutpoints = c(0,  .001,.01,.05, 1),
             #        symbols = c("***","**","*"," "),legend=FALSE)
             #        txt <- paste0(txt,symp)}
             cex <- cex.cor*0.8/(max(strwidth("0.12***"),strwidth(txt)))
             # https://github.com/braverock/PerformanceAnalytics/blob/master/R/chart.Correlation.R
             # no scale for stars
             if(stars) {pval <- r.test(sum(!is.na(x*y)),r)$p
                      symp <- symnum(pval, corr = FALSE,cutpoints = c(0,  .001,.01,.05, 1),
                    symbols = c("***","**","*"," "),legend=FALSE)
                    text(.8, .8, symp, cex=cex, col=2)}
             if(scale)  {cex1 <- cex  * abs(r)
             if(cex1 < .25) cex1 <- .25 #otherwise they just vanish
             text(0.5, 0.5, txt, cex = cex1) } else {
             text(0.5, 0.5, txt,cex=cex)}
         }

    "panel.smoother" <-
    function (x, y,pch = par("pch"),
        col.smooth = "red", span = 2/3, iter = 3, ...)
    {
      # usr <- par("usr"); on.exit(par(usr))
     #  par(usr = c(usr[1]-abs(.05*usr[1]) ,usr[2]+ abs(.05*usr[2])  , usr[3],usr[4]) )     #doensn't affect the axis correctly
      xm <- mean(x,na.rm=TRUE)
      ym <- mean(y,na.rm=TRUE)
      xs <- sd(x,na.rm=TRUE)
      ys <- sd(y,na.rm=TRUE)
      r = cor(x, y,use="pairwise",method=method)
     if(jiggle) { x <- jitter(x,factor=factor)
      y <- jitter(y,factor=factor)}
     if(smoother) {smoothScatter(x,y,add=TRUE, nrpoints=0)} else {if(show.points)  points(x, y, pch = pch, ...)}

        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) {
         if(smooth & ci) {   lml <- loess(y~x ,degree=1,family="symmetric")
         tempx <- data.frame(x = seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=47))
           pred <-  predict(lml,newdata=tempx,se=TRUE )

     if(ci) {  upperci <- pred$fit + confid*pred$se.fit
           lowerci <- pred$fit - confid*pred$se.fit
          polygon(c(tempx$x,rev(tempx$x)),c(lowerci,rev(upperci)),col=adjustcolor("light grey", alpha.f=0.8), border=NA)
            }
         lines(tempx$x,pred$fit,  col = col.smooth, ...)   #this is the loess fit
    }  else {if(smooth)  lines(stats::lowess(x[ok],y[ok],f=span,iter=iter),col=col.smooth) }}
     if(ellipses)  draw.ellipse(xm,ym,xs,ys,r,col.smooth=col.smooth,...)  #this just draws the ellipse
    }

     "panel.lm" <-
    function (x, y,  pch = par("pch"),
        col.lm = "red",  ...)
    {   ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin,xmin),max(ymax,xmax))
        xlim <- ylim
        if(jiggle) { x <- jitter(x,factor=factor)
                     y <- jitter(y,factor=factor)}
      if(smoother) {smoothScatter(x,y,add=TRUE, nrpoints=0)} else {if(show.points) {points(x, y, pch = pch,ylim = ylim, xlim= xlim, ...)}}# if(show.points) points(x, y, pch = pch,ylim = ylim, xlim= xlim,...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) {
          lml <- lm(y ~ x)


        if(ci) {
             tempx <- data.frame(x = seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=47))
             pred <-  predict.lm(lml,newdata=tempx,se.fit=TRUE)  #from Julian Martins
             upperci <- pred$fit + confid*pred$se.fit
             lowerci <- pred$fit - confid*pred$se.fit
             polygon(c(tempx$x,rev(tempx$x)),c(lowerci,rev(upperci)),col=adjustcolor("light grey", alpha.f=0.8), border=NA)
               }
        if(ellipses) {
            xm <- mean(x,na.rm=TRUE)
            ym <- mean(y,na.rm=TRUE)
            xs <- sd(x,na.rm=TRUE)
            ys <- sd(y,na.rm=TRUE)
            r = cor(x, y,use="pairwise",method=method)
            draw.ellipse(xm,ym,xs,ys,r,col.smooth=col.lm,...)   #just draw the ellipse
                    }
             abline(lml, col = col.lm, ...)
      }
    }


     "draw.ellipse" <-  function(x=0,y=0,xs=1,ys=1,r=0,col.smooth,add=TRUE,segments=51,...) {
                         #based upon John Fox's ellipse functions
     angles <- (0:segments) * 2 * pi/segments
         unit.circle <- cbind(cos(angles), sin(angles))
       if(!is.na(r)) {
       if (abs(r)>0 )theta <- sign(r)/sqrt(2) else theta=1/sqrt(2)

     shape <- diag(c(sqrt(1+r),sqrt(1-r))) %*% matrix(c(theta,theta,-theta,theta),ncol=2,byrow=TRUE)
        ellipse <- unit.circle %*% shape
        ellipse[,1] <- ellipse[,1]*xs + x
        ellipse[,2] <- ellipse[,2]*ys + y
        if(show.points) points(x,y,pch=19,col=col.smooth,cex=1.5 )  #draw the mean
        lines(ellipse, ...)   }
     }

    "panel.ellipse" <-
    function (x, y,   pch = par("pch"),
         col.smooth = "red", ...)
     { segments=51
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1]-abs(.05*usr[1]) ,usr[2]+ abs(.05*usr[2])  , 0, 1.5) )
     xm <- mean(x,na.rm=TRUE)
      ym <- mean(y,na.rm=TRUE)
      xs <- sd(x,na.rm=TRUE)
      ys <- sd(y,na.rm=TRUE)
       r = cor(x, y,use="pairwise",method=method)
       if(jiggle) { x <- jitter(x,factor=factor)
      y <- jitter(y,factor=factor)}
      if(smoother) {smoothScatter(x,y,add=TRUE, nrpoints=0)} else {if(show.points) {points(x, y, pch = pch, ...)}}

     angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
         if(!is.na(r)) {
      if (abs(r)>0 ) theta <- sign(r)/sqrt(2) else theta=1/sqrt(2)

      shape <- diag(c(sqrt(1+r),sqrt(1-r))) %*% matrix(c(theta,theta,-theta,theta),ncol=2,byrow=TRUE)
       ellipse <- unit.circle %*% shape
       ellipse[,1] <- ellipse[,1]*xs + xm
       ellipse[,2] <- ellipse[,2]*ys + ym
       points(xm,ym,pch=19,col=col.smooth,cex=1.5 )  #draw the mean
      if(ellipses) lines(ellipse, ...)
         }
    }
    #######

     #Beginning of the main function
    ######
    #The original organization was very clunky, but has finally been cleaned up with lots of extra comments removed  (8/13/17)

    old.par <- par(no.readonly = TRUE) # save default, for resetting...
    on.exit(par(old.par))     #and when we quit the function, restore to original values


    if(missing(cex.cor)) cex.cor <- 1   #this allows us to scale the points separately from the correlations

    for(i in 1:ncol(x)) {  #treat character data as numeric
            if(is.character(x[[i]] ))  { x[[i]] <- as.numeric(as.factor(x[[i]]) )
                                colnames(x)[i] <- paste(colnames(x)[i],"*",sep="")}
               }
     n.obs <- nrow(x)
     confid <- qt(1-alpha/2,n.obs-2)   #used in finding confidence intervals for regressions and loess

     if(!lm) { #the basic default is here
               if(cor) {
                pairs(x, diag.panel = panel.hist.density, lower.panel = panel.cor
                , upper.panel = panel.smoother, pch=pch, ...)} else {
                pairs(x, diag.panel = panel.hist.density, lower.panel = panel.smoother, upper.panel = panel.smoother, pch=pch, ...)}

        } else { #lm is TRUE
            if(!cor)  { #this case does not show the correlations, but rather shows the regression lines above and below the diagonal
                        pairs(x, diag.panel = panel.hist.density, lower.panel = panel.lm, upper.panel = panel.lm, pch=pch, ...)
                      } else {  #the normal case is to show the regressions below and the rs above
                        pairs(x, diag.panel = panel.hist.density, lower.panel = panel.cor, upper.panel = panel.lm,pch=pch,  ...)

            }
                   }

    }   #end of pairs.panels
    ####************************************************************************************************
                                     ####*end*####
    ####************************************************************************************************
    my.pairs.panels(x=x, smooth=smooth, histogram=histogram, scale=scale, density=density, ellipses=ellipses, digits=digits, method=method, pch=pch, lm=lm, cor=cor, jiggle=jiggle, factor=factor, hist.col=hist.col, show.points=show.points, rug=rug, breaks=breaks, cex.cor=cex.cor, smoother=smoother, stars=stars, ci=ci, alpha=alpha, bg=bg, ...)
}


#' rescale a vector to 0-1
#' @description rescale a vector to 0-1
#' @return if x is not numeric, return original x (unchanged)
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
#' @description define a new coordinate system
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
#' @note As a reminder, the returned ggplot object can be modified post-hoc
#' @export
#' @references \href{http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html}{Erwan Le Pennec - CMAP}
ez.radarmap = function(df, id, stats="mean", lwd=1, angle=0, fontsize=0.8, facet=FALSE, facetfontsize=1, color=id, linetype=NULL,theme.apa=TRUE){
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    df.ori = df
    gghistory=paste(gghistory,'df.ori = df',sep='\n')

    # 1) summarise
    if (stats!="none") {
        cmd = sprintf('df.stats = dplyr::summarise_each(dplyr::group_by(df.ori,%s),
                      funs(%s(.,na.rm=T)))
                      ',id,stats)
        eval(parse(text = cmd))
        gghistory=paste(gghistory,cmd,sep='\n')
    } else {
        df.stats = df.ori
        gghistory=paste(gghistory,'df.stats = df.ori',sep='\n')
    }

    # 2) rescale
    df.stats = data.frame(lapply(df.stats, ez.rescale01))
    gghistory=paste(gghistory,'df.stats = data.frame(lapply(df.stats, ez.rescale01))',sep='\n')

    # 3) to long format
    cmd = sprintf('tidyr::gather(df.stats, variable,value,-%s,factor_key = T) -> df
                  ',id)
    eval(parse(text = cmd))
    gghistory=paste(gghistory,cmd,sep='\n')

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
                      facet_wrap(~%s) +
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
    gghistory=paste(gghistory,cmd,sep='\n')

    # 5) hack: couldn't pass NULL to color, linetype
    if (is.null(color)) {
        cmd = sprintf('p = p + scale_color_manual(values=rep("black",nlevels(factor(df$%s))))
                      ',id)
        if (theme.apa) cmd = paste0(cmd,'+theme_apa()')
        eval(parse(text = cmd))
    }
    if (is.null(linetype)) {
        cmd = sprintf('p = p + scale_linetype_manual(values=rep("solid",nlevels(factor(df$%s))))
                      ',id)
        if (theme.apa) cmd = paste0(cmd,'+theme_apa()')
        eval(parse(text = cmd))
    }
    gghistory=paste(gghistory,cmd,sep='\n')
    pp=p
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' visualize where all the NAs of a dataframe are
#' @description visualize where all the NAs of a dataframe are
#' @param df data frame in wide format
#' @param id a column name as id, will be shown as y axis, quoted "", eg, subject ID; if id not given, internally add row# as id
#' @param angle the x axis label angle, default=270 (vertical), suggests 330 if label is not too long
#' @param color color of missing values, eg, "red"
#' @param basesize base font size
#' @param xsize x axis label font relative size
#' @param ysize y axis label font relative size
#' @return a ggplot object (+theme_apa() to get apa format plot)
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

      if ("amelia" %in% class(obj)) {
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

      if ("amelia" %in% class(obj)) {
          if (is.null(csvar)) csvar <- obj$arguments$cs
          if (is.null(tsvar)) tsvar <- obj$arguments$ts
      }

      if (missing(y.labels)) {
        if (!is.null(csvar)) {
          if ("amelia" %in% class(obj)) {
            cs <- obj$imputations[[1]][,csvar]
          } else {
            cs <- obj[,csvar]
          }
          y.labels <- cs
          if (is.factor(y.labels)) y.labels <- levels(y.labels)[unclass(y.labels)]

          cs.names <- y.labels


          if (!is.numeric(cs)) cs <- as.numeric(as.factor(cs))
          if (!is.null(tsvar)) {
            if ("amelia" %in% class(obj)) {
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
          oldOpts = options(warn=1)
          y.labels <- suppressWarnings(row.names(obj$imputations[[1]]))
          options(oldOpts)
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
    # alternatively, guess one can also create an empty df with specified cols
    df = suppressWarnings(data.frame(apply(df,2,as.numeric)))
    df[indx.value] = 0
    df[indx.na] = 1

    cmd = sprintf('df$%s = theID',id)
    eval(parse(text = cmd))

    cmd = sprintf('p = ez.heatmap(df, "%s", colors=c("blue", "white", "%s"),
                  legend.position="none", angle=%d, basesize=%f, xsize=%f, ysize=%f)'
                  , id, color, angle, basesize, xsize, ysize)
    eval(parse(text = cmd))

    cat('\nNumber of NAs in the data frame:')
    print(NAs)
    cat('=====================Done!=====================\n')
    return(p)
}

#' scatter plot internal function
#' @description scatter plot internal function
#' @export
.scatterplot.rnp = function(df, y, x, model,...){
    # https://gist.github.com/kdauria/524eade46135f6348140
    # http://stackoverflow.com/a/7549819/2292993
    # http://stackoverflow.com/a/13451587/2292993
    result = ez.lms(df=df,y=y,x=x,model=model,covar = NULL,report=F,plot=F,view=F,...)
    if (model=="lm") {
        rvalue = result$r.spartial
        nvalue = result$n
        pvalue = result$p.spartial
    } else {
        ez.esp('
        rvalue = result$r.spartial.{model}
        nvalue = result$n.{model}
        pvalue = result$p.spartial.{model}
        ')
    }
    rvalue = ifelse(abs(rvalue)>=.005, sprintf("%.2f",rvalue), sprintf("%.2e", rvalue))
    # N/A for rlm, do not know how to calculate it yet
    if (model=='rlm') rvalue='N/A'
    if (pvalue<.001) {
        pvalue = sprintf("%.2e", pvalue)
    } else if (pvalue<.01) {
        pvalue = sprintf("%.3f", pvalue)
    } else {
        pvalue = sprintf("%.2f", pvalue)
    }
    eq <- substitute(italic(r)~"="~rvalue*","~italic(n)~"="~nvalue*","~italic(p)~"="~pvalue,list(rvalue = rvalue,nvalue = nvalue,pvalue = pvalue))
    as.character(as.expression(eq))
}

#' scatter plot internal function
#' @description scatter plot internal function
#' @export
.scatterplot.ablinemethod = function(form,data,model){
    # for latticeExtra::panel.smoother(method), it has se, proper line range
    # better than panel.abline
    # this function has to be exported, cannot be found within scatterplot function, maybe because
    # the do.call() in latticeExtra::panel.smoother
    # essentially same procedure as ez.lms
    set.seed(20190117)
    na.action=na.exclude
    if (model=="lm") m = stats::lm(form,data,na.action=na.action)
    if (model=="lmrob") m = suppressWarnings(robustbase::lmrob(form,data,control=robustbase::lmrob.control(max.it=500,maxit.scale=500),na.action=na.action))
    if (model=="lmRob") m = suppressWarnings(robust::lmRob(form,data,control=robust::lmRob.control(seed=1313,mxr=500,mxf=500,mxs=500),na.action=na.action))
    if (model=="rlm") m = suppressWarnings(MASS::rlm(form,data,maxit=500,na.action=na.action))
    return(m)
}

#' scatter plot with lattice
#' @description scatter plot with lattice
#' @param df data frame
#' @param cmd like "y~x+a+b", "y~x+a+b|z", "y~x+a+b||z", "y~x+a+b|||z"where y x are continous, z discrete (| one regression line, || multiple regression lines by levels of z--gives interaction p value), +a+b optional for covariates residualization
#' \cr y~x+a+b|||z plots separated by z groups
#' @param loess T/R adds a loess fit (uses panel.loess, the same as type = c("smooth"))
#' @param model one of c('lm', 'lmrob', 'lmRob', 'rlm'), robustbase::lmrob--MM-type Estimators; robust::lmRob--automatically chooses an appropriate algorithm. one or more, 'lm' will always be included internally, even if not specified
#' @param hack T/F if more than 1 model specified, plot all in one plot
#' @param scale when having covariates, z transform residuals
#' @param rp show r (signed) and p values
#' @param rp.size  r p values font size, ignored if rp=FALSE
#' @param rp.x  r p values x position (0-1, relative to top left, big-->right), ignored if rp=FALSE.
#' @param rp.y  r p values y position (0-1, relative to top left, big-->up), ignored if rp=FALSE.
#' @param se standard error of linear regression line
#' @param line.color only applicable when not auto varied, regression line color.
#' @param point.color only applicable when not auto varied
#' @param point.shape only applicable when not auto varied
#' @param point.alpha  if overplot for points, one can reduce alpha
#' @param point.size if less point, oen can increase size
#' @param ylab  y label. NULL
#' @param xlab  x label. NULL
#' @param x.tick.number integer, how many ticks. can also set xlim, say xlim=c(1,10) via \code{...}
#' @param zlab  z legend label/title, only applicable when there is z provided. NULL
#' @param legend.box  box of legend, T or F
#' @param legend.position  legend position 'none' (I think this is not natively supported, but works) "left", "right", "top", and "bottom"
#' \cr    c(x,y,two-element fractional numeric vector)     c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position, but within the plot box.
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,12) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param ... other parameters passed to \code{\link[lattice]{xyplot}}. eg, xlim=c(1,10), layout=c(col,row)--a bit
#' weird. lattice orders panels from the bottom up. To get what you want either use as.table = TRUE or
#' reverse the ordering in index.cond. eg, as.table=F, index.cond=list(c(3,2,1))
#' @return a lattice plot
#' @note specify auto through param colors, shapes
#' @export
ez.scatterplot = function(df,cmd,loess=FALSE,model=c('lm', 'lmrob', 'lmRob', 'rlm'),scale=FALSE,rp=TRUE,rp.x=0.025,rp.y=0.05,se=TRUE,layout=NULL,
    rp.size=14,line.width=3,point.size=14,x.axis.size=16,y.axis.size=16,x.lab.size=18,y.lab.size=18,title.size=20,legend.size=c(0,14),
    line.color='#BE1B22',line.style=1,
    loess.color='dark grey',loess.width=3,loess.style=2,
    point.color='#0086B8',point.shape=16,point.alpha=0.90,
    colors=ez.palette("Zhu"),shapes=c(16,17,15,3,7,8),
    ylab=NULL,xlab=NULL,x.tick.number=5,y.tick.number=5,
    title=NULL,
    zlab=NULL,legend.box=FALSE,legend.position='top',legend.direction="horizontal",hack=FALSE,print2scr=TRUE,...){

    if (length(model)>1 & hack){
        out = mapply(ez.scatterplot,model=model,title=model,MoreArgs=list(df=df, cmd=cmd, loess=loess, scale=scale, rp=rp, rp.size=rp.size, rp.x=rp.x, rp.y=rp.y, se=se,
                    layout=layout, line.color=line.color, line.width=line.width, line.style=line.style,
                    loess.color=loess.color, loess.width=loess.width, loess.style=loess.style, point.color=point.color,
                    point.shape=point.shape, point.alpha=point.alpha, point.size=point.size,
                    colors=colors, shapes=shapes,
                    ylab=ylab, xlab=xlab,
                    x.axis.size=x.axis.size, y.axis.size=y.axis.size, x.lab.size=x.lab.size, y.lab.size=y.lab.size,
                    x.tick.number=x.tick.number, y.tick.number=y.tick.number, title.size=title.size,
                    zlab=zlab, legend.box=legend.box, legend.position=legend.position,
                    legend.direction=legend.direction, legend.size=legend.size, hack=FALSE, print2scr=print2scr, ...),SIMPLIFY=FALSE,USE.NAMES=TRUE)
        multiplot(plotlist=out,title=title)
        return(invisible(out))
    }

    if (grepl("||||",cmd,fixed=TRUE)){
        p1 = ez.scatterplot(df,gsub("||||","|",cmd,fixed=TRUE),
            loess=loess, model=model, scale=scale, rp=rp, rp.size=rp.size, rp.x=rp.x, rp.y=rp.y, se=se,
            layout=layout, line.color=line.color, line.width=line.width, line.style=line.style,
            loess.color=loess.color, loess.width=loess.width, loess.style=loess.style, point.color=point.color,
            point.shape=point.shape, point.alpha=point.alpha, point.size=point.size, colors=colors,
            shapes=shapes, ylab=ylab, xlab=xlab, x.axis.size=x.axis.size, y.axis.size=y.axis.size,
            x.lab.size=x.lab.size, y.lab.size=y.lab.size, x.tick.number=x.tick.number,
            y.tick.number=y.tick.number, title=title, title.size=title.size, zlab=zlab, legend.box=legend.box,
            legend.position=legend.position, legend.direction=legend.direction, legend.size=legend.size,
            hack=hack, print2scr=F, ...)
        p2 = ez.scatterplot(df,gsub("||||","|||",cmd,fixed=TRUE),
            loess=loess, model=model, scale=scale, rp=rp, rp.size=rp.size, rp.x=rp.x, rp.y=rp.y, se=se,
            layout=layout, line.color=line.color, line.width=line.width, line.style=line.style,
            loess.color=loess.color, loess.width=loess.width, loess.style=loess.style, point.color=point.color,
            point.shape=point.shape, point.alpha=point.alpha, point.size=point.size, colors=colors,
            shapes=shapes, ylab=ylab, xlab=xlab, x.axis.size=x.axis.size, y.axis.size=y.axis.size,
            x.lab.size=x.lab.size, y.lab.size=y.lab.size, x.tick.number=x.tick.number,
            y.tick.number=y.tick.number, title=title, title.size=title.size, zlab=zlab, legend.box=legend.box,
            legend.position=legend.position, legend.direction=legend.direction, legend.size=legend.size,
            hack=hack, print2scr=print2scr, ...)
        # a bit ugly hack
        p1strip = paste0(p2$condlevels[[1]],collapse='+')
        pp = ez.esp('latticeExtra:::c.trellis("{p1strip}"=p1,p2,x.same=TRUE,y.same=TRUE,layout=c(dim(p1)+dim(p2),1))')
        if (!missing(layout)) pp$layout = layout
        return(pp)
    }

    if (print2scr) {ez.lms(df,cmd,report=T,model=c('lm', 'lmrob', 'lmRob', 'rlm'),view=F,plot=F,error=T)}

    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))
    bt = trellis.par.get("fontsize")$text ; bp = trellis.par.get("fontsize")$points
    rp.size = rp.size/bt                  ; point.size = point.size/bt
    title.size = title.size/bt            ; legend.size = legend.size/bt
    x.axis.size=x.axis.size/bt            ; y.axis.size=y.axis.size/bt
    x.lab.size=x.lab.size/bt              ; y.lab.size=y.lab.size/bt

    model = match.arg(model)
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=legend.size[2]}  # change default legend title size 0
    if (is.character(legend.position)) legend.position = ez.sprintf('space="{legend.position}"') else legend.position = ez.sprintf('corner=c({ez.vv(legend.position,print2scr=F)})')

    cmd = ez.trim(cmd) %>% gsub("|||","@",.,fixed=TRUE) %>% gsub("||","*",.,fixed=TRUE)
####************************************************************************************************
                              ####*covariate residualize begin*####
####************************************************************************************************
if (grepl("+",cmd,fixed=TRUE)) {
    if (grepl("|",cmd,fixed=TRUE)) {
        # y~x+a+b|z
        tmp = strsplit(cmd,"[~+|]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v})) %>%
             ez.zresidize('{x}',c({v}),model='{model}',scale={scale})
        "
        cmd = ez.sprintf('{y}~{x}|{z}')
    } else if (grepl("*",cmd,fixed=TRUE)) {
        # y~x+a+b||z
        tmp = strsplit(cmd,"[~+*]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        # grouping changes df data type which would fail lme4::lmList, ungroup and data.frame()
        # https://stackoverflow.com/a/40061201/2292993
        # group_by has no effects for ez.zresidize directly
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v})) %>%
             dplyr::group_by({z}) %>%
             dplyr::do({'{'}ez.zresidize(.,'{x}',c({v}),model='{model}',scale={scale}){'}'}) %>%
             dplyr::ungroup() %>% data.frame()
        "
        cmd = ez.sprintf('{y}~{x}*{z}')
    } else if (grepl("@",cmd,fixed=TRUE)) {
        # y~x+a+b|||z
        tmp = strsplit(cmd,"[~+@]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v})) %>%
             dplyr::group_by({z}) %>%
             dplyr::do({'{'}ez.zresidize(.,'{x}',c({v}),model='{model}',scale={scale}){'}'}) %>%
             dplyr::ungroup() %>% data.frame()
        "
        cmd = ez.sprintf('{y}~{x}@{z}')
    } else {
        # y~x+a+b
        tmp = strsplit(cmd,"[~+]")[[1]]
        y = tmp[1]; x = tmp[2]
        v = tmp[3:length(tmp)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', {v})) %>%
             ez.zresidize('{x}',c({v}),model='{model}',scale={scale})
        "
        cmd = ez.sprintf('{y}~{x}')
    }
    ez.esp(tt)
    gghistory=paste(gghistory,ez.sprintf(tt),sep='\n')
}
####************************************************************************************************
                              ####*covariate residualize end*####
####************************************************************************************************
    # let the party begin
    if (grepl("|",cmd,fixed=T)) {
        # y~x|z
        tmp = strsplit(cmd,"[~|]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2]); z = trimws(tmp[3])
        df=ez.dropna(df,c(y,x,z)) %>% ez.factorelevel(z)
        n = nlevels(df[[z]])

        if (is.null(ylab)) ylab = y ; if (is.null(xlab)) xlab = x
        if (is.null(zlab)) zlab = z ; if (is.null(title)) title = ''
        point.shape = ez.vv(shapes[1:n],print2scr=F); point.color = ez.vv(colors[1:n],print2scr=F)
        if (legend.direction=='vertical') nkeycol=1 else nkeycol = n
        # the essence is: disable as many defaults as possible, and call panel with or without panel.superpose(panel.groups))
        # pass any additional parameters to xyplot(... )
        tt = ez.sprintf('
        pp = lattice::xyplot({y}~{x}, df, grid=F, type="",
            groups = {z},
            ylab = list("{ylab}", cex={y.lab.size}, fontfamily="{RMN}"), xlab = list("{xlab}", cex={x.lab.size}, fontfamily="{RMN}"),
            main = list("{title}", cex= {title.size}, fontfamily="{RMN}"),
            par.settings = list(strip.background=list(col="#D9D9D9"),strip.border=list(col="black"),grid.pars=list(fontfamily="{RMN}")),
            par.strip.text = list(cex={legend.size[2]}, fontfamily="{RMN}"),
            scales = list(
                x = list(cex={x.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={x.tick.number}, tck=c(1,0)),
                y = list(cex={y.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={y.tick.number}, tck=c(1,0))
                ),
            key = list(
                fontfamily="{RMN}",
                text = list(levels(df[["{z}"]]), cex = {legend.size[2]}),
                border = {legend.box},
                {legend.position},
                title = "{zlab}", cex.title = {legend.size[1]},
                points = list(pch = c({point.shape}), col = c({point.color}), cex = {legend.size[2]}),
                columns = {nkeycol}),
            panel = function(x, y, ...){"{"}
            panel.xyplot(x, y, ...)
            # print(names(list(...)))   # "subscripts", "grid", "type", "groups"
            if ({rp}) grid::grid.text(ez.eval(as.expression(.scatterplot.rnp(df, "{y}", "{x}", model = "{model}"))),
                            x = unit({rp.x}, "npc"), y = unit({rp.y}, "npc"),
                            gp = grid::gpar(cex = {rp.size}, fontfamily = "{RMN}"),
                            just = c("left", "bottom"))
            # do not add ... to panel.smoother, panel.loess
            # panel.smoother ignores col.line, but accepts col
            latticeExtra::panel.smoother(x,y,method=".scatterplot.ablinemethod",model="{model}",se={se},lwd={line.width},lty={line.style},col="{line.color}")
            if ({loess}) panel.loess(x, y, col.line = "{loess.color}", lwd = {loess.width}, lty = {loess.style})

            panel.superpose(x, y, ...,
            panel.groups = function(x, y, ...){"{"}
            panel.xyplot(x, y, ...)
            # print(names(list(...)))   # a lot
            # do not add ... to panel.points
            panel.points(x, y, cex = {point.size}, alpha = {point.alpha}, pch = c({point.shape}), col = c({point.color}))
            {"}"})
            {"}"}, as.table=TRUE, ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s")) %%>%% ez.factorelevel("%s")',y,x,z,z),
               tt,sep='\n')
    } else if (grepl("*",cmd,fixed=T)) {
        # y~x||z
        tmp = strsplit(cmd,"[~*]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2]); z = trimws(tmp[3])
        df=ez.dropna(df,c(y,x,z)) %>% ez.factorelevel(z)
        n = nlevels(df[[z]])

        if (is.null(ylab)) ylab = y ; if (is.null(xlab)) xlab = x
        if (is.null(zlab)) zlab = z ; if (is.null(title)) title = ''
        point.shape = ez.vv(shapes[1:n],print2scr=F); point.color = ez.vv(colors[1:n],print2scr=F)
        if (legend.direction=='vertical') nkeycol=1 else nkeycol = n
        # the essence is: disable as many defaults as possible, and call panel with or without panel.superpose(panel.groups))
        # pass any additional parameters to xyplot(... )
        tt = ez.sprintf('
        pp = lattice::xyplot({y}~{x}, df, grid=F, type="",
            groups = {z}, col = c({point.color}),
            ylab = list("{ylab}", cex={y.lab.size}, fontfamily="{RMN}"), xlab = list("{xlab}", cex={x.lab.size}, fontfamily="{RMN}"),
            main = list("{title}", cex= {title.size}, fontfamily="{RMN}"),
            par.settings = list(strip.background=list(col="#D9D9D9"),strip.border=list(col="black"),grid.pars=list(fontfamily="{RMN}")),
            par.strip.text = list(cex={legend.size[2]}, fontfamily="{RMN}"),
            scales = list(
                x = list(cex={x.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={x.tick.number}, tck=c(1,0)),
                y = list(cex={y.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={y.tick.number}, tck=c(1,0))
                ),
            key = list(
                fontfamily="{RMN}",
                text = list(levels(df[["{z}"]]), cex = {legend.size[2]}),
                border = {legend.box},
                {legend.position},
                title = "{zlab}", cex.title = {legend.size[1]},
                points = list(pch = c({point.shape}), col = c({point.color}), cex = {legend.size[2]}),
                columns = {nkeycol}),
            panel = function(x, y, ...,model=model){"{"}
            # cross groups
            panel.xyplot(x, y, ...)
            if ({rp}) grid::grid.text(ez.eval(as.expression(.scatterplot.rnp(df, "{y}", "{x}", model = "{model}"))),
                            x = unit({rp.x}, "npc"), y = unit({rp.y}, "npc"),
                            gp = grid::gpar(cex = {rp.size}, fontfamily = "{RMN}"),
                            just = c("left", "bottom"))
            latticeExtra::panel.smoother(x,y,method=".scatterplot.ablinemethod",model="{model}",se={se},lwd={line.width},lty={line.style},col="{line.color}")
            if ({loess}) panel.loess(x, y, col.line = "{loess.color}", lwd = {loess.width}, lty = {loess.style})

            # separate groups
            panel.superpose(x, y, ...,
            # confusingly, specify col within panel.groups(), col magically appear and brought from col above
            # other para could be group.number
            panel.groups = function(x, y, ..., col){"{"}
            panel.xyplot(x, y, ...)
            latticeExtra::panel.smoother(x,y,method=".scatterplot.ablinemethod",model="{model}",se={se},col.se=col,col=col,lwd={line.width},lty={line.style})
            if ({loess}) panel.loess(x, y, col.line = col, lwd = {loess.width}, lty = {loess.style})
            panel.points(x, y, cex = {point.size}, alpha = {point.alpha}, pch = c({point.shape}), col = c({point.color}))
            {"}"})
            {"}"}, as.table=TRUE, ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s")) %%>%% ez.factorelevel("%s")',y,x,z,z),
               tt,sep='\n')
    } else if (grepl("@",cmd,fixed=T)) {
        # y~x|||z
        tmp = strsplit(cmd,"[~@]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2]); z = trimws(tmp[3])
        df=ez.dropna(df,c(y,x,z)) %>% ez.factorelevel(z)
        n = nlevels(df[[z]])

        if (is.null(ylab)) ylab = y ; if (is.null(xlab)) xlab = x
        if (is.null(zlab)) zlab = z ; if (is.null(title)) title = ''
        if (is.null(layout)) layout = c(n,1);
        point.shape = ez.vv(shapes[1:n],print2scr=F); point.color = ez.vv(colors[1:n],print2scr=F)
        layout = ez.vv(layout,print2scr=F);
        if (legend.direction=='vertical') nkeycol=1 else nkeycol = n
        # the essence is: disable as many defaults as possible, and call panel with or without panel.superpose(panel.groups))
        # pass any additional parameters to xyplot(... )
        tt = ez.sprintf('
        pp = lattice::xyplot({y}~{x}|{z}, df, grid=F, type="p",
            groups = {z},pch = c({point.shape}), col = c({point.color}),
            ylab = list("{ylab}", cex={y.lab.size}, fontfamily="{RMN}"), xlab = list("{xlab}", cex={x.lab.size}, fontfamily="{RMN}"),
            main = list("{title}", cex= {title.size}, fontfamily="{RMN}"),
            par.settings = list(strip.background=list(col="#D9D9D9"),strip.border=list(col="black"),grid.pars=list(fontfamily="{RMN}")),
            par.strip.text = list(cex={legend.size[2]}, fontfamily="{RMN}"),
            scales = list(
                x = list(cex={x.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={x.tick.number}, tck=c(1,0)),
                y = list(cex={y.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={y.tick.number}, tck=c(1,0))
                ),
            key = list(
                fontfamily="{RMN}",
                text = list(levels(df[["{z}"]]), cex = {legend.size[2]}),
                border = {legend.box},
                {legend.position},
                title = "{zlab}", cex.title = {legend.size[1]},
                points = list(pch = c({point.shape}), col = c({point.color}), cex = {legend.size[2]}),
                columns = {nkeycol}),
            panel = function(x, y, ...,model=model){"{"}
            panel.superpose(x, y, ...,
            panel.groups = function(x, y, ..., col){"{"}
            panel.xyplot(x, y, ...)
            if ({rp}) grid::grid.text(ez.eval(as.expression(.scatterplot.rnp(data.frame({x}=x,{y}=y), "{y}", "{x}", model = "{model}"))),
                            x = unit({rp.x}, "npc"), y = unit({rp.y}, "npc"),
                            gp = grid::gpar(cex = {rp.size}, fontfamily = "{RMN}"),
                            just = c("left", "bottom"))
            latticeExtra::panel.smoother(x,y,method=".scatterplot.ablinemethod",model="{model}",se={se},lwd={line.width},lty={line.style},col=col)
            if ({loess}) panel.loess(x, y, col.line = col, lwd = {loess.width}, lty = {loess.style})
            # print(names(list(...)))   # a lot
            # do not add ... to panel.points
            #panel.points(x, y, cex = {point.size}, alpha = {point.alpha}, pch = c({point.shape}), col = c({point.color}))
            {"}"})
            {"}"}, as.table=TRUE, layout=c({layout}), ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s")) %%>%% ez.factorelevel("%s")',y,x,z,z),
               tt,sep='\n')
    } else {
        # y~x
        tmp = strsplit(cmd,"[~]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2])
        df=ez.dropna(df,c(y,x))

        if (is.null(ylab)) ylab = y ; if (is.null(xlab)) xlab = x
        if (is.null(zlab)) zlab = '' ; if (is.null(title)) title = ''
        # the essence is: disable as many defaults as possible, and call panel with or without panel.superpose(panel.groups))
        # pass any additional parameters to xyplot(... )
        tt = ez.sprintf('
        pp = lattice::xyplot({y}~{x}, df, grid=F, type="",
            ylab = list("{ylab}", cex={y.lab.size}, fontfamily="{RMN}"), xlab = list("{xlab}", cex={x.lab.size}, fontfamily="{RMN}"),
            main = list("{title}", cex= {title.size}, fontfamily="{RMN}"),
            par.settings = list(strip.background=list(col="#D9D9D9"),strip.border=list(col="black"),grid.pars=list(fontfamily="{RMN}")),
            par.strip.text = list(cex={legend.size[2]}, fontfamily="{RMN}"),
            scales = list(
                x = list(cex={x.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={x.tick.number}, tck=c(1,0)),
                y = list(cex={y.axis.size}, fontfamily="{RMN}", rot=0, alternating=1, tick.number={y.tick.number}, tck=c(1,0))
                ),
            panel = function(x, y, ...){"{"}
            panel.xyplot(x, y, ...)
            # print(names(list(...)))   # "subscripts", "grid", "type", "groups"
            if ({rp}) grid::grid.text(ez.eval(as.expression(.scatterplot.rnp(df, "{y}", "{x}", model = "{model}"))),
                            x = unit({rp.x}, "npc"), y = unit({rp.y}, "npc"),
                            gp = grid::gpar(cex = {rp.size}, fontfamily = "{RMN}"),
                            just = c("left", "bottom"))
            # do not add ... to panel.smoother, panel.loess
            # panel.smoother ignores col.line, but accepts col
            latticeExtra::panel.smoother(x,y,method=".scatterplot.ablinemethod",model="{model}",se={se},lwd={line.width},lty={line.style},col="{line.color}")
            if ({loess}) panel.loess(x, y, col.line = "{loess.color}", lwd = {loess.width}, lty = {loess.style})
            panel.points(x, y, cex = {point.size}, alpha = {point.alpha}, pch = {point.shape}, col = "{point.color}")
            {"}"}, as.table=TRUE, ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s","%s"))',y,x),
                   tt,sep='\n')
    } # end final else
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' scatter plot with ggplot
#' @description scatter plot with ggplot
#' @param df data frame
#' @param cmd like "y~x+a+b", "y~x+a+b|z", "y~x+a+b||z", "y~x+a+b|||z"where y x are continous, z discrete (| one regression line, || multiple regression lines by levels of z--gives interaction p value), +a+b optional for covariates residualization
#' \cr y~x+a+b|||z an invisible plotlist returned, call ggmultiplot(plotlist)
#' @param line.color only applicable when y~x and y~x|z (ie, not auto varied with aes()), regression line color
#' @param point.color only applicable when y~x (ie, not auto varied with aes()). for auto ones, use scale_*_*
#' @param point.shape only applicable when y~x (ie, not auto varied with aes()). for auto ones, use scale_*_*
#' @param point.alpha  if overplot for points, reduce alpha
#' @param point.size if less point, increase size
#' @param rug.size rug size
#' @param rp.size  r p values font size, ignored if rp=FALSE
#' @param rp.x  r p values x position (0-1, relative to top left, big-->right), ignored if rp=FALSE. Internally, convert to geom_label(x,y) where x,y refers to actually x,y axis value
#' @param rp.y  r p values y position (0-1, relative to top left, big-->bottom), ignored if rp=FALSE
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param rp show r (signed) and p values
#' @param se standard error of linear regression line
#' @param rug marginal rug indicating univariate distribution
#' @param ellipse draw confidence ellipses, powered by stat_ellipse()
#' @return a ggplot object (+theme_apa() to get apa format plot)
#' @export
ez.scatterplot2 = function(df,cmd,rp.size=5,rp.x=0.25,rp.y=0.99,colors=ez.palette("Zhu"),shapes=c(16,17,15,3,7,8),line.color='#BE1B22',point.color='#0086B8',point.shape=16,point.alpha=0.95,point.size=3,rug.size=0.25,ylab=NULL,xlab=NULL,zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),rp=TRUE,se=TRUE,rug=FALSE,ellipse=FALSE,theme.apa=TRUE){
####************************************************************************************************
                                     ####*recursive call*####
####************************************************************************************************
if (grepl("|||",cmd,fixed=TRUE)) {
    tmp = strsplit(cmd,"|||",fixed=T)[[1]]
    cmd = tmp[1]; z = tmp[2]
    lvls =  levels(df[[z]])
    pplist = list()


    for (i in 1:length(lvls)) {
        dftmp = 'df %>% dplyr::filter({z} %in% "{lvls[i]}")' %>% ez.esp()
        pplist[[i]] = ez.scatterplot2(df = dftmp,
            cmd=cmd, line.color=colors[i], point.color=colors[i], point.shape=shapes[i],
            rp.size=rp.size, rp.x=rp.x, rp.y=rp.y, point.alpha=point.alpha, point.size=point.size, rug.size=rug.size, ylab=ylab, xlab=xlab, zlab=zlab, legend.position=legend.position, legend.direction=legend.direction, legend.box=legend.box, legend.size=legend.size, rp=rp, se=se, rug=rug, ellipse=ellipse)
    }
    ez.pprint('an invisible plotlist returned, call ggmultiplot(plotlist)')
    return(invisible(pplist))
}
####************************************************************************************************
                                     ####**####
####************************************************************************************************
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))
    cmd = ez.trim(cmd)
    # play a trick
    cmd = gsub("||","*",cmd,fixed=TRUE)

    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')

    tt = '
        ####################################################### subfunction
        # https://gist.github.com/kdauria/524eade46135f6348140
        # http://stackoverflow.com/a/7549819/2292993
        # http://stackoverflow.com/a/13451587/2292993
        lmrp = function(m) {
            nvalue = m$df.residual + 2
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

            eq <- substitute(italic(r)~"="~rvalue*","~italic(n)~"="~nvalue*","~italic(p)~"="~pvalue,list(rvalue = rvalue,nvalue = nvalue,pvalue = pvalue))
            as.character(as.expression(eq));
        }

        # lmList: Fit a list of lm objects with a common model for different subgroups of the data
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
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    gghistory=paste(gghistory,tt,sep='\n')

####************************************************************************************************
                              ####*covariate residualize begin*####
####************************************************************************************************
if (grepl("+",cmd,fixed=TRUE)) {
    if (grepl("|",cmd,fixed=TRUE)) {
        tmp = strsplit(cmd,"[~+|]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)];
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v}))
        df %<>% ez.zresidize('{x}',c({v}),model='lm',scale=TRUE)
        "
        cmd = ez.sprintf('{y}~{x}|{z}')
    } else if (grepl("*",cmd,fixed=TRUE)) {
        tmp = strsplit(cmd,"[~+*]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)];
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v}))
        df %<>% dplyr::group_by({z}) %>%
            dplyr::do({'{'}ez.zresidize(.,'{x}',c({v}),model='lm',scale=TRUE){'}'}) %>%
            dplyr::ungroup() %>% data.frame()
        "
        cmd = ez.sprintf('{y}~{x}*{z}')
    } else {
        tmp = strsplit(cmd,"[~+]")[[1]]
        y = tmp[1]; x = tmp[2];
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', {v}))
        df %<>% ez.zresidize('{x}',c({v}),model='lm',scale=TRUE)
        "
        cmd = ez.sprintf('{y}~{x}')
    }
    ez.esp(tt)
    gghistory=paste(gghistory,ez.sprintf(tt),sep='\n')
}
####************************************************************************************************
                              ####*covariate residualize end*####
####************************************************************************************************

    if (grepl("|",cmd,fixed=TRUE)) {
      cmd = strsplit(cmd,"[~|]")[[1]]
      # y~x|z
      yy = trimws(cmd[1])
      xx = trimws(cmd[2])
      zz = trimws(cmd[3])
      df=ez.dropna(df,c(yy,xx,zz))

      rp.x = min(df[[xx]]) + (max(df[[xx]])-min(df[[xx]]))*rp.x
      rp.y = max(df[[yy]]) + (min(df[[yy]])-max(df[[yy]]))*rp.y
      # http://stackoverflow.com/a/27959418/2292993
      rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp(lm(%s ~ %s, df))), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx),'')
      se = ifelse(se,'TRUE','FALSE')
      rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f,aes(color=%s)) +',rug.size,zz),'')
      ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm",aes(color=%s)) +',zz),'')
      tt = sprintf('
                  pp=ggplot(df, aes(x=%s, y=%s)) +
                  geom_point(alpha=%f,size=%f,aes(color=%s,shape=%s)) + %s
                  geom_smooth(method=lm,se=%s,color="%s") + %s %s
                  scale_color_manual(values=colors) + scale_shape_manual(values=shapes) +
                  %s %s %s %s
                  theme(legend.direction="%s") +
                  theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                  ,xx,yy,point.alpha,point.size,zz,zz,rp,se,line.color,rug,ellipse,ylab,xlab,zlab,legend.position,legend.direction,legend.size[1],legend.size[2],legend.size[2]
      )
      gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s"))',yy,xx,zz),
               tt,sep='\n')

    } else {
      cmd = strsplit(cmd,"[~*]")[[1]]
      if (length(cmd)==2) {
      # y~x
      yy = trimws(cmd[1])
      xx = trimws(cmd[2])
      df=ez.dropna(df,c(yy,xx))
      rp.x = min(df[[xx]]) + (max(df[[xx]])-min(df[[xx]]))*rp.x
      rp.y = max(df[[yy]]) + (min(df[[yy]])-max(df[[yy]]))*rp.y
      # http://stackoverflow.com/a/27959418/2292993
      rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp(lm(%s ~ %s, df))), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx),'')
      se = ifelse(se,'TRUE','FALSE')
      rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f) +',rug.size),'')
      ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm") +'),'')
      # legend is ignored, but because lab might be empty, better to keep the legend commands here
      tt = sprintf('
                  pp=ggplot(df, aes(x=%s, y=%s)) +
                  geom_point(alpha=%f,size=%f,color="%s",shape=%d) + %s
                  geom_smooth(method=lm,se=%s,color="%s") + %s %s
                  %s %s %s
                  theme(legend.direction="%s") +
                  theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                  ,xx,yy,point.alpha,point.size,point.color,point.shape,rp,se,line.color,rug,ellipse,ylab,xlab,'theme(legend.position="none")',legend.direction,legend.size[1],legend.size[2],legend.size[2]
      )
      gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s"))',yy,xx),
               tt,sep='\n')
      } else {
          # y~x||z
          yy = trimws(cmd[1])
          xx = trimws(cmd[2])
          zz = trimws(cmd[3])
          df=ez.dropna(df,c(yy,xx,zz))
          rp.x = min(df[[xx]]) + (max(df[[xx]])-min(df[[xx]]))*rp.x
          rp.y = max(df[[yy]]) + (min(df[[yy]])-max(df[[yy]]))*rp.y
          rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp2("%s","%s","%s",df)), parse = TRUE)+',rp.size,rp.x,rp.y,yy,xx,zz),'')
          se = ifelse(se,'TRUE','FALSE')
          rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f,aes(color=%s)) +',rug.size,zz),'')
          ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm",aes(color=%s)) +',zz),'')
          tt = sprintf('
                      pp=ggplot(df, aes(x=%s, y=%s)) +
                      geom_point(alpha=%f,size=%f,aes(color=%s,shape=%s)) + %s
                      geom_smooth(method=lm,se=%s,aes(color=%s)) + %s %s
                      scale_color_manual(values=colors) + scale_shape_manual(values=shapes) +
                      %s %s %s %s
                      theme(legend.direction="%s") +
                      theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                      ,xx,yy,point.alpha,point.size,zz,zz,rp,se,zz,rug,ellipse,ylab,xlab,zlab,legend.position,legend.direction,legend.size[1],legend.size[2],legend.size[2]
          )
          gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s","%s","%s"))',yy,xx,zz),
                   tt,sep='\n')
      }
    }
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' plot count data
#' @description plot count data, eg, ez.countplot(iris, 'Species'). See also \code{\link{ez.piechart}} \code{\link{ez.hist}}
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd like "x", c("x1", "x2", "x3"), "x|z", "x|z a" where x z a are all discrete.
#' @param position so far can only be "stack", "fill", "both". ('dodge' not supported yet)
#' @param n.size set to 0 to hide count/percentage
#' @param n.type 1 = n for stack, pct for fill; 2 = pct for stack, n for fill; 3 = n (pct) for stack, pct (n) for fill; 4 = pct (n) for stack, n (pct) for fill
#' @param alpha bar alpha value
#' @param color  "bw" or "color"  black/white or colorblind-friendly color
#' @param width  the width of bar/bin itself
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @param facet  one of 'cols', 'rows', 'wrap'
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.countplot = function(df,cmd,position='both',color='color',colors=ez.palette("Zhu"),alpha=1,n.size=5.5,n.type=3,width=0.7,ylab=NULL,xlab=NULL,zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),xangle=0,vjust=NULL,hjust=NULL,facet='cols',theme.apa=TRUE) {
    df.bak=df
    if (position=='both') {
        p1=ez.countplot(df,cmd,'stack',color, alpha, n.size, n.type, width, ylab, xlab, zlab, legend.position, legend.direction, legend.box, legend.size, xangle, vjust, hjust)
        p2=ez.countplot(df,cmd,'fill',color, alpha, n.size, n.type, width, ylab, xlab, zlab, legend.position, legend.direction, legend.box, legend.size, xangle, vjust, hjust)
        return(ggmultiplot(p1,p2,cols=1))
    }

    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))

    color = ifelse(color=='bw','scale_fill_grey(start=0,end=0.8)','scale_fill_manual(values=colors)')
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))
    n.type.stack = c('n.str','pct.str','n.pct.str','pct.n.str')[n.type]
    n.type.fill = c('pct.str','n.str','pct.n.str','n.pct.str')[n.type]

    cmd = ez.trim(cmd)
    # c("x1", "x2", "x3")
    if (length(cmd)>1) cmd=paste0(cmd,collapse='/')
    cmd = strsplit(cmd,'[~|]')[[1]]
    # xx
    if (length(cmd)==1) {
        xx = cmd[1]
        xx = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xx, perl=TRUE)
        xx = strsplit(xx,"/",fixed=TRUE)[[1]]
        df = ez.dropna(df, xx)
        dfdf = df %>% tidyr::gather_("theKey","theValue",xx)
        # first compute pos
        dfdf = dfdf %>% dplyr::count_(c("theKey","theValue")) %>% dplyr::group_by_("theKey") %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct)
        # then compute n/pct without groupby (only 1 factor out there)
        dfdf = dfdf %>% dplyr::mutate(pct=n/sum(dfdf[["n"]]),pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))
        if (position=='stack') {
            if (is.null(ylab)) ylab='Count'
            ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
            xlab = ifelse(xlab=='','xlab("")+',xlab)
            # http://ggplot2.tidyverse.org/articles/releases/ggplot2-2.2.0.html#stacking-bars
            tt = sprintf('
            pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                         geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                         %s + %s %s %s %s %s
                         ggtitle(paste0("N = ",nrow(df))) +
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") +
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                         geom_text(color="white", size=%f, aes(label=%s,y=pct.pos))'
                         , 'theKey','theValue', position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.stack
            )
        } else if (position=='fill') {
            if (is.null(ylab)) ylab='Percentage'
            ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
            xlab = ifelse(xlab=='','xlab("")+',xlab)
            tt = sprintf('
            pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                         geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                         %s + %s %s %s %s %s
                         ggtitle(paste0("N = ",nrow(df))) +
                         theme(axis.text.x=element_text(angle=%f %s %s)) +
                         theme(legend.direction="%s") +
                         theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                         geom_text(color="white", size=%f, aes(label=%s,y=n.pos))+
                         scale_y_continuous(labels=scales::percent)+coord_flip()'
                         , 'theKey','theValue', position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.fill
            )
        }
        gghistory=paste(gghistory,
                 sprintf('xx=c(%s)',ez.vv(xx,print2scr=FALSE)),
                 sprintf('df=ez.dropna(df,xx)'),
                 'dfdf = df %>% tidyr::gather_("theKey","theValue",xx)',
                 '# first compute pos
                 dfdf = dfdf %>% dplyr::count_(c("theKey","theValue")) %>% dplyr::group_by_("theKey") %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct)
                 # then compute n/pct without groupby (only 1 factor out there)
                 dfdf = dfdf %>% dplyr::mutate(pct=n/sum(dfdf[["n"]]),pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))',
                 tt,sep='\n')
    # xx|zz or xx|zz aa
    } else {
        xx = cmd[1]
        zz = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        zz = strsplit(zz," ",fixed=TRUE)[[1]]
        # xx|zz
        if (length(zz)==1) {
            zz = zz[1]
            df=ez.dropna(df,c(xx,zz))
            pvalue='N.A.'
            tryCatch({
                    # only available for two factors
                    pvalue=fisher.test(ez.2factor(df[[xx]]),ez.2factor(df[[zz]]))$p.value
                    if (pvalue<.001) {
                        pvalue = sprintf("%.2e", pvalue)
                    } else if (pvalue<.01) {
                        pvalue = sprintf("%.3f", pvalue)
                    } else {
                        pvalue = sprintf("%.2f", pvalue)
                    }
            }, error = function(e) {})
            dfdf = df %>% dplyr::count_(c(xx,zz)) %>% dplyr::group_by_(xx) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))
            if (position=='stack') {
                if (is.null(ylab)) ylab='Count'
                ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
                tt = sprintf('
                pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                             geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                             %s + %s %s %s %s %s
                             ggtitle(paste0("N = ",nrow(df),", p = %s (Fisher)")) +
                             theme(axis.text.x=element_text(angle=%f %s %s)) +
                             theme(legend.direction="%s") +
                             theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                             geom_text(color="white", size=%f, aes(label=%s,y=pct.pos))'
                             , xx, zz, position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, pvalue, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.stack
                )
            } else if (position=='fill') {
                if (is.null(ylab)) ylab='Percentage'
                ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
                tt = sprintf('
                pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                             geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                             %s + %s %s %s %s %s
                             ggtitle(paste0("N = ",nrow(df),", p = %s (Fisher)")) +
                             theme(axis.text.x=element_text(angle=%f %s %s)) +
                             theme(legend.direction="%s") +
                             theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                             geom_text(color="white", size=%f, aes(label=%s,y=n.pos))+
                             scale_y_continuous(labels=scales::percent)+coord_flip()'
                             , xx, zz, position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, pvalue, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.fill
                )
            }
            gghistory=paste(gghistory,
                     sprintf('df=ez.dropna(df,c("%s","%s"))',xx,zz),
                     sprintf('xx="%s"',xx),
                     sprintf('zz="%s"',zz),
                     'dfdf = df %>% dplyr::count_(c(xx,zz)) %>% dplyr::group_by_(xx) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))',
                     tt,sep='\n')
        # xx|zz aa
        } else {
            if (length(zz)==2) {
                aa = zz[2]
                zz = zz[1]
                df=ez.dropna(df,c(xx,zz,aa))
                # grouping by aa (facet) then xx, notice group_by_(c(aa,xx)) is equal to group_by_(aa). count_(xx,zz,aa) gives error!
                dfdf = df %>% dplyr::count_(c(xx,zz,aa)) %>% dplyr::group_by_(aa,xx) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))
                if (position=='stack') {
                    if (is.null(ylab)) ylab='Count'
                    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
                    tt = sprintf('
                    pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                                 geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                                 %s + %s %s %s %s %s
                                 ggtitle(paste0("N = ",nrow(df))) +
                                 theme(axis.text.x=element_text(angle=%f %s %s)) +
                                 theme(legend.direction="%s") +
                                 theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                                 geom_text(color="white", size=%f, aes(label=%s,y=pct.pos))+%s'
                                 , xx, zz, position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.stack, sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),aa)
                    )
                } else if (position=='fill') {
                    if (is.null(ylab)) ylab='Percentage'
                    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
                    tt = sprintf('
                    pp = ggplot2::ggplot(dfdf, aes(x=%s,n,fill=%s)) +
                                 geom_bar(position=position_%s(reverse=TRUE),stat="identity",alpha=%f,width=%f) +
                                 %s + %s %s %s %s %s
                                 ggtitle(paste0("N = ",nrow(df))) +
                                 theme(axis.text.x=element_text(angle=%f %s %s)) +
                                 theme(legend.direction="%s") +
                                 theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                                 geom_text(color="white", size=%f, aes(label=%s,y=n.pos))+%s+
                                 scale_y_continuous(labels=scales::percent)+coord_flip()'
                                 , xx, zz, position, alpha, width, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.fill, sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),aa)
                    )
                }
            gghistory=paste(gghistory,
                     sprintf('df=ez.dropna(df,c("%s","%s","%s"))',xx,zz,aa),
                     sprintf('xx="%s"',xx),
                     sprintf('zz="%s"',zz),
                     sprintf('aa="%s"',aa),
                     'dfdf = df %>% dplyr::count_(c(xx,zz,aa)) %>% dplyr::group_by_(aa,xx) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))',
                     tt,sep='\n')
            }
        }
    }
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' pie chart
#' @description pie chart
#' @param df data frame in long format (but be careful that standard error might be inaccurate depending on grouping in the long format)
#' @param cmd a single col name
#' @param start   offset of starting point from 12 o'clock in radians eg, pi/3
#' @param direction   1, clockwise; -1, anticlockwise
#' @param n.size set to 0 to hide count/percentage
#' @param n.type 1 = pct; 2 = n; 3 = pct (n); 4 = n (pct)
#' @param alpha pie alpha value
#' @param color  "bw" or "color"  black/white or colorblind-friendly color
#' @param ylab  y label
#' @param xlab  x label
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.piechart = function(df,cmd,start=0,direction=1,color='color',colors=ez.palette("Zhu"),alpha=1,n.size=5.5,n.type=3,ylab='',xlab='',zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),xangle=0,vjust=NULL,hjust=NULL,theme.apa=TRUE) {
    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))

    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))

    color = ifelse(color=='bw','scale_fill_grey(start=0,end=0.8)','scale_fill_manual(values=colors)')
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))
    n.type.fill = c('pct.str','n.str','pct.n.str','n.pct.str')[n.type]

    cmd = ez.trim(cmd)
    xx = cmd
    # note that xylab internally twised because of coord_polar()
    if (ylab=='') ylab=xx
    df = ez.dropna(df, xx)
    dfdf = df %>% dplyr::count_(c(xx)) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))
    # https://github.com/tidyverse/ggplot2/issues/2058
    # https://github.com/tidyverse/ggplot2/issues/2242
    # axis.text seems to have a bug. so always use:
    # axis.line.x, axis.line.y
    # axis.ticks.x, axis.ticks.y
    # axis.text.x, axis.text.y
    # axis.title.x, axis.title.y
    tt = sprintf('
    pp = ggplot2::ggplot(dfdf, aes(x="",n,fill=%s)) +
                 geom_bar(position="fill",stat="identity",alpha=%f,width=1) +
                 %s + xlab("%s")+ylab("%s") + %s %s %s
                 ggtitle(paste0("N = ",nrow(df))) +
                 theme(axis.text.x=element_text(angle=%f %s %s)) +
                 theme(legend.direction="%s") +
                 theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                 geom_text(color="white", size=%f, aes(label=%s,y=n.pos))+
                 coord_polar(theta="y",start=%f,direction=%f)+
                 theme_apa()+
                 theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
                       axis.line.x=element_blank(),axis.line.y=element_blank(),
                       axis.text.x=element_blank(),axis.text.y=element_blank(),
                       panel.background=element_rect(fill="white",color="white"))'
                 , xx, alpha, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], n.size, n.type.fill, start, direction
    )
    gghistory=paste(gghistory,
         sprintf('df=ez.dropna(df,c("%s"))',xx),
         sprintf('xx="%s"',xx),
         'dfdf = df %>% dplyr::count_(c(xx)) %>% dplyr::mutate(pct=n/sum(n),pct.pos=cumsum(n)-0.5*n,n.pos=cumsum(pct)-0.5*pct,pct.str=sprintf("%0.1f%%",pct*100),n.str=sprintf("(%d)",n),n.pct.str=sprintf("%d (%0.1f%%)",n,pct*100),pct.n.str=sprintf("%0.1f%% (%d)",pct*100,n))',
         tt,sep='\n')
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}

#' plot continous data
#' @description plot continous data
#' @param x df or vector, if vector cmd ignored
#' @param cmd like "x, x|z, x|z a" where x z a are all discrete
#' @param xline draw red line(s) on x axis, eg, 2.5 or c(2.5,4). NULL=mean(df[[xx]])
#' @param alpha bar alpha value
#' @param color  "bw" or "color"  black/white or colorblind-friendly color
#' @param density when true ignore bins, plot density (sort of smoothed histogram); when false plot normal histogram
#' @param bins  number of bins (alternatively pass bidwidth that depends on the scale of variable)
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z/a/fill/legend label, only applicable when there is z provided NULL
#' @param legend.position  legend position 'top', 'bottom', 'left', 'right', 'none', c(x,y,two-element numeric vector)
#' \cr         c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
#' \cr         if no z/a (legend) provided, auto force to 'none'
#' @param legend.box  box of legend, T or F
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @param xangle  angle of x text 0
#' @param vjust  vjust of x text NULL
#' @param hjust  hjust of x text NULL
#' @param facet  one of 'cols', 'rows', 'wrap'
#' @param print.cmd T/F
#' @return a ggplot object (+theme_apa() to get apa format plot), +scale_y_continuous(limits=c(-5,8),breaks=seq(-5,8,by=2),oob=scales::rescale_none)
#' \cr see http://stackoverflow.com/a/31437048/2292993 for discussion
#' @export
ez.hist = function(x,cmd,bins=60,density=FALSE,xline=NULL,color='color',colors=ez.palette("Zhu"),alpha=0.5,ylab=NULL,xlab=NULL,zlab=NULL,legend.position='top',legend.direction="horizontal",legend.box=T,legend.size=c(0,10),xangle=0,vjust=NULL,hjust=NULL,facet='cols',theme.apa=TRUE,...) {
    if (!is.data.frame(x)) {
        var=deparse(substitute(x))
        dfcmd=sprintf('df=data.frame("%s"=x)',var)
        ez.eval(dfcmd)
        gghistory=dfcmd
        cmd=colnames(df)
    } else {
        df=x
        gghistory='df=.'
    }
    df.bak=df
    # https://stackoverflow.com/a/25215323/2292993
    # call options(warn=1) to set the global warn (opt is alway global, even change inside a function) to 1, but returns the old value to oldWarn
    # finally on exit the function, set it back to old value
    oldOpts = options(warn=1)
    on.exit(options(oldOpts))

    color = ifelse(color=='bw','scale_fill_grey(start=0,end=0.8)','scale_fill_manual(values=colors)')
    xlab = ifelse(is.null(xlab),'',sprintf('xlab("%s")+',xlab))
    ylab = ifelse(is.null(ylab),'',sprintf('ylab("%s")+',ylab))
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    zlab = ifelse(is.null(zlab),'',sprintf('labs(fill="%s")+',zlab))
    legend.position = ifelse(is.character(legend.position), sprintf('theme(legend.position="%s")+',legend.position), sprintf('theme(legend.position=c(%s))+',paste(legend.position,collapse=',')))
    legend.box = ifelse(legend.box,'theme(legend.background = element_rect(color = "black"))+','')
    vjust = ifelse(is.null(vjust),'',sprintf(',vjust=%f',vjust))
    hjust = ifelse(is.null(hjust),'',sprintf(',hjust=%f',hjust))
    hist.type=ifelse(!density, sprintf('geom_histogram(position="stack",stat="bin",alpha=%f,bins=%d,...)',alpha,bins), sprintf('geom_density(stat = "density",position = "identity",alpha=%f,...)',alpha))

    cmd = ez.trim(cmd)
    cmd = strsplit(cmd,'[~|]')[[1]]
    # xx
    if (length(cmd)==1) {
        xx = cmd[1]
        df = ez.dropna(df, xx)
        if (is.null(xline)) xline=mean(df[[xx]])
        vline=''
        for (xl in xline) {
            vline=paste0(vline,sprintf('geom_vline(aes(xintercept=%f),color="red")+geom_text(x=%f,y=0,label="%.2f")+',xl,xl,xl))
        }
        tt = sprintf('
            pp = ggplot2::ggplot(df, aes(x=%s)) +
                     %s +
                     %s + %s %s %s %s %s
                     ggtitle(paste0("N = ",nrow(df),", M = ",sprintf("%%.2f",mean(df[["%s"]])))) +
                     theme(axis.text.x=element_text(angle=%f %s %s)) +
                     theme(legend.direction="%s") +
                     theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f)) +
                     %s theme(legend.position="None")'
                     , xx, hist.type, color, ylab, xlab, zlab, legend.position, legend.box, xx, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], vline
        )
        gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s"))',xx),
                   tt,sep='\n')
    # xx|zz or xx|zz aa
    } else {
        xx = cmd[1]
        zz = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd[2], perl=TRUE)
        zz = strsplit(zz," ",fixed=TRUE)[[1]]
        # xx|zz
        if (length(zz)==1) {
            zz = zz[1]
            df=ez.dropna(df,c(xx,zz))
            tt = sprintf('
                pp = ggplot2::ggplot(df, aes(x=%s,fill=%s)) +
                     %s +
                     %s + %s %s %s %s %s
                     ggtitle(paste0("N = ",nrow(df))) +
                     theme(axis.text.x=element_text(angle=%f %s %s)) +
                     theme(legend.direction="%s") +
                     theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                     , xx, zz, hist.type, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2]
            )
            gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s","%s"))',xx,zz),
                   tt,sep='\n')
        # xx|zz aa
        } else {
            if (length(zz)==2) {
                aa = zz[2]
                zz = zz[1]
                df=ez.dropna(df,c(xx,zz,aa))
                tt = sprintf('
                    pp = ggplot2::ggplot(df, aes(x=%s,fill=%s)) +
                     %s +
                     %s + %s %s %s %s %s
                     ggtitle(paste0("N = ",nrow(df))) +
                     theme(axis.text.x=element_text(angle=%f %s %s)) +
                     theme(legend.direction="%s") +
                     theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))+
                     %s'
                     , xx, zz, hist.type, color, ylab, xlab, zlab, legend.position, legend.box, xangle, vjust, hjust, legend.direction, legend.size[1], legend.size[2], legend.size[2], sprintf(ifelse(facet=="cols","facet_grid(.~%s)",ifelse(facet=="rows","facet_grid(%s~.)","facet_wrap(~%s)")),aa)
                )
                gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s","%s","%s"))',xx,zz,aa),
                   tt,sep='\n')
            }
        }
    }
    if (theme.apa) tt = paste0(tt,'+theme_apa()')
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df.bak
    return(pp)
}
