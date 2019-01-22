
#' scatter plot internal function
#' @description scatter plot internal function
#' @export
.scatterplot.rnp = function(...){
    # https://gist.github.com/kdauria/524eade46135f6348140
    # http://stackoverflow.com/a/7549819/2292993
    # http://stackoverflow.com/a/13451587/2292993
    result = ez.lms(...,report=F,plot=F,view=F)
    if (model=="lm") {
        rvalue = result$r.residized
        nvalue = result$n
        pvalue = result$p.residized
    } else {
        ez.esp('
        rvalue = result$r.residized.{model}
        nvalue = result$n.{model}
        pvalue = result$p.residized.{model}
        '}
    }
    rvalue = ifelse(abs(rvalue)>=.005, sprintf("%.2f",rvalue), sprintf("%.2e", rvalue))
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
#' @param rp show r (signed) and p values
#' @param rp.size  r p values font size, ignored if rp=FALSE
#' @param rp.x  r p values x position (0-1, relative to top left, big-->right), ignored if rp=FALSE.
#' @param rp.y  r p values y position (0-1, relative to top left, big-->up), ignored if rp=FALSE.
#' @param se standard error of linear regression line
#' @param line.color only applicable when not auto varied, regression line color
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
#' @param ... other parameters passed to \code{\link[lattice]{xyplot}}. eg, xlim=c(1,10), layout=c(col,row)
#' @return a lattice plot
#' @export
ez.scatterplot = function(df,cmd,loess=TRUE,model=c('lm', 'lmrob', 'lmRob', 'rlm'),scale=TRUE,rp=TRUE,rp.size=14,rp.x=0.025,rp.y=0.025,se=TRUE,layout=NULL,
    line.color='#BE1B22',line.width=3,line.style=1,
    loess.color='dark grey',loess.width=3,loess.style=2,
    point.color='#0086B8',point.shape=16,point.alpha=0.90,point.size=14,
    ylab=NULL,xlab=NULL,x.axis.size=16,y.axis.size=16,x.lab.size=18,y.lab.size=18,x.tick.number=5,y.tick.number=5,
    title=NULL,title.size=20,
    zlab=NULL,legend.box=FALSE,legend.position='top',legend.direction="horizontal",legend.size=c(0,14),...){

    df.bak=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))
    shapes = rep(c(16,17,15,3,7,8),100)   ; colors = rep(ez.palette("Zhu"),100)
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
        # grouping changes df data type which would fail lme4::lmList below
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v})) %>%
             dplyr::group_by({z}) %>%
             ez.zresidize('{x}',c({v}),model='{model}',scale={scale}) %>%
             dplyr::ungroup() %>% data.frame()
        "
        cmd = ez.sprintf('{y}~{x}*{z}')
    } else if (grepl("@",cmd,fixed=TRUE)) {
        # y~x+a+b|||z
        tmp = strsplit(cmd,"[~+@]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]
        v = tmp[3:(length(tmp)-1)]; v = ez.vv(v,print2scr=F)
        # grouping changes df data type which would fail lme4::lmList below
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v})) %>%
             dplyr::group_by({z}) %>%
             ez.zresidize('{x}',c({v}),model='{model}',scale={scale}) %>%
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
        df=ez.dropna(df,c(y,x,z))
        n = nlevels(as.factor(df[[z]]))

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
            {"}"}, ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s"))',y,x,z),
               tt,sep='\n')
    } else if (grepl("*",cmd,fixed=T)) {
        # y~x||z
        tmp = strsplit(cmd,"[~*]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2]); z = trimws(tmp[3])
        df=ez.dropna(df,c(y,x,z))
        n = nlevels(as.factor(df[[z]]))

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
            panel = function(x, y, ...){"{"}
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
            {"}"}, ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s"))',y,x,z),
               tt,sep='\n')
    } else if (grepl("@",cmd,fixed=T)) {
        # y~x|||z
        tmp = strsplit(cmd,"[~@]")[[1]]
        y = trimws(tmp[1]); x = trimws(tmp[2]); z = trimws(tmp[3])
        df=ez.dropna(df,c(y,x,z))
        n = nlevels(as.factor(df[[z]]))

        if (is.null(ylab)) ylab = y ; if (is.null(xlab)) xlab = x
        if (is.null(zlab)) zlab = z ; if (is.null(title)) title = ''
        if (is.null(layout)) layout = c(1,n);
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
            panel = function(x, y, ...){"{"}
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
            {"}"}, layout=c({layout}), ...
        )
        ') # end sprintf
        gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s"))',y,x,z),
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
            {"}"}, ...
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
