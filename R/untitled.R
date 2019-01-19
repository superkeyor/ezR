#' scatter plot with lattice
#' @description scatter plot with lattice
#' @param df data frame
#' @param cmd like "y~x+a+b", "y~x+a+b|z", "y~x+a+b||z", "y~x+a+b|||z"where y x are continous, z discrete (| one regression line, || multiple regression lines by levels of z--gives interaction p value), +a+b optional for covariates residualization
#' \cr y~x+a+b|||z plots separated by z groups
#' @param type smooth for fit, r for regression line, p for points
#' @param se standard error of linear regression line
#' @param line.color only applicable when y~x and y~x|z (ie, not auto varied), regression line color
#' @param point.color only applicable when y~x (ie, not auto varied)
#' @param point.shape only applicable when y~x (ie, not auto varied)
#' @param point.alpha  if overplot for points, one can reduce alpha
#' @param point.size if less point, oen can increase size
#' @param rp show r (signed) and p values
#' @param rp.size  r p values font size, ignored if rp=FALSE
#' @param rp.x  r p values x position (0-1, relative to top left, big-->right), ignored if rp=FALSE. 
#' @param rp.y  r p values y position (0-1, relative to top left, big-->up), ignored if rp=FALSE.
#' @param ylab  y label NULL
#' @param xlab  x label NULL
#' @param zlab  z legend label, only applicable when there is z provided NULL
#' @param legend.box  box of legend, T or F
#' @param legend.position  legend position 'none' (I think this is not natively supported, but works) "left", "right", "top", and "bottom"
#' \cr    c(x,y,two-element fractional numeric vector)     c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position, but within the plot box.
#' @param legend.direction  horizontal or vertical
#' @param legend.size c(0,10) the first number 0 controls the legend title, 0=hide; the second number controls legend.key.size, legend.text
#' @return a lattice plot
#' @export
ez.scatterplot = function(df,cmd,type=c("r","p"),rp=TRUE,rp.size=10,rp.x=0.025,rp.y=0.025,se=TRUE,line.color='#BE1B22',line.width=3,line.style=1,point.color='#0086B8',point.shape=16,point.alpha=0.95,point.size=3,title=NULL,title.size=18,ylab=NULL,xlab=NULL,x.axis.size=16,y.axis.size=16,x.lab.size=18,y.lab.size=18,zlab=NULL,legend.box=TRUE,legend.position='top',legend.direction="horizontal",legend.size=c(0,10)){
    df__copy=df
    gghistory=sprintf('df=%s',deparse(substitute(df)))
    cmd = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=TRUE)
    # play a trick
    cmd = gsub("|||","@",cmd,fixed=TRUE)
    cmd = gsub("||","*",cmd,fixed=TRUE)
    shapes = rep(c(16,17,15,3,7,8),100)
    colors = rep(c("#B3DE69","#80B1D3","#BC80BD","#FFED6F","#FB8072"),100)
    base.tx.size = trellis.par.get("fontsize")$text
    base.pt.size = trellis.par.get("fontsize")$points
    rp.size = rp.size/base.tx.size
    point.size = point.size/base.pt.size
    legend.size = legend.size/base.tx.size
    title.size = title.size/base.pt.size
    x.axis.size=x.axis.size/base.tx.size; 
    y.axis.size=y.axis.size/base.tx.size; 
    x.lab.size=x.lab.size/base.tx.size; 
    y.lab.size=y.lab.size

    tt = ez.sprintf('
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
        ####################################################### subfunction
        ',.open="<<",".close"=">>")
    eval(parse(text = tt))
    gghistory=paste(gghistory,tt,sep='\n')

    type = ez.vv(type,print2screen=F)
    if (is.null(ylab)) ylab = '' else ylab = ez.esp('list("{ylab}", cex={y.lab.size}, fontfamily="{RMN}")')
    if (is.null(xlab)) xlab = '' else xlab = ez.esp('list("{xlab}", cex={x.lab.size}, fontfamily="{RMN}")')
    if ((!is.null(zlab)) && legend.size[1]==0) {legend.size[1]=10}  # change default legend title size 0
    if (is.null(zlab)) zlab = ''
    if (is.null(title)) title = ''
    if (is.character(legend.position)) legend.position = ez.sprintf('space="{legend.position}"') else legend.position = ez.sprintf('corner={legend.position}')

####************************************************************************************************
                              ####*covariate residualize begin*####
####************************************************************************************************
if (grepl("+",cmd,fixed=TRUE)) {
    if (grepl("|",cmd,fixed=TRUE)) {
        tmp = strsplit(cmd,"[~+|]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]; v = tmp[3:(length(tmp)-1)]
        v1 = ez.vv(v,print2screen=F); v2=paste0(v,collapse='+')
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v1}))
        df %<>% dplyr::mutate({y} = ez.zresid( lm({y}~{v2}, data=df) ))
        "
        cmd = ez.sprintf('{y}~{x}|{z}')
    } else if (grepl("*",cmd,fixed=TRUE)) {
        tmp = strsplit(cmd,"[~+*]")[[1]]
        y = tmp[1]; x = tmp[2]; z = tmp[length(tmp)]; v = tmp[3:(length(tmp)-1)]
        v1 = ez.vv(v,print2screen=F); v2=paste0(v,collapse='+')
        # grouping changes df data type which would fail lme4::lmList below
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', '{z}', {v1}))
        df %<>% dplyr::group_by({z}) %>%
            dplyr::mutate({y} = ez.zresid( lm({y}~{v2}) )) %>%
            dplyr::ungroup() %>% data.frame()
        "
        cmd = ez.sprintf('{y}~{x}*{z}')
    } else {
        tmp = strsplit(cmd,"[~+]")[[1]]
        y = tmp[1]; x = tmp[2]; v = tmp[3:length(tmp)]
        v1 = ez.vv(v,print2screen=F); v2=paste0(v,collapse='+')
        tt = "
        df = ez.dropna(df,c('{y}', '{x}', {v1}))
        df %<>% dplyr::mutate({y} = ez.zresid( lm({y}~{v2}, data=df) ))
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
      tmp = strsplit(cmd,"[~|]")[[1]]
      # y~x|z
      y = trimws(tmp[1])
      x = trimws(tmp[2])
      z = trimws(tmp[3])
      df=ez.dropna(df,c(y,x,z))
      n = nlevels(as.factor(df[[z]]))

      point.shape = ez.vv(shapes[1:n],print2screen=F); point.color = ez.vv(colors[1:n],print2screen=F); line.color = point.color
      if (legend.direction=='vertical') nkeycol=1 else nkeycol = n
      tt = ez.sprintf('
        lattice::xyplot({y}~{x}, df, type = c({type}), cex = {point.size}, alpha = {point.alpha}, grid=F, 
               groups = {z}, pch=c({point.shape}), col = c({point.color}), col.line = c({line.color}), lwd={line.width}, lty={line.style},
               ylab="{ylab}", 
               xlab="{xlab}", 
               main = list("{title}", cex= {title.size}, fontfamily="{RMN}" ),
               scales = list(
                   x=list( cex={x.axis.size}, fontfamily="{RMN}" ),
                   y=list( cex={y.axis.size}, fontfamily="{RMN}" )
                   ),
               key=list(
                text=list(levels(df[["{z}"]]),cex={legend.size[2]}),
                {legend.position},
                title = "{zlab}", cex.title = {legend.size[1]},
                points=list(pch=c({point.shape}),col=c({point.color}),cex={legend.size[2]}),
                columns = {nkeycol}),
                panel = function(x,y,...){"{"}
                panel.xyplot(x,y,...)
                if ({rp}){"{"}
                grid::grid.text(ez.eval(as.expression(lmrp(lm({y} ~ {x}, df)))), 
                                x=unit({rp.x}, "npc"), y=unit({rp.y}, "npc"), 
                                gp = grid::gpar(cex = {rp.size}, fontfamily="{RMN}"),
                                just = c("left", "bottom"))
                {"}"}
               {"}"}
        )
      ') # end sprintf
      gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s","%s"))',y,x,z),
               tt,sep='\n')

    } else if (grepl("*",cmd,fixed=TRUE)) {
      tmp = strsplit(cmd,"[~*]")[[1]]
      # y~x||z
      y = trimws(tmp[1])
      x = trimws(tmp[2])
      z = trimws(tmp[3])
      df=ez.dropna(df,c(y,x,z))
      n = nlevels(as.factor(df[[z]]))

      point.shape = ez.vv(shapes[1:n],print2screen=F); point.color = ez.vv(colors[1:n],print2screen=F); line.color = point.color
      if (legend.direction=='vertical') nkeycol=1 else nkeycol = n
      tt = ez.sprintf('
        lattice::xyplot({y}~{x}, df, type = c({type}), cex = {point.size}, alpha = {point.alpha}, grid=F, 
               groups = {z}, pch=c({point.shape}), col = c({point.color}), col.line = c({line.color}), lwd={line.width}, lty={line.style},
               ylab="{ylab}", 
               xlab="{xlab}", 
               main = list("{title}", cex= {title.size}, fontfamily="{RMN}" ),
               scales = list(
                   x=list( cex={x.axis.size}, fontfamily="{RMN}" ),
                   y=list( cex={y.axis.size}, fontfamily="{RMN}" )
                   ),
               key=list(
                text=list(levels(df[["{z}"]]),cex={legend.size[2]}),
                {legend.position},
                title = "{zlab}", cex.title = {legend.size[1]},
                points=list(pch=c({point.shape}),col=c({point.color}),cex={legend.size[2]}),
                columns = {nkeycol}),
                panel = function(x,y,...){"{"}
                panel.xyplot(x,y,...)
                if ({rp}){"{"}
                grid::grid.text(ez.eval(as.expression(lmrp(lm({y} ~ {x}, df)))), 
                                x=unit({rp.x}, "npc"), y=unit({rp.y}, "npc"), 
                                gp = grid::gpar(cex = {rp.size}, fontfamily="{RMN}"),
                                just = c("left", "bottom"))
                {"}"}
               {"}"}
        )
      ') # end sprintf
      gghistory=paste(gghistory,
               sprintf('df=ez.dropna(df,c("%s","%s"))',y,x),
               tt,sep='\n')
      } else {
          # y~x||z
          y = trimws(cmd[1])
          x = trimws(cmd[2])
          z = trimws(cmd[3])
          df=ez.dropna(df,c(y,x,z))
          rp.x = min(df[[x]]) + (max(df[[x]])-min(df[[x]]))*rp.x
          rp.y = max(df[[y]]) + (min(df[[y]])-max(df[[y]]))*rp.y
          rp = ifelse(rp,sprintf('geom_label(family = RMN,size=%f,aes(x = %f, y = %f, label = lmrp2("%s","%s","%s",df)), parse = TRUE)+',rp.size,rp.x,rp.y,y,x,z),'')
          se = ifelse(se,'TRUE','FALSE')
          rug = ifelse(rug,sprintf('geom_rug(sides ="tr",position="jitter",size=%f,aes(color=%s)) +',rug.size,z),'')
          ellipse = ifelse(ellipse,sprintf('stat_ellipse(type = "norm",aes(color=%s)) +',z),'')
          tt = sprintf('
                      pp=ggplot(df, aes(x=%s, y=%s)) +
                      geom_point(alpha=%f,size=%f,aes(color=%s,shape=%s)) + %s
                      geom_smooth(method=lm,se=%s,aes(color=%s)) + %s %s
                      scale_color_manual(values=rep(c("#B3DE69","#80B1D3","#BC80BD","#FFED6F","#FB8072"),100)) +
                      %s %s %s %s
                      theme(legend.direction="%s") +
                      theme(legend.title=element_text(size=%f,face ="bold")) + theme(legend.key.size=unit(%f,"pt")) + theme(legend.text=element_text(size=%f))'
                      ,x,y,point.alpha,point.size,z,z,rp,se,z,rug,ellipse,ylab,xlab,zlab,legend.position,legend.direction,legend.size[1],legend.size[2],legend.size[2]
          )
          gghistory=paste(gghistory,
                   sprintf('df=ez.dropna(df,c("%s","%s","%s"))',y,x,z),
                   tt,sep='\n')
      }
    }
    eval(parse(text = tt))
    pp$gghistory=paste0(gghistory,'\nprint(pp)')
    pp$df=df__copy
    return(pp)
}
