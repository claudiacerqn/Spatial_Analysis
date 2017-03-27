### Electoral competition map
### Quadrant map
### High-low map - multivariate cluster map
### Kernel Density Map (Hot-Spot Map)
### LISA map
### Proportional symbol map


##### Function colmap() #####

# colmap() - establishes the color divisions, palettes and breaks to maps and other graphical representations. It uses basically RColorBrewer to provide color schemes, but user-defined palettes can be passed to the function by the argument custcol=.

# Parameters:
#             val - variable to be use to classify cases
#             classint - classification method to be used. The possible values are: "unique", "user", 
#                         "unclassed", "sd","quantile","equal","kmeans","jenks"
#             palet - the RColorBrewer palette to be used in the color classification
#             userbrk - the user defined break points in the variable when classint="user".
#             nbrks - number of breaks to divide cases
#             invpalet - logical - determines whether to invert the order of colors in the palet or 
#                       not. This is specially usefull when some palets start with darker colors, as
#                       Greys, or are ordered in a contra-intuitive order, as in some divergent palets
#                       like "RdYlBu", "RdYlGn", "RdGn","RdBu","PuOr","PRGn" or "BrBG".
#             custcol - list of custom colors to be used in the classification.



colmap <- function(val, classint="quantile", palette="Blues", userbrk=NULL, nbrks=5, invpalet=F,custcol=NULL, transp=99, na.col=NULL){
  
  require(RColorBrewer)
  require(classInt)
  
  if(is.null(na.col)) val[is.na(val)] <- 0
  
  
  if (! is.null(custcol)){ 
    palette <- custcol
  }else{  
    pal <- palette
    palette <- brewer.pal(9, palette)
    if (pal=="Greys") palette <- palette[1:length(palette)-1]
    if (invpalet==T) palette <- palette[length(palette):1]
  }
  
  col <- colorRampPalette(palette)
  
  if (classint=="unclassed"){
    groups <- 0
    colors <- col(length(val))
    palette <- colors
    colors <- colors[findInterval(val,sort(val), all.inside=T)]
    pct.cases <- 0
  }
  
  
  if (classint=="unique"){
    groups
    groups <- sort(unique(val))
    
    if(is.character(val)) val <- as.factor(val)
    if(is.factor(val)) val <- as.numeric(val)
    if (length(groups)>length(custcol)) colors <- col(length(groups)) else colors <- custcol
    palette <- unique(colors)
    colors <- colors[findInterval(val,sort(as.numeric(unique(val))), all.inside=F)]
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
  }
  
  
  
  if (classint=="user"){
    if (is.null(userbrk)) stop("El mapa de tipo 'User' requiere que los intervalos sean providos por el usuario!")
    
    groups <- userbrk
    colors <- col(length(userbrk)-1)
    palette <- unique(colors)
    colors <- colors[findInterval(val,userbrk, all.inside=T)] 
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
    
  }
  
  if (classint %in% c("sd","quantile","equal","kmeans","jenks","pretty")){
    
    if (classint=="jenks"){
      if (length(val)>900){groups <- classIntervals(var=sample(val,120), n=nbrks, style=classint)
      }else {groups <- classIntervals(var=val, n=nbrks, style=classint)}
    }       
    if (classint!="jenks") groups <- classIntervals(var=val, n=nbrks, style=classint)
    
    groups <- unique(groups[[2]])
    colors <- col(length(groups)-1)
    palette <- unique(colors)
    colors <- colors[findInterval(val,groups, all.inside=T)] 
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
    
  }
  
  if(length(colors[is.na(colors)])>0) colors[is.na(colors)] <- na.col
  
  
  if (transp!=99){
    colors <- paste0(colors, transp, sep="")
    palette <- paste0(palette, transp)}
  
  return(list(colors=colors, groups=groups, palette=palette, pct.cases=pct.cases))
  
}  




adleg <- function(legpos="bottomleft",leg.main="", cex.leg=1.4, pt.cex=NULL, y.intersp=0.8, x.intersp=0.5, pch=19, classint="quantile", val=NULL, palette=NULL,userbrk=NULL,groups=NULL,dec=1, plot.pct=FALSE, pct.cases=NULL, cl="SpatialPolygonsDataFrame", big.mark=".", decimal.mark=",",horiz=F, add=T, symbol.bg="red", symbol.fg="black", leg.col="black", na.col=NULL, na.lab="n.a."){
  
  require(maptools)
  
  par(mar=rep(0,4))
  
  if (is.numeric(legpos)) legpos <- cbind(legpos[1], legpos[2])
  
  if (classint=="unclassed"){
    
    # Open the library fields required to the progressive color bar
    require(fields)
    limites <- c(min(val,na.rm=T),max(val, na.rm=T))
    image.plot(legend.only=T, zlim=limites, col= sort(unique(palette),decreasing=T), smallplot=c(0.05,0.08,0.05,0.5))
  }  
  
  
  if (classint!="unclassed"){
    
    if (classint=="user"& is.null(userbrk)) stop("O mapa de tipo 'user' exige que os intervalos sejam fornecidos pelo usuário!")
    
    if (classint=="unique"){
      if (is.factor(val)) legs <- levels(val) else legs <- as.character(groups)}
    
    if (classint !="unique") legs <- leglabs(format(round(groups, dec), big.mark=big.mark, decimal.mark=decimal.mark), under="<", over=">")
    
    if (plot.pct==TRUE)legs <- paste(legs,paste("(", format(round(pct.cases, dec), big.mark=big.mark, decimal.mark=decimal.mark), "%)", sep=""), sep=" ")
    
    if(add==F) plot(1, col="transparent", axes=F)
    
    if (! is.null(na.col)) {
      legs <-  c(na.lab,legs)
      palette <- c(na.col, palette) 
    }
    
    if (cl%in% c("SpatialPolygons","SpatialPolygonsDataFrame")){
      legend(legpos,legend=legs,fill=palette, bg="transparent", bty="n", cex=cex.leg, y.intersp=y.intersp, x.intersp=x.intersp, title=leg.main, title.adj=0.15, horiz = horiz, text.col = leg.col)}
    else if (cl%in% c("SpatialPoints","SpatialPointsDataFrame")){
      legend(legpos,legend=legs, pt.bg = symbol.bg, border=symbol.fg, bg="transparent", bty="n", pch=pch, pt.cex=pt.cex, cex=cex.leg, y.intersp=y.intersp, x.intersp=x.intersp, title=leg.main, title.adj=0.15, horiz = horiz, text.col = leg.col)
    }
    
  }
  
}



# Mapa de competencia electoral


compmap <- function(spobj, vara_t0, vara_t1, varb_t0,varb_t1, lang="es", leg=c("Party A","Party B"), border="transparent", frame=NULL, leg.pos="bottomleft",leg.cex=1.2, y.intersp=0.8, palette="Spectral", inv.pal=F){
  
  require(RColorBrewer)
  
  pal <- brewer.pal(6, palette)
  
  if (inv.pal==TRUE) pal <- pal[length(pal):1]

  adif <- vara_t1-vara_t0
  bdif <- varb_t1-varb_t0
  
  ganat0 <- 0
  ganat0[vara_t0>varb_t0] <- 1
  ganat0[is.na(ganat0)] <- 0
  
  ganat1 <- 0
  ganat1[vara_t1>varb_t1] <- 1
  ganat1[is.na(ganat1)] <- 0
  
  dif <- NA
  dif[adif>0 & ganat0==1 & ganat1==1] <- 1 
  dif[adif<0 & ganat0==1 & ganat1==1] <- 2 
  dif[ganat0==0 & ganat1==1] <- 3 
  dif[ganat0==1 & ganat1==0] <- 4 
  dif[bdif<0 & ganat0==0 & ganat1==0] <- 5 
  dif[bdif>0 & ganat0==0 & ganat1==0] <- 6 
  
  ta <- table(c(1,2,3,4,5,6))
  tb <-table(dif)
  ta[! names(ta) %in% names(tb)] <- 0
  ta[ names(ta) %in% names(tb)] <- tb
  
  tbp <- round(ta/sum(ta)*100,1)
  
  if (lang=="en") lgval <- c(" , > margin "," , < margin ", " to ")
  if (lang=="es") lgval <- c(" , > margen "," , < margen ", " a ")
  if (lang=="pt") lgval <- c(" , > margem "," , < margem ", " a ")
  
  legs <- c(paste0(leg[1], lgval[1]),paste0(leg[1], lgval[2]),paste0(leg[2],lgval[3],leg[1]),paste0(leg[1],lgval[3],leg[2]),paste0(leg[2], lgval[2]),paste0(leg[2],lgval[1]))
  
  legs <- paste(legs," (",tbp,"%)", sep="")
  
  cols <- pal[findInterval(dif,sort(unique(dif)))]
  
  par(mar=rep(0,4))
  plot(spobj, border=border, col=cols)
  if(! is.null(frame)) plot(frame, add=T)
  
  legend(leg.pos, legend=legs, fill=pal, bty="n", y.intersp=y.intersp, cex=leg.cex)
  
  
}




### Quadrant map #####

# Creates the map of quadrants of a bi-dimensional plot (scatterplot).

# Parameters:
#             spobj - spatial object to be used as base to create the map
#             x - a continuous variable with the same length as spobj
#             y - a continuous variable different from x with the same length as spobj
#             frame - spatial object used to frame the areas from spobj (optional).
#             border - border color of spobj. The default is "black".
#             zlim - the lower threshold value in standard deviations (z-value) to include an 
#                   observation as a cluster. If, for example, zlim=1, it will be necessary that both #                   x and y have  z-value higher than 1 in order to be considered  high-high cluster.
#             lang - language of the legend. options: en - english, es - spanish, pt - portuguese.
#             palette - the color palette compose of four basic colors to be used in identifying each category. The default is "royalblue","red2","#FFFF99","#7FC97F".
#             lwd - line width of the frame layer
#             leg.pos - legend position
#             cor.pos - position of the correlation coeficient to be displayed in the map

qdmap <- function(spobj, x, y, frame=NULL, border="black", zlim=0, lang="en", cex.leg=1, palet=c("royalblue","red2","#FFFF99","#7FC97F"), lwd=1, lwd.frame=2, leg.pos="bottomleft", cor.pos= c(-41.71177, -31.07674),y.intersp=0.7, x.intersp=0.4, selqad=1, add=F, ret.col=F){
  
  zx <- scale(x)
  zy <- scale(y)
  
  zlima <- -1*zlim
  
  if (selqad==2) palet[3:4] <- "transparent"
  if (selqad==3) palet[1:2] <- "transparent"
  
  
  spobj$color <- "transparent"
  spobj$color[zx<=zlima & zy<=zlima] <- palet[1]
  spobj$color[zx>zlim & zy>zlim] <- palet[2]
  spobj$color[zx<=zlima & zy>zlim] <- palet[3]
  spobj$color[zx>zlim & zy<=zlima] <- palet[4]
  
  table(spobj$color)
  
  spobj$count <- NA
  spobj$count[zx<=zlima & zy<=zlima] <- 1
  spobj$count[zx>zlim & zy>zlim] <- 2
  spobj$count[zx<=zlima & zy>zlim] <- 3
  spobj$count[zx>zlim & zy<=zlima] <- 4
  
  sum(table(spobj$count))
  
  #   tot <- length(spobj$count[! is.na(spobj$count)])   
  tot <-   sum(table(spobj$count))
  tb <-   table(spobj$count)
  
  pbb <- round(tb[1]/tot*100,1)
  paa <- round(tb[2]/tot*100,1)
  pba <- round(tb[3]/tot*100,1)
  pab <- round(tb[4]/tot*100,1)
  
  par(mar=c(0.1,0.1,0.1,0.1))
  plot(spobj, col=spobj$color, border=border, lwd=lwd, add=add)
  
  if (lang=="en") desc <- c("Low-Low","High-High","Low-High","High-Low","Quadrant: % of cases","Threshold=")
  if (lang=="es") desc <- c("Bajo-Bajo","Alto-Alto","Bajo-Alto","Alto-Bajo","Cuadrante: % de casos","Umbral=")
  if (lang=="pt") desc <- c("Baixo-Baixo","Alto-Alto","Baixo-Alto","Alto-Baixo", "Quadrante: % de casos","Limite=")
  
  if (selqad==1){
    leg <- c(paste0(desc[1],": ", pbb, "%"),paste0(desc[2],": ", paa, "%"),paste0(desc[3],": ", pba, "%"),paste0(desc[4],": ", pab, "%"))} 
  
  if (selqad==2){
    leg <- c(paste0(desc[1],": ", pbb, "%"),paste0(desc[2],": ", paa, "%"))
    palet <- palet[1:2]} 
  
  if (selqad==3){
    leg <- c(paste0(desc[3],": ", pba, "%"),paste0(desc[4],": ", pab, "%"))
    palet <- palet[3:4] }
  
  
  if(! is.null(frame)) plot(frame, add=T, lwd=lwd.frame)
  
  if (! is.numeric(leg.pos)) legend(leg.pos, legend=leg, fill=palet, bty="n", cex=cex.leg, title=desc[5], title.adj=0.15, y.intersp=y.intersp, x.intersp=x.intersp)
  
  if (is.numeric(leg.pos))legend(leg.pos[1], leg.pos[2], legend=leg, fill=palet, bty="n", cex=cex.leg, title=desc[5], title.adj=0.15, y.intersp=y.intersp, x.intersp=x.intersp)
  
  
  if (cex.leg>1.3) cex.r <- cex.leg else cex.r <- 1.3
  
  if (selqad==1) cora <- round(cor( x, y,method="spearman", use="complete.obs"),3)
  if (selqad==2) cora <- round(cor(x[spobj$count%in%c(1,2)], y[spobj$count%in%c(1,2)],method="spearman", use="complete.obs"),3)
  if (selqad==3) cora <- round(cor(x[spobj$count%in%c(3,4)], y[spobj$count%in%c(3,4)],method="spearman", use="complete.obs"),3)
  
  text(cor.pos[1], cor.pos[2], labels=paste("r=", cora, sep=" "), cex=cex.r)
  
  #  if (zlim>0) text(cor.pos[1], (cor.pos[2]+cor.pos[2]*0.1), labels=paste(desc[6], zlim, "s.d.", sep=" "), cex=cex.r-0.3)
  
  if(ret.col==T) return(list(colors=spobj$color, zx=zx, zy=zy))
  
  
}




#### High-low map - multivariate cluster map ####

# Using a numeric matrix as base, this function constructs a map of clustered high and low values. It is useful to create a multivariate representation of complex fenomena when different dimensions, assumed to vary in the same direction are clustered and should be represented together (find examples).

# Parameters:
#             spobj - spatial object to be represented.
#             data - numeric data frame used in the calculations.
#             frame - spatial object used to frame the basic map.
#             border - border color of the basic map. The default is "black".
#             zlim - limit, in Z-value to color spatial units. The default is 0.
#             lang - language (english -"en", spanish - "es", portuguese - "pt"). The default is "en".
#             cex.leg - character expantion factor of the legend. The default is 1.
#             lwd.frame - width of the border of the frame object. The default is 2.
#             legpos - position of the legend in the map. The default is "bottomleft".
#             palette - color palette used to represent high and low values. The default is "red" for high values and "blue" for low ones.

hilomap <- function(spobj, data, frame=NULL, border="black", zlim=0, lang="en", cex.leg=1, lwd.frame=2, legpos="bottomleft", palette=c("red","blue"),...){
  
  x <- scale(data)
  
  mathigh <- x>zlim
  matlow  <- x<(-1*zlim)

  idhigh <- rowSums(mathigh)
  idlow <- rowSums(matlow)
  
  coler <- "transparent"
  coler[idhigh==ncol(x)] <- palette[1]
  coler[idlow==ncol(x)] <- palette[2]
  
  if (lang=="en") groups <- c("High","Low")
  if (lang=="es") groups <- c("Alto","Bajo")
  if (lang=="pt") groups <- c("Alto","Baixo")
  
  plot(spobj, col=coler, border=border)
  if (! is.null(frame)) plot(frame, lwd=lwd.frame, add=T)  
  
  adleg(legpos = legpos,cex.leg = cex.leg, palette = palette, groups = groups, classint = "unique",...)
  
}



##### Kernel Map #####
# Performs a kernel density map of a point object

# Parameters:
#             sppoint - spatial point object used in the calculation
#             spwindow - spatial polygon object used as window to calculate the kernel
#             frame -
#             plot -
#             return -
#             res -
#             alpha - 
#             diggle -
#             edge - 

kernelmap <- function(sppoint=NULL, spwindow=NULL, frame=NULL, plot=T, return=F, res=480, alpha=0.5, diggle=T, edge=T, title=NULL, nwhite=20, border.col="gray70", transp=NULL, backcol="#FFFFFF", legpos="bottomleft", leg.cex=1.2, draw.leg=T, add=F, legs=c("High","Low")){
  
  # load the package spatsat
  require(spatstat)
  
  
  # Set the checking option off in order to create the window object
  # spatstat.options(checkpolygons=FALSE)
  
  # Generates the window which will frame the results
  spowin <- as.owin.SpatialPolygons(spwindow)
  
  # Set the checking option ON after creating the window object
  # spatstat.options(checkpolygons=TRUE)
  
  # Retrieve the coordinates of airports
  coords <- coordinates(sppoint)
  
  #Generate the point object used to create the density map
  ptobj <- as.ppp(coords, W=spowin)
  
  # Calc. the density map - it can take some time because the grid resolution is medium-high (dimyx=480)
  dens <- density.ppp(ptobj, alpha, diggle=diggle, edge=edge, dimyx=res)
  
  
  if (plot==T & ! is.null(frame)){
    
    
    # Define the color palette
    colores <- heat.colors(100)
    
    #Plot the base map
    #     par(mfrow=c(1,1))
    
    if (! is.null(title)){
      par(mar=c(0,0,2,0)) 
      plot(frame, border=border.col, col=backcol, bg=backcol, add=add)
      title(title)
    }else{
      par(mar=rep(0,4))
      plot(frame, border=border.col, col=backcol, bg=backcol, add=add)}
    
    colo <- colores[100:1]
    colo[1:nwhite] <- backcol
    
    if(! is.null(transp)) colo <- paste0(colo,transp)
    
    # Plot the results
    plot(dens, col=colo, add=TRUE)
    
    # Superpose the frame object as a contextual frame
    plot(frame, border=border.col, add=TRUE)
  }
  
  #   image.plot(legend.only=T, zlim=c(0,1), col= heat.colors(100)[100:1], smallplot=c(0.05,0.08,0.05,0.25),axis.args=list(at=c(0,1), labels=c("Baja","Alta")))
  
  
  if (draw.leg==T) legend(legpos, legend = c(legs[1],rep("",6),legs[2]), fill = heat.colors(8), y.intersp = 0.5, border = "transparent", cex=leg.cex, bty = "n")
  
  
  if(return==T)  return(dens)
  
  
}











##### Proportional symbol map - dot density map using images ####

# This function generates a proportional symbol map combining it with a classification method tha colours the symbols according to the parameter passed to the colmap() function.

# Parameters:
#             spobj - spatial object used to generate the map
#             var - variable containing the values to be represented
#             type - the type of calculation to determine the size of the circles. The possible values are: "volume", "surface", and "perceptual". The default is perceptual.
#             frame - spatial object used as frame to the points
#             percep - logical - if TRUE uses perceptual algorithm to define the size of symbols
#             max.size - maximum size of symbols
#             col - color of simbols used
#             pch - point character employed
#             leg.brks - number of breaks in the legend
#             horiz - logical - defines if the legend will be horizontal or vertical
#             leg.cex - size of characters on the legend
#             dec - decimals used to represent values in the legend
#             add - logical - adds this proportional symbol map to a previous one
#             leg.pos - legend position.
#             y.intersp - 
#             x.intersp -
#             varclass - variable used to the classification of cases
#             palet - RColorBrewer color palet employed
#             classint - classification method used (see colmap)
#             nbrks - number of breaks
#             invpalet - invert the order of color palet (see colmap and mapa)
#             lex.aux - position of auxiliary legend
#             title.leg - title of main legend
#             title.aux - title of auxiliary legend
#             title.adj - adjustment of the text of legend titles
#             custcol - custom colors (when do not wish to use RColorBrewer palettes)


propmap <- function(spobj, var=NULL, frame=NULL, type="perceptual", max.size=5, pch=21, leg.brks=3, horiz=F, horiz.aux=F, leg.cex=1.8, leg.aux.cex=1.8,dec=0, add=F, leg.pos="bottomleft", y.int=0.5, x.int=NULL, varclass=NULL, palet="Blues", classint="jenks", nbrks=5, invpalet=F, leg.aux="bottomright",title.leg=NULL, title.aux=NULL, title.adj=0.5, custcol=NULL, big.mark=".", decimal.mark=",", y.int.aux=0.5, x.int.aux=0.2, yjust=0.5,transp=99, userbrk=NULL,symbol.fg="black", symbol.bg="red", dec.aux=1,...){
  
  leg.brks <- leg.brks + 1
  

  require(classInt)
  
  if (class(spobj)=="SpatialPolygonsDataFrame") spobj <- SpatialPointsDataFrame(spobj, data.frame(spobj))
  
  var[is.na(var)] <- 0
  
  switch(type, volume = spobj$pval <- ((var/max(var,na.rm=T))^(1/3)) * max.size, surface = spobj$pval <- sqrt(var/max(var,na.rm=T)) * max.size, perceptual = spobj$pval <- ((var/max(var,na.rm=T)) * max.size))

  if (is.null(userbrk)){
    rd <- classIntervals(var=var, n=leg.brks, style=classint)
    rd <- rd$brks[2:length(rd$brks)]
  }else{
    rd <- userbrk[1:length(userbrk)]
  }
  
  switch(type, volume = ri <- ((rd/max(var,na.rm=T))^(1/3)) * max.size, surface = ri <- sqrt(rd/max(var,na.rm=T)) * max.size, perceptual = ri <- ((rd/max(var,na.rm=T)) * max.size)  )  
  
  
  
  if (! is.null(varclass)){
    varclass[is.na(varclass)] <- 0
    cores <- colmap(varclass,palet=palet, classint=classint,nbrks=nbrks, invpalet=invpalet, custcol=custcol, transp=transp)
    spobj$col <- cores[[1]]
    legs.aux <- as.numeric(cores[[2]])
    pal <- cores[[3]]
    col <- pal[length(pal)]
    sbcol <- rgb(t(col2rgb(symbol.bg)), maxColorValue=255)
  }else{
    col <- rgb(t(col2rgb(symbol.bg)), maxColorValue=255)
    spobj$col <- paste(col, transp, sep="")
    sbcol <- rgb(t(col2rgb(symbol.bg)), maxColorValue=255)
  } 
  
  
  spobj <- spobj[order(spobj$pval,decreasing=T),]
  
  if(! is.null(frame)){
    plot(frame)
    plot(spobj, pch=pch, cex=spobj$pval, bg=spobj$col, fg=symbol.fg, add=T)
  }else{
    plot(spobj, pch=pch, cex=spobj$pval, bg=spobj$col, fg=symbol.fg, add=add)
  }
  
  legs <- sort(rd, decreasing = T)
  ri <-sort(ri, decreasing = T)
  
  legs <- round(legs,dec)
  legs <- format(round(legs, dec), big.mark=big.mark, decimal.mark=decimal.mark, scientific = F)
  
  if(is.null(x.int)) x.int <- ri/max.size*0.7
  
  adleg(legpos = leg.pos, leg.main = title.leg, cex.leg = leg.cex, pt.cex=ri, y.intersp = y.int,x.intersp= x.int, pch = pch, classint = "unique", val = var, palette = sbcol,userbrk = userbrk,groups = legs, dec = dec, cl = "SpatialPoints", big.mark = big.mark, decimal.mark = decimal.mark, horiz = horiz, add = T, symbol.bg = sbcol, symbol.fg = symbol.fg)
  
  
  if (! is.null(varclass)){
    adleg(leg.aux, leg.main = title.aux, cex.leg = leg.aux.cex, y.intersp = y.int.aux, x.intersp = x.int.aux, classint = classint, val = var, palette = pal, cl = "SpatialPolygons", userbrk = userbrk, groups =legs.aux, big.mark = big.mark, decimal.mark = decimal.mark, horiz = horiz, add = T, dec = dec.aux)
  }
  
  #   par(family="")
  #   
}





##### Matriz de origen destino para cualquier flujo #####

calcMigra <- function(data=NULL, origin=NULL, destination=NULL, total=NULL){
  
  nm <- names(data)
  nm
  o <- grep(origin, nm)
  d <- grep(destination, nm)
  m <- grep(total, nm)
  
  ag <- aggregate(data[,m], by=list(origin=data[,o], destination=data[,d]), sum)
  
  names(ag)[3] <- "envia"
  ag <- ag[order(ag$origin),]
  
  ag$recibe <- 0
  ag$repete <- 0
  
  for (i in 1:nrow(ag)){
    x <- ag$envia[ag$origin==ag$destination[i] & ag$destination==ag$origin[i]]
    n <- which(ag$origin==ag$destination[i] & ag$destination==ag$origin[i])
    
    if(length(x)>0){
      ag$recibe[i] <- x
      if (n>i) ag$repete[ag$origin==ag$destination[i] & ag$destination==ag$origin[i]] <- 1}
    
    #     print(i)
  }
  
  ag <- ag[ag$repete==0,]
  ag$repete <- NULL
  ag$net_flow <- abs(ag$envia-ag$recibe)
  ag$cambio_demo <- ag$envia-ag$recibe
  ag$total_flow <- ag$envia+ag$recibe
  
  ag
}








##### Funcion que genera un objeto SpatialLine o SpatialLineDataFrame a partir de coordinadas de origen y de destino #####

### Obs: esta funcion se utiliza dentro de la proxima, ptCoords, que esta mas automatizada y resulta mas completa.

### Parametros:
### coords -matriz o data.frame con 4 columnas: longitud (origen), latitud (origen), longitud (destino), latitud  (destino)
### data -data.frame conteniendo los datos a ser asociados al archivo espacial. Debe contener el mismo numero de observaciones que pares origen-destino informados en coords.

# Crea una funcion que convierte coordinadas de origen y destino en objetos espaciales de linea
ptSpLine <- function(coords=NULL, data=NULL){
  
  vd <- as.matrix(coords)
  
  li <- lapply(1:nrow(vd),FUN=function(x) Lines(Line(rbind(vd[x,c(1:2)],vd[x,c(3:4)])),x))
  
  li <- SpatialLines(li)
  
  if (! is.null(data)) li <- SpatialLinesDataFrame(li, data=data, match.ID=F)
  
  return(li)
  
}


##### Genera un objeto espacial a partir de codigos o nombres geograficos #####

### Parametros:
### spboj - objeto espacial (puede ser tanto puntos como polígonos) a partir del cual se obtendran las coordinadas.
#           Debe contener al menos una variable que identique los casos. 
### idvar - numero indice o nombre de la variable contenida en el objeto espacial que identifica los codigos o nombres 
#           de los locales de origen y destino de los flujos.
### origin - vector numerico o alfanumerico que identifica los locales de ORIGEM de los flujos (en la variable idvar).
### destino - vector numerico o alfanumerico que identifica los locales de DESTINO de los flujos (en la variable idvar).
### spline - decide si se retorna o no un objeto SpatialLines o SpatialLinesDataFrame (utilizando la funcion ptSpLine). 
#            El estándar es verdadero (spline=T).
### data - data.frame a ser asociado con las coordinadas para generar un objeto SpatialLinesDataFrame.

ptCoords <- function(spobj=NULL,idvar=NULL, origin=NULL, destination=NULL, spline=T, data=NULL, coldata=NULL){
  
  require(maptools)
  
  if (is.null(coldata)) coldata <- names(data)
  
  if (!is.numeric(idvar)) idvar <- grep(idvar, names(spobj))
  
  row.names(spobj) <- as.character(data.frame(spobj)[,idvar])
  
  coordor <- coordinates(spobj[which(data.frame(spobj)[,idvar] %in% origin),])
  coorddest <- coordinates(spobj[which(data.frame(spobj)[,idvar] %in% destination),])
  
  codor <- data.frame(spobj)[which(data.frame(spobj)[,idvar] %in% origin),idvar]
  codest <- data.frame(spobj)[which(data.frame(spobj)[,idvar] %in% destination),idvar]
  
  origina <-   data.frame(cbind(rownames(coordor),coordor[,1], coordor[,2]))
  names(origina) <- c("origin","LONG_OR","LAT_OR")
  
  destinationa <- data.frame(cbind(rownames(coorddest),coorddest[,1], coorddest[,2]))
  names(destinationa) <- c("destination","LONG_DEST","LAT_DEST")
  
  dt <- merge(data, origina, by="origin", all.x=T)
  dt <- merge(dt, destinationa, by="destination", all.x=T)
  
  dt$LONG_OR <- as.numeric(as.character(dt$LONG_OR))
  dt$LONG_DEST <- as.numeric(as.character(dt$LONG_DEST))
  dt$LAT_OR <- as.numeric(as.character(dt$LAT_OR))
  dt$LAT_DEST <- as.numeric(as.character(dt$LAT_DEST))
  
  
  data <- dt[, c("LONG_OR","LAT_OR", "LONG_DEST","LAT_DEST", coldata)]
  
  if (spline==T){  
    return(ptSpLine(dt[,c("LONG_OR","LAT_OR", "LONG_DEST","LAT_DEST")], data[,c(coldata)]))
  }else{ return(dt)}
  
}







genVisit <- function(dt, idvar=NULL, aggregate=T){
  
  idvar <- grep(idvar,names(dt), fixed = T)
  
  for (i in 1:(nrow(dt)-1)){
    
    if (i==1) res <- cbind(dt[,idvar][dt$order==i], dt[,idvar][dt$order==i+1]) else res <- rbind(res, cbind(dt[,idvar][dt$order==i], dt[,idvar][dt$order==i+1]))   
    
  }
  
  
  res <- data.frame(res)
  names(res) <- c("origin","destination")
  
  res$total <- 1
  
  if (aggregate==T){
    res <- aggregate(res$total, by=list(origin=res$origin, destination=res$destination), sum)
    names(res)[3] <- "total"
  }
  
  res
  
}


