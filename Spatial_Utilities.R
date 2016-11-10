



##### Points in Polygon (Overlay) #####

# pointpoly() - counts the total number of points that fall inside each polygon of a spatial polygon object
# Parameters:
#             sppoint - spatial point object
#             sppoly - spatial polygon object
#             idvar - variable identifying the codes for each polygon in the sppoly object
#             by -  variable name identifying the different types of event (in the sppoint object).            
#             
#             


pointpoly <- function(sppoint, sppol, idvar=NULL, by=NULL){

  require(rgdal)
  require(rgeos)
  
  
  if(! class(sppoint)%in% c("SpatialPoints","SpatialPointsDataFrame")) stop("The spatial point object must belong to a SpatialPoints or a SpatialPointsDataFrame class.") 
  if(! class(sppol)%in% c("SpatialPolygons","SpatialPolygonsDataFrame")) stop("The spatial polygon object must belong to a SpatialPolygons or a SpatialPolygonssDataFrame class.")  


  if(is.null(idvar) & ! is.null(by)) stop("The name of the identification variable in the SpatialPolygons object must be provided when a grouping variable is defined.")
  
  
  if (is.na(proj4string(sppol))) proj4string(sppol) <- CRS("+proj=longlat +datum=WGS84") else   sppol <- spTransform(sppol, CRS(proj4string(sppol)))
  
  if (is.na(proj4string(sppoint))) proj4string(sppoint) <- CRS("+proj=longlat +datum=WGS84") else   sppoint <- spTransform(sppoint, CRS(proj4string(sppol)))
  
  if(is.null(by)){
  
    n_point <- over(sppol, sppoint, fn=length)
    dp <- data.frame(sppol)
    
    if(is.null(idvar)) val <- row.names(sppol) else val <- dp[,grep(idvar,names(dp), fixed = T)]
    
    if (class(val)=="factor") val <- as.character(val)
       
    data <- data.frame(cbind(val,n_point[,1]))
    
    names(data) <- c("id","tot_points")
    
    data
  
  }else{
    
    dt <- data.frame(sppoint)
    
    val <- as.character(unique(dt[,grep(by,names(dt), fixed = T)]))
    
    sppol <- sppol[,idvar]
    
    for (i in 1:length(val)){
      
      sb <- sppoint[which(dt[grep(by,names(sppoint), fixed = T)]==val[i]),]
      
      dh <- over(sb,sppol)
      
      dh <- data.frame(table(dh[,grep(idvar,names(dh), fixed = T)]))
      names(dh) <- c(idvar,substr(val[i],1,8))
      
      if (i==1) res <- dh else res <- merge(res, dh, by=idvar)
      
    }
    
    res <- merge(data.frame(sppol), res, by=idvar)
    
    res
    
    
  }
  
}







##### Area of Influence: Voronoi #####

# spvoro - creates a spatial polygon object based on the voronoi algorithm.
# Parameters:
#             sppoint - spatial point object used as base to calculate the voronoi diagram
#             sppoly - spatial polygon object used as frame to define the limits of the voronoi 
#             ID - character vector for the identification of polygons (if sppoint is not a 
#                  SpatialPointsDataFrame)
#             plot - plot the results
#             oper - one of geographical operations: "DIFF", "INT", "UNION", or "XOR", 
#                    representing difference, intersection, union, and exclusive-or, respectively.
spvoro <- function(sppoint, sppoly, ID=NULL, plot=T, oper="INT"){
  
  # Load the package PBSmapping, used here to make the operation
  require(PBSmapping)
  require(rgeos)
  require(maptools)
  
  
  # Extract the coordinates from Hospitals in Salamanca
  coord <- data.frame(coordinates(sppoint))
  names(coord) <- c("X","Y")
  
  # Calculate the Voronoi diagram according to the limits of Salamanca
  bb <- bbox(sppoly)
  voro <- calcVoronoi(coord, xlim=bb[1,], ylim=bb[2,])
  
  # Convert the results into a PolySet object
  voro.ps <- as.PolySet(voro, projection = "LL")
  
  # Convert the limits of Salamanca to Polyset 
  pol <- SpatialPolygons2PolySet(sppoly)
  
  # Performs the intersection between Voronoi and the window limits
  voro.ps <- joinPolys(voro.ps, pol, operation=oper)
  
  # Convert the results into a Spatial Polygon object
  voro_sppoly <- PolySet2SpatialPolygons(voro.ps, close_polys=TRUE)
  
  
  # checks if it is a SpatialPointsDataFrame
  if (class(sppoint)[1]=="SpatialPointsDataFrame"){
    dt <- data.frame(sppoint)
    voro_sppoly <- SpatialPolygonsDataFrame(voro_sppoly,dt, match.ID=F)
  }else{
    if(! is.null(ID)) row.names(voro_sppoly) <- as.character(ID)
  } 
  
  slot(voro_sppoly, "polygons") <- lapply(slot(voro_sppoly, "polygons"), checkPolygonsHoles) 
  
  # Plot the results if plot=T
  if (plot==T){
    plot(voro_sppoly)
    plot(sppoint, pch=19, col="red",cex=0.8, add=T)
  }
  
  
  
  return(voro_sppoly)
}



##### Spatial basic descriptives: Mean Center and Standard Distance #####

spdesc <- function(spobj=NULL, w=NULL, by=NULL){

  if(! is.null(by)) datas <-split(spobj, by) else datas <- list(spobj)
  
  byt <- unique(by)

  for (i in 1:length(datas)){

    spt <- datas[[i]]
    
    coords <- coordinates(spt)

    if (is.null(w)){
      mu_x <- sum(coords[,1])/length(coords[,1])
      mu_y <- sum(coords[,2])/length(coords[,2])
      
      dx <- (coords[,1]-mu_x)^2
      dy <- (coords[,2]-mu_y)^2
  
      sdsp <- sqrt((sum(dx)+sum(dy))/length(coords[,1]))
  
    }else{

      wt <- data.frame(spt)[,grep(w,names(spt), fixed = T)]
      mu_x <- sum(coords[,1]*wt)/sum(wt,na.rm = T)
      mu_y <- sum(coords[,2]*wt)/sum(wt,na.rm = T)
      
      dx <- wt*((coords[,1]-mu_x)^2)
      dy <- wt*((coords[,2]-mu_y)^2)
  
      sdsp <- sqrt((sum(dx)+sum(dy))/sum(wt, na.rm = T))
      
    }
  
    
    if (is.null(by)){
      red <- data.frame(cbind(mu_x,mu_y, sdsp))
    }else{
      red <- data.frame(cbind(mu_x,mu_y, sdsp, byt[i])) 
    }  
    
    
    if (i==1) res <- red else res <- rbind(res, red)

  }

  if (is.null(by)) names(res) <- c("mean_x","mean_y", "sp_sd") else names(res) <- c("mean_x","mean_y", "sp_sd", "by")
  
  coordinates(res) <- ~mean_x+mean_y
  
  res

}


#### Spatial Lag variable ####


splag <- function(spobj, val, queen=T){
  require(spatstat)
  require(spdep)
  
  if (class(spobj)%in%c("SpatialPolygonsDataFrame","SpatialPolygons")) vecino <- poly2nb(spobj, queen=queen) else vecino <- tri2nb(coordinates(spobj), row.names = row.names(spobj))
  
  lwvec <- nb2listw(vecino, zero.policy=T, )
  vallag <- lag.listw(lwvec, val, NAOK=T)
  vallag[is.na(vallag)] <- val[is.na(vallag)] 
  return(vallag)
}



##### Location Quotient #####

lqcalc <- function(var1, var2){
  lq <- (var1/var2)/(sum(var1, na.rm=T)/sum(var2, na.rm=T))
  lq[is.infinite(lq)] <- NA
  return(lq)}






