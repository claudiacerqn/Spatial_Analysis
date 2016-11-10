source("d:/Dropbox/Stats/Code/Spatial_Utilities.R")

library(sp)
library(maps)
library(maptools)



e <- readShapeSpatial("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/Estados.shp")
a <- readShapeSpatial("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/Aerodromos.shp")
b <- readShapeSpatial("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/mapa_base.shp")



a$pavement <- "other"
a$pavement[a$PAVIMENTO=="terra"] <- "dirt runway"
a$pavement[a$PAVIMENTO%in%c("asfalto ou concreto Asfál","concreto")] <- "paved"


uf <- c("SP","MG","RJ","BA","RS","PR","PE","CE","PA","MA","SC","GO","AM","PB","ES","RN","AL","MT","PI","DF","MS","SE","RO","TO","AC","AP","RR")
pop <- c(44.8,21,16.6,15.3,11.3,11.2,9.4,9,8.3,7,6.9,6.7,4,4,4,3.5,3.4,3.3,3.2,3.2,2.7,2.3,1.8,1.5,0.8,0.8,0.5)

u <- data.frame(UF=uf, Population=pop)

e <- merge(e,u, by="UF")





par(mar=rep(0,4))
plot(e)
plot(a, pch=19, add=T)
plot(e,border="red", lwd=2, add=T)


# Retrieves the number of points in each polygon using polygons row.names for identification.
pointpoly(a,e)

# Retrieves the number of points in each polygon using one identification variable.
pointpoly(a,e, idvar="UF")

# Retrieves the number of points according to a grouping variable defining different types of event.
pointpoly(a,e, idvar="UF", by = "pavement")





###### Voronoi ####################



es <- e[e$UF=="SP",]

as <- a[which(a$UF=="SP"),]

plot(es)
plot(as, pch=19, add=T)

vo <- spvoro(as,es,ID = "NOME")

plot(vo)




###### Spatial Descriptive measures: mean center and stardard distance ######


### loads the data
load("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/BR_Pres_vote.RData")

bl <- readShapeSpatial("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/Brazilian_Localities.shp")
bl <- bl[,c("GEOCODIG_M", "LONG","LAT")]

## Merges electoral data to the geographical locations of Brazilian municipalities
br <- br[order(br$year),]
br <- merge(br, bl, by="GEOCODIG_M", all.x=T)
br <- br[! is.na(br$LONG),]

## Converts the new object into a SpatialPointsDataFrame
coordinates(br) <- ~LONG+LAT


### Simple example: Brazilian localities

# Spatial descriptives for the Brazilian localities
bx <- spdesc(bl)

# See the results
bx

# plot the results
plot(e)
plot(bx, pch=19, col="orange", add=T)

# Draws a circle with a radius corresponding to the standard distance
library(plotrix)
draw.circle(coordinates(bx)[,1],coordinates(bx)[,2], bx$sp_sd)


#### Expand the Example to integrate two additional characteristics: weight the center mean by the number of votes obtained and compute the center for each electoral year.

# Separates the data for each major party: PT, PSDB and PSB
pt <- br[br$party==13,]
ps <- br[br$party==45,]
po <- br[br$party==43 & br$year==2010,]
po <- rbind(po, br[br$party==40 & br$year==2014,])


# Compute the spatial descriptives for each party
xt <- spdesc(pt, w = "qt_votes",by = pt$year)
xs <- spdesc(ps, w = "qt_votes",by = ps$year)
xo <- spdesc(po, w = "qt_votes",by = po$year)

# Plot the State of Minas Gerais (where all cases are located)
plot(e[e$UF=="MG",])

# Adds the centers for each party and year in the map
plot(xt, pch=19, col="red", add=T)
plot(xs, pch=19, col="blue", add=T)
plot(xo, pch=19, col="darkgreen", add=T)

# Adds the years to identify each election
text(coordinates(xt), labels = xt$by, col = "red", pos=3)
text(coordinates(xs), labels = xs$by, col = "blue", pos=3)
text(coordinates(xo), labels = xo$by, col = "darkgreen", pos=3)

# Adds Brazilian mean center
plot(bx, pch=19, col="orange", add=T)
text(coordinates(bx), labels = "Brazil", pos = 3)

# Adds a legend to the map
legend(-44,-18, legend = c("PT","PSDB","Marina"), fill=rep("white",3), border = c("red", "blue","darkgreen"), bty="n", y.intersp = 0.7)

View(po)


######################################################################################################
######################################################################################################
############################## Spatial Lagged variable ###############################################
######################################################################################################
######################################################################################################


### One of the most employed methods in spatial analysis
### Smooths the distribution of variables, facilitating the visualization of patterns.

br <- data.frame(br)
bpt <- br[br$party==13 & br$year==2002, c("GEOCODIG_M","qt_votes","tot_votes","prop_votes")]
ba <- merge(b, bpt, by="GEOCODIG_M", all.x=T )


ba$lag <- splag(spobj = ba, ba$prop_votes, queen = T)


# A wrap-up function to map values and avoid repeating code.
map <- function(spobj, spframe, var){

  require(RColorBrewer)
  
  col <- brewer.pal(n = 5, "Reds")
  v <- quantile(x = var, probs = seq(0,1,0.2), na.rm = T)
  legs <- leglabs(round(v,2))
  plot(spframe)
  plot(spobj, pch=19, col=col[findInterval(var,v, all.inside = T)], add=T, border="transparent")
  plot(spframe, lwd=2, add=T)
  legend("bottomleft", fill=col, legend = legs, bty="n")
  
}


par(mfrow=c(1,2))

map(ba,e,ba$prop_votes)

map(ba,e,ba$lag)

par(mfrow=c(1,1))




######################################################################################################
######################################################################################################
#################################### Location Quotient ###############################################
######################################################################################################
######################################################################################################


bpa <- br[br$party==45 & br$year==2014, c("GEOCODIG_M","qt_votes","tot_votes")]
bpb <- br[br$party==43 & br$year==2014, c("GEOCODIG_M","qt_votes")]

names(bpa) <- c( "GEOCODIG_M","pa_votes","tot_votes")
names(bpb) <- c( "GEOCODIG_M","pb_votes")

bpa <- merge(bpa, bpb, by="GEOCODIG_M", all.x=T)
ba <- merge(b, bpa, by="GEOCODIG_M", all.x=T )


map(ba,e,lqcalc(ba$pa_votes,ba$tot_votes))

map(ba,e,lqcalc(ba$pb_votes,ba$tot_votes))

map(ba,e,lqcalc(ba$pa_votes,ba$pb_votes))

map(ba,e,lqcalc(ba$pb_votes,ba$pa_votes))









