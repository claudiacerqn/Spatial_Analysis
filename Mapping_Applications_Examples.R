rm(list=ls())

source("d:/Dropbox/Stats/Code/Mapping_Applications/Mapping_Applications.R")

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


year <- seq(1998,2014,4) 


dt <- data.frame(br)

for (i in 1:length(year)){
  
  names(dt)
  dta <- dt[dt$year==year[i] & dt$party==13,c("GEOCODIG_M","prop_votes","qt_votes")]
  dtb <- dt[dt$year==year[i] & dt$party==45,c("GEOCODIG_M","prop_votes","qt_votes")]
  
  names(dta)[2] <- paste0("p_pt", year[i])
  names(dtb)[2] <- paste0("p_psdb", year[i])
  names(dta)[3] <- paste0("qt_pt", year[i])
  names(dtb)[3] <- paste0("qt_psdb", year[i])
  
  
  b <- merge(b, dta, by="GEOCODIG_M", all.X=T) 
  b <- merge(b, dtb, by="GEOCODIG_M", all.X=T) 
  
}


#########################################################################################################
#########################################################################################################
############################# ELECTORAL COMPETITION MAP #################################################
#########################################################################################################
#########################################################################################################






compmap(b, b$p_pt1998, b$p_psdb1998, b$p_pt2002, b$p_psdb2002, border = "transparent", frame = e, leg = c("PT","PSDB"), palette = "RdBu",lang = "en", leg.cex = 1.6, y.intersp = 0.5)

text(-40,3.6, "1998-2002", cex = 3)

compmap(b, b$p_pt2002, b$p_pt2006, b$p_psdb2002, b$p_psdb2006, border = "transparent", frame = e, leg = c("PT","PSDB"), palette = "RdBu", lang = "en", leg.cex = 1.6, y.intersp = 0.5)

text(-40,3.6, "2002-2006", cex = 3)


compmap(b, b$p_pt2006, b$p_pt2010, b$p_psdb2006, b$p_psdb2010, border = "transparent", frame = e, leg = c("PT","PSDB"), palette = "RdBu", lang = "en", leg.cex = 1.6, y.intersp = 0.5)

text(-40,3.6, "2006-2010", cex = 3)


compmap(b, b$p_pt2010, b$p_pt2014, b$p_psdb2010, b$p_psdb2014, border = "transparent", frame = e, leg = c("PT","PSDB"), palette = "RdBu", lang = "en", leg.cex = 1.6, y.intersp = 0.5)

text(-40,3.6, "2010-2014", cex = 3)





#########################################################################################################
#########################################################################################################
################################## Quadrant Map #########################################################
#########################################################################################################
#########################################################################################################


res <- qdmap(b, b$p_psdb2010, b$p_psdb2014, border = "transparent", frame=e, ret.col = T)


res <- qdmap(b, b$p_pt1998, b$p_pt2014, border = "transparent", frame=e, ret.col = T)

par(mar=c(4,4,1,1))
plot(res$zx, res$zy, col=res$colors, pch=19, xlab= "Z-score, PT 1998",  ylab= "Z-score, PT 2014")
abline(h=0, v=0)


qdmap(b, b$p_pt1998, b$p_pt2014, border = "transparent", frame=e, lang="pt", cex.leg = 1.6)

qdmap(b, b$p_psdb1998, b$p_psdb2014, border = "transparent", frame=e, zlim = 0.25, lang = "pt")


par(mfrow=c(1,2))
qdmap(b, b$p_pt1998, b$p_pt2010, border = "transparent", frame=e, selqad = 2)
qdmap(b, b$p_pt1998, b$p_pt2010, border = "transparent", frame=e, selqad = 3)
par(mfrow=c(1,1))


#########################################################################################################
#########################################################################################################
################################## High-Low Map #########################################################
#########################################################################################################
#########################################################################################################



hilomap(b, data.frame(b)[,c("p_pt2006","p_pt2010","p_pt2014")], border="transparent", frame = e)

hilomap(b, data.frame(b)[,c("p_psdb2006","p_psdb2010","p_psdb2014")], border="transparent", frame = e)


hilomap(b, data.frame(b)[,c("p_pt1998","p_pt2002","p_pt2006","p_pt2010","p_pt2014")], border="transparent", frame = e, lang = "pt")
text(-40,3.6, "PT", cex = 3)

hilomap(b, data.frame(b)[,c("p_psdb1998","p_psdb2002","p_psdb2006","p_psdb2010","p_psdb2014")], border="transparent", frame = e, lang="pt")
text(-40,3.6, "PSDB", cex = 3)



hilomap(b, data.frame(b)[,c("p_pt2006","p_pt2010","p_pt2014")], border="transparent", frame = e, zlim=1)



#########################################################################################################
#########################################################################################################
################################## Kernel Density Map ###################################################
#########################################################################################################
#########################################################################################################

res <- kernelmap(a, e, e, return = T, alpha=1)

b$qt_pt2014[is.na(b$qt_pt2014)] <- 0
b$qt_psdb2014[is.na(b$qt_psdb2014)] <- 0
b$p_pt2014[is.na(b$p_pt2014)] <- 0
b$p_psdb2014[is.na(b$p_psdb2014)] <- 0



# Density of votes in PT and PSDB
pt <- dotsInPolys(pl = b, x = b$qt_pt2014/(10^3))
ps <- dotsInPolys(pl = b, x = b$qt_psdb2014/(10^3))

par(mfrow=c(1,2))
kernelmap(pt, e, e, alpha=1.5)
kernelmap(ps, e, e, alpha=1.5, draw.leg = F)
par(mfrow=c(1,1))


# The same with the percentage of votes in PT and PSDB
pt <- dotsInPolys(pl = b, x = b$p_pt2014*100)
ps <- dotsInPolys(pl = b, x = b$p_psdb2014*100)


par(mfrow=c(1,2))
kernelmap(pt, e, e, alpha=1.5)
kernelmap(ps, e, e, alpha=1.5, draw.leg = F)
par(mfrow=c(1,1))

# The same with lower resolution
par(mfrow=c(1,2))
kernelmap(pt, e, e, alpha=1.5, res = 64)
kernelmap(ps, e, e, alpha=1.5, draw.leg = F, res = 64)
par(mfrow=c(1,1))




#########################################################################################################
#########################################################################################################
############################# Proportional Symbol Map ###################################################
#########################################################################################################
#########################################################################################################


propmap(b[b$qt_pt2014>50000,], b$qt_pt2014[b$qt_pt2014>50000], frame = e, y.int = 1.5)


par(mfrow=c(1,2))

propmap(b[b$qt_psdb2014>50000,], b$qt_psdb2014[b$qt_psdb2014>50000], frame = e, varclass = b$p_psdb2014[b$qt_psdb2014>50000]*100, leg.aux = "bottomleft", leg.pos=c(-75.5,-12), max.size = 8, y.int = 1, x.int = 1.2, leg.cex = 1, leg.aux.cex = 1, symbol.bg = "blue",leg.brks = 2, transp=98)

text(x = -39,y = 2.9, "PSDB", adj = 0, cex = 2)

propmap(b[b$qt_pt2014>50000,], b$qt_pt2014[b$qt_pt2014>50000], frame = e, varclass = b$p_pt2014[b$qt_pt2014>50000]*100, leg.aux = "bottomleft", leg.pos=c(-75.5,-12), max.size = 8, y.int = 1.1, x.int = 1.2, leg.cex = 1, leg.aux.cex = 1, palet = "Reds",leg.brks = 2, transp=98)

text(x = -39,y = 2.9, "PT", adj=0, cex = 2)

par(mfrow=c(1,1))






#########################################################################################################
#########################################################################################################
#################################### FLOW Map ###########################################################
#########################################################################################################
#########################################################################################################



cid <- read.csv("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/ciudades.csv", header=T, sep=";")
cd <- cid
cid  <- cid[,c("GEOCODIG_M","nome")]

mov <- read.csv("d:/Dropbox/Stats/Code/Spatial_Utilities_Data/Mov_2014.csv", header=T, sep=";")

dtb <- merge(mov, cid, by.x="origin", by.y="nome", all.x=T, sort=F)
names(dtb)[5] <- "COD_OR"

dtb <- merge(dtb, cid, by.x="destination", by.y="nome", all.x=T, sort=F)
names(dtb)[6] <- "COD_DEST"


dtd <- dtb[dtb$candidate=="Dilma",]
dta <- dtb[dtb$candidate=="Aécio",]
dtm <- dtb[dtb$candidate=="Marina",]


mDi <- calcMigra(dtd, "COD_OR", "COD_DEST", "total")
mAe <- calcMigra(dta, "COD_OR", "COD_DEST", "total")
mMa <- calcMigra(dtm, "COD_OR", "COD_DEST", "total")



# Finds the coordinates for the Brazilian cities
bx <- br[br$year==2014 & br$party==13,]

# Create the spatial flows object for each candidate
pDi <- ptCoords(bx,idvar = "GEOCODIG_M", origin = mDi$origin, destination = mDi$destination, spline = T, data = mDi)
pMa <- ptCoords(bx,idvar = "GEOCODIG_M", origin = mMa$origin, destination = mMa$destination, spline = T, data = mMa)
pAe <- ptCoords(bx,idvar = "GEOCODIG_M", origin = mAe$origin, destination = mAe$destination, spline = T, data = mAe)


pDi$lwd <- pDi$total_flow/max(pDi$total_flow)*5
pMa$lwd <- pMa$total_flow/max(pMa$total_flow)*5
pAe$lwd <- pAe$total_flow/max(pAe$total_flow)*5

bDi <- merge(bx, cd[,c("GEOCODIG_M","nome","dilma")])
bAe <- merge(bx, cd[,c("GEOCODIG_M","nome","aecio")])
bMa <- merge(bx, cd[,c("GEOCODIG_M","nome","marina")])

bDi <- bDi[which(bDi$dilma>0),]
bDi <- SpatialPointsDataFrame(bDi, data = data.frame(bDi))
bDi$lwd <- round((bDi$dilma/max(bDi$dilma))*10,1)

bAe <- bAe[which(bAe$aecio>0),]
bAe <- SpatialPointsDataFrame(bAe, data = data.frame(bAe))
bAe$lwd <- round((bAe$aecio/max(bAe$aecio))*10,1)

bMa <- bMa[which(bMa$marina>0),]
bMa <- SpatialPointsDataFrame(bMa, data = data.frame(bMa))
bMa$lwd <- round((bMa$marina/max(bMa$marina))*10,1)




kernelmap(pt, e, e, alpha=1.5, draw.leg = F)
plot(pDi, add=T, lwd=pDi$lwd, col="red")
propmap(bDi, bDi$lwd, add=T, col="red", trans=95, leg.pos=c(-75.5,-11), title.leg="Visitas", y.int=0.9)
legend("bottomleft", col="red", lwd=c(10,5.7,1),legend=c(7,4,1), bty="n", title="Viagens", cex=2, y.intersp=0.5, x.intersp=0.5,title.adj=0.35 )


kernelmap(ps, e, e, alpha=1.5, draw.leg = F)
plot(pAe, add=T, lwd=pAe$lwd, col="blue")
propmap(bAe, bAe$lwd, add=T, col="blue", symbol.bg = "blue",trans=95, leg.pos=c(-75.5,-11), title.leg="Visitas", y.int=0.9)
legend("bottomleft", col="blue", lwd=c(10,5.7,1),legend=c(7,4,1), bty="n", title="Viagens", cex=2, y.intersp=0.5, x.intersp=0.5,title.adj=0.35 )








