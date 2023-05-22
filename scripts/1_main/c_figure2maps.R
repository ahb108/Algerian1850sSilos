
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") # MacOS

## Libraries
library(sf)

## Load data
silos <- read.table("data/raw/tsv/fractions_silos.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
silos <- silos[!is.na(silos$SiloCount),]
silos1853 <- silos[silos$Year == 1853,]
sites <- read.table("data/raw/tsv/sites_toponyms.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
chk <- is.na(sites$SiloCount1853) & is.na(sites$SiloCount1854)
sites <- sites[!chk,]
fractions <- read.table("data/raw/tsv/fractions.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
tribes <- read.table("data/raw/tsv/tribes.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
tribesagri <- read.table("data/raw/tsv/tribes_popagri.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
tribesagri <- tribesagri[tribesagri$Year==1853,c("TribeID","Zouidja")]
tribes <- merge(tribes, tribesagri, by="TribeID", all.x=TRUE)
alltribes <- st_read("data/raw/vector/tribes/tribes.shp")
tribesp <- alltribes[!alltribes$TribeID %in% c(7,8),]
subareas <- st_read("data/raw/vector/subareas/subareas.shp")
calcthick <- st_read("data/raw/vector/calcthick/calcthick.shp")
fractioninfo <- read.table("data/postprep/tsv/fractions_variables.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
fractioninfo$SilosPP <- fractioninfo$SiloCount1853 / fractioninfo$TotalPop
fractioninfo$SilosHa <- fractioninfo$SiloCount1853 / (fractioninfo$HaCultPP * fractioninfo$TotalPop)

## Add a complex variable comparing litres of cereal (1855/1856 harvest) per silo (1853 census)
fractionstats <- read.table("data/raw/tsv/fractions_censuses.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
fractionstats[fractionstats$TribeID==7,"FractionID"] <- 11
silos[silos$TribeID==8,"FractionID"] <- 35 # fix tribe/fraction inconsistencies
fractionstats[fractionstats$TribeID==8,"FractionID"] <- 35
fractionstats$CerealsL <- (fractionstats$BarleyKg / 0.6) + (fractionstats$WheatKg / 0.79)
tmpCerealsL <- aggregate(CerealsL ~ FractionID, fractionstats, sum)
head(fractioninfo)
fractioninfo <- merge(fractioninfo, tmpCerealsL, by="FractionID", all.x=TRUE)
fractioninfo$CerealsLSilo <- fractioninfo$CerealsL / fractioninfo$SiloCount1853

## Convert to UTM31N
alltribes <- st_transform(alltribes, crs=32631)
tribesp <- st_transform(tribesp, crs=32631)
sites <- st_as_sf(sites, coords=c("LonPlot","LatPlot"), crs=4326)
sites <- st_transform(sites, crs=32631)
fractions <- st_as_sf(fractions, coords=c("Longitude","Latitude"), crs=4326)
fractions <- st_transform(fractions, crs=32631)
tribes <- st_as_sf(tribes, coords=c("Longitude","Latitude"), crs=4326)
tribes <- st_transform(tribes, crs=32631)
subareas <- st_transform(subareas, crs=32631)
calcthick <- st_transform(calcthick, crs=32631)
fractioninfo <- st_as_sf(fractioninfo, coords=c("LonPlot","LatPlot"), crs=4326)
fractioninfo <- st_transform(fractioninfo, crs=32631)

## Extra plotting functions
scaleBar <- function(x, y, width, height,...){
  polygon(x=c(x, (x+width), (x+width), x), y=c(y, y, (y-height), (y-height)), ...)
}

northArrow <- function(x, y, r, fill="black", bkgrd=NA, ...){
    theta <- seq(0,(2*pi),len=360)
    angle <- 90 * (pi /180)
    x1 <- x + ((r * cos(theta) * cos(angle)) - (r * sin(theta) * sin(angle)))
    y1 <- y + ((r * cos(theta) * sin(angle)) + (r * sin(theta) * cos(angle)))
    cpol<-cbind(x1,y1)
    cpol <- rbind(cpol, cpol[1,])
    polygon(cpol, col=bkgrd, ...)
    bl <- c((x + r * sin(210*pi/180)),(y + r * cos(210*pi/180)))
    br <- c((x + r * sin(150*pi/180)),(y + r * cos(150*pi/180)))
    polygon(x=c(x, br[1], x,  bl[1],x),y=c(y+r, br[2], (y-(0.5 * r)), bl[2],y+r), col=fill, ...)
}

pHatch <- function(x, density){
    ## Modified with thanks from:
    ## https://gist.github.com/johnbaums/c6a1cb61b8b6616143538950e6ec34aa
    bb <- st_bbox(x)
    wdth <- bb[3] - bb[1]    
    x1 <- seq(bb[1], bb[3] + wdth, length.out=floor(density*3))
    x0 <- seq(bb[1] - wdth, bb[3], length.out=floor(density*3))
    y0 <- rep(bb[2], floor(density*3))
    y1 <- rep(bb[4], floor(density*3))
    mylines <- mapply(function(x0, y0, x1, y1) { rbind(c(x0, y0), c(x1, y1)) }, x0, y0, x1, y1, SIMPLIFY=FALSE)
    mylines <- st_sfc(st_multilinestring(mylines))
    st_crs(mylines) <- st_crs(x)
    return(st_intersection(mylines, x))
}

## Plot 1853 silos in four different ways
dev.new(width=10, height=8)
par(mfrow=c(2,2))
par(mar=c(0.5, 0.5, 0.5, 0.5)) #c(bottom, left, top, right)
## panel 1
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
checkp <- tribesp$TribeID %in% unique(sites$TribeID[!is.na(sites$SiloCount1853)])
plot(st_geometry(tribesp[checkp,]), col="grey90", border="white", lwd=1, add=TRUE)
plot(st_geometry(tribesp[checkp,]), border="white", lwd=1, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID[checkp]
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
check0 <- !is.na(sites$SiloCount1853)
check1 <- check0 & sites$SiloCount1853 <= 50
check2 <- check0 & sites$SiloCount1853 > 50 & sites$SiloCount1853 <= 100
check3 <- check0 & sites$SiloCount1853 > 100 & sites$SiloCount1853 <= 250
check4 <- check0 & sites$SiloCount1853 > 250 & sites$SiloCount1853 <= 400
check5 <- check0 & sites$SiloCount1853 > 400
plot(st_geometry(sites[check5,]), col="purple", pch=10, cex=3, lwd=1.5, add=TRUE)
plot(st_geometry(sites[check4,]), col="purple", pch=1, cex=2.5, add=TRUE)
plot(st_geometry(sites[check3,]), col="red", pch=1, cex=1.8, add=TRUE)
plot(st_geometry(sites[check2,]), col="orange", pch=19, cex=0.5, add=TRUE)
plot(st_geometry(sites[check1,]), col="grey50", pch=19, cex=0.2, add=TRUE)
plot(st_geometry(fractions[fractions$FractionID==91,]), col="blue", pch=0, cex=1, add=TRUE) ## one fraction of Haoura practiced wholly in-house storage
plot(st_geometry(fractions[fractions$FractionID %in% c(48, 103,158),]), col="blue", pch=0, cex=0.6, add=TRUE) ## two fractions of Mouzaia and one fraction of Ouled Sidi Ahmed ben Youssef practiced partly in-house storage
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey90", font=2)
text(448000,3937000, "a", cex=1, font=2)
text(543000,4020700, "silo-pit counts\nper site (1853)", cex=0.8, font=3)
points(535000,4014000, cex=0.2, col="grey50", pch=19)
points(535000,4010000, cex=0.5, col="orange", pch=19)
points(535000,4006000, cex=1.8, col="red", pch=1)
points(535000,4001000, cex=2.5, col="purple", pch=1)
points(535000,3994500, cex=3, col="purple", pch=10, lwd=1.5)
points(535000,3989000, cex=1, col="blue", pch=0)
text(541000,4014000, "1-50", cex=0.7, adj=0)
text(541000,4010000, "51-100", cex=0.7, adj=0)
text(541000,4006000, "101-250", cex=0.7, adj=0)
text(541000,4001000, "251-400", cex=0.7, adj=0)
text(541000,3994500, "401-580", cex=0.7, adj=0)
text(541000,3989500, "(in-house/farm)", cex=0.7, adj=0)
box()
## panel 2
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
checkp <- tribesp$TribeID %in% unique(sites$TribeID[!is.na(sites$SiloCount1853)])
plot(st_geometry(tribesp[checkp,]), col="grey90", border="white", lwd=1, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID[checkp]
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
check0 <- !is.na(fractioninfo$SilosPP)
check1 <- check0 & fractioninfo$SilosPP <= 0.2
check2 <- check0 & fractioninfo$SilosPP > 0.2 & fractioninfo$SilosPP <= 0.5
check3 <- check0 & fractioninfo$SilosPP > 0.5 & fractioninfo$SilosPP <= 1
check4 <- check0 & fractioninfo$SilosPP > 1 & fractioninfo$SilosPP <= 2
check5 <- check0 & fractioninfo$SilosPP > 2
check55 <- fractioninfo$Comments=="1855"
check56 <- fractioninfo$Comments=="1856"
checkother <- fractioninfo$Comments!="1855" & fractioninfo$Comments!="1856"
plot(st_geometry(fractioninfo[check1 & check55,"SilosPP"]), cex=0.5, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check55,"SilosPP"]), cex=1, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check55,"SilosPP"]), cex=1.8, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check55,"SilosPP"]), cex=2.5, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check55,"SilosPP"]), cex=3, col="chartreuse2", pch=10, add=TRUE)
plot(st_geometry(fractioninfo[check1 & check56,"SilosPP"]), cex=0.5, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check56,"SilosPP"]), cex=1, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check56,"SilosPP"]), cex=1.8, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check56,"SilosPP"]), cex=2.5, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check56,"SilosPP"]), cex=3, col="darkgreen", pch=10, add=TRUE)
plot(st_geometry(fractioninfo[check1 & checkother,"SilosPP"]), cex=0.5, col="saddlebrown", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & checkother,"SilosPP"]), cex=1, col="saddlebrown", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & checkother,"SilosPP"]), cex=1.8, col="saddlebrown", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & checkother,"SilosPP"]), cex=2.5, col="saddlebrown", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & checkother,"SilosPP"]), cex=3, col="saddlebrown", pch=10, add=TRUE)
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey90", font=2)
text(448000,3937000, "b", cex=1, font=2)
text(544000,4020700, "silo-pit counts (1853)\nper capita", cex=0.8, font=3)
points(535000,4014000, cex=0.5, col="grey50", pch=1)
points(535000,4010000, cex=1, col="grey50", pch=1)
points(535000,4006000, cex=1.8, col="grey50", pch=1)
points(535000,4001000, cex=2.5, col="grey50", pch=1)
points(535000,3994500, cex=3, col="grey50", pch=10, lwd=1.5)
text(541000,4014000, "0.04-0.2", cex=0.7, adj=0)
text(541000,4010000, "0.2-0.5", cex=0.7, adj=0)
text(541000,4006000, "0.5-1", cex=0.7, adj=0)
text(541000,4001000, "1-2", cex=0.7, adj=0)
text(541000,3994500, "2-2.7", cex=0.7, adj=0)
text(538000,3988000, "population from:", cex=0.7, adj=0)
legend(535000,3988000, col=c("chartreuse2","darkgreen","saddlebrown"), legend=c("1855 census","1856 census","1850s (tribe)"), bty="n", cex=0.7, pch=15)
box()
## panel 3
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
checkp <- tribesp$TribeID %in% unique(sites$TribeID[!is.na(sites$SiloCount1853)])
plot(st_geometry(tribesp[checkp,]), col="grey90", border="white", lwd=1, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID[checkp]
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
check0 <- !is.na(fractioninfo$SilosHa)
check1 <- check0 & fractioninfo$SilosHa <= 0.15
check2 <- check0 & fractioninfo$SilosHa > 0.15 & fractioninfo$SilosHa <= 0.4
check3 <- check0 & fractioninfo$SilosHa > 0.4 & fractioninfo$SilosHa <= 0.6
check4 <- check0 & fractioninfo$SilosHa > 0.6 & fractioninfo$SilosHa <= 0.9
check5 <- check0 & fractioninfo$SilosHa > 0.9
check55 <- fractioninfo$Comments=="1855"
check56 <- fractioninfo$Comments=="1856"
checkother <- fractioninfo$Comments!="1855" & fractioninfo$Comments!="1856"
plot(st_geometry(fractioninfo[check1 & check55,"SilosHa"]), cex=0.5, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check55,"SilosHa"]), cex=1, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check55,"SilosHa"]), cex=1.8, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check55,"SilosHa"]), cex=2.5, col="chartreuse2", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check55,"SilosHa"]), cex=3, col="chartreuse2", pch=10, add=TRUE)
plot(st_geometry(fractioninfo[check1 & check56,"SilosHa"]), cex=0.5, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check56,"SilosHa"]), cex=1, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check56,"SilosHa"]), cex=1.8, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check56,"SilosHa"]), cex=2.5, col="darkgreen", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check56,"SilosHa"]), cex=3, col="darkgreen", pch=10, add=TRUE)
plot(st_geometry(fractioninfo[check1 & checkother,"SilosHa"]), cex=0.5, col="darkgoldenrod1", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & checkother,"SilosHa"]), cex=1, col="darkgoldenrod1", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & checkother,"SilosHa"]), cex=1.8, col="darkgoldenrod1", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & checkother,"SilosHa"]), cex=2.5, col="darkgoldenrod1", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & checkother,"SilosHa"]), cex=3, col="darkgoldenrod1", pch=10, add=TRUE)
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey90", font=2)
text(448000,3937000, "c", cex=1, font=2)
text(544000,4020700, "silo-pit counts per hectare\n(1853 zouidja * ~10 ha)", cex=0.8, font=3)
points(535000,4014000, cex=0.5, col="grey50", pch=1)
points(535000,4010000, cex=1, col="grey50", pch=1)
points(535000,4006000, cex=1.8, col="grey50", pch=1)
points(535000,4001000, cex=2.5, col="grey50", pch=1)
points(535000,3994500, cex=3, col="grey50", pch=10, lwd=1.5)
text(541000,4014000, "0.015-0.15", cex=0.7, adj=0)
text(541000,4010000, "0.15-0.4", cex=0.7, adj=0)
text(541000,4006000, "0.4-0.6", cex=0.7, adj=0)
text(541000,4001000, "0.6-0.9", cex=0.7, adj=0)
text(541000,3994500, "0.9-1.16", cex=0.7, adj=0)
text(538000,3988000, "arable area from:", cex=0.7, adj=0)
legend(530000,3988000, col=c("chartreuse2","darkgreen","darkgoldenrod1"), legend=c("1855 census (fraction)","1856 census (fraction)","1853 (tribe downsampled)"), bty="n", cex=0.7, pch=15)
box()
## panel 4
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
checkp <- tribesp$TribeID %in% unique(sites$TribeID[!is.na(sites$SiloCount1853)])
plot(st_geometry(tribesp[checkp,]), col="grey90", border="white", lwd=1, add=TRUE)
plot(pHatch(calcthick, density=25), col="grey75", border=NA, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID[checkp]
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
check0 <- !is.na(fractioninfo$CerealsLSilo)
check1 <- check0 & fractioninfo$CerealsLSilo <= 400
check2 <- check0 & fractioninfo$CerealsLSilo > 400 & fractioninfo$CerealsLSilo <= 1000
check3 <- check0 & fractioninfo$CerealsLSilo > 1000 & fractioninfo$CerealsLSilo <= 2000
check4 <- check0 & fractioninfo$CerealsLSilo > 2000 & fractioninfo$CerealsLSilo <= 3000
check5 <- check0 & fractioninfo$CerealsLSilo > 3000
check55 <- fractioninfo$Comments=="1855"
check56 <- fractioninfo$Comments=="1856"
checkother <- fractioninfo$Comments!="1855" & fractioninfo$Comments!="1856"
plot(st_geometry(fractioninfo[check1 & check55,"CerealsLSilo"]), cex=0.5, col="red", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check55,"CerealsLSilo"]), cex=1, col="red", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check55,"CerealsLSilo"]), cex=1.8, col="red", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check55,"CerealsLSilo"]), cex=2.5, col="red", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check55,"CerealsLSilo"]), cex=3, col="red", pch=10, add=TRUE)
plot(st_geometry(fractioninfo[check1 & check56,"CerealsLSilo"]), cex=0.5, col="orange", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check2 & check56,"CerealsLSilo"]), cex=1, col="orange", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check3 & check56,"CerealsLSilo"]), cex=1.8, col="orange", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check4 & check56,"CerealsLSilo"]), cex=2.5, col="orange", pch=1, add=TRUE)
plot(st_geometry(fractioninfo[check5 & check56,"CerealsLSilo"]), cex=3, col="orange", pch=10, add=TRUE)
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey75", font=2)
text(544000,4020700, "cereals (L) per silo (1853)", cex=0.8, font=3)
points(535000,4014000, cex=0.5, col="grey50", pch=1)
points(535000,4010000, cex=1, col="grey50", pch=1)
points(535000,4006000, cex=1.8, col="grey50", pch=1)
points(535000,4001000, cex=2.5, col="grey50", pch=1)
points(535000,3994500, cex=3, col="grey50", pch=10, lwd=1.5)
text(541000,4014000, "31-400", cex=0.7, adj=0)
text(541000,4010000, "400-1000", cex=0.7, adj=0)
text(541000,4006000, "1000-2000", cex=0.7, adj=0)
text(541000,4001000, "2000-3000", cex=0.7, adj=0)
text(541000,3994500, "3000-5210", cex=0.7, adj=0)
text(538000,3988000, "harvest size from:", cex=0.7, adj=0)
legend(542000,3987000, col=c("red","orange"), legend=c("1855","1856"), bty="n", cex=0.7, pch=15)
legend(531000,3982000, legend="", fill="grey75", border=NA, density=25, bty="n", cex=2)
text(550000,3974000, "calcareous\n/thick soils", cex=0.7)
text(448000,3937000, "d", cex=1, font=2)
box()
dev.print(device=pdf, file=paste("outputs/pdf/figure2.pdf", sep=""))
dev.off()


