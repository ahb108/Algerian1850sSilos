
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") # MacOS

## Libraries
library(sf)
library(ape)

## Load data and convert to UTM31N
alltribes <- st_read("data/raw/vector/tribes/tribes.shp")
alltribes <- st_transform(alltribes, crs=32631)
tribesp <- alltribes[!alltribes$TribeID %in% c(7,8),]
subareas <- st_read("data/raw/vector/subareas/subareas.shp")
subareas <- st_transform(subareas, crs=32631)
tribes <- read.table("data/raw/tsv/tribes.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
tribes <- st_as_sf(tribes, coords=c("Longitude","Latitude"), crs=4326)
tribes <- st_transform(tribes, crs=32631)
silosbyfraction <- read.table("data/postprep/tsv/fractions_variables.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
silosbyfraction$SiloChange <- round(silosbyfraction$SiloCount1854 - silosbyfraction$SiloCount1853, 0)
silosbyfraction$PercSiloChange <- round((((silosbyfraction$SiloCount1854 - silosbyfraction$SiloCount1853) / silosbyfraction$SiloCount1853) * 100),2)
silosbyfraction <- st_as_sf(silosbyfraction, coords=c("LonPlot","LatPlot"), crs=4326)
silosbyfraction <- st_transform(silosbyfraction, crs=32631)

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

## Plot (a) a histpogram of absolute change in silo-pit counts per fraction from 1853 to 1854, and (b) a map percentage change in silo-pit counts per fraction from 1853 to 1854
dev.new(width=10, height=4)
par(mfrow=c(1,2))
par(mar=c(0.5, 0.5, 0.5, 0.5)) #c(bottom, left, top, right)
## panel 1
par(mar=c(0.5, 0.5, 0.5, 0.5)) #c(bottom, left, top, right)
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
plot(st_geometry(tribesp), col="grey90", border="white", lwd=1, add=TRUE)
plot(st_geometry(tribesp), border="white", lwd=1, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey75", font=2)
check0 <- !is.na(silosbyfraction$SiloChange)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange == 0,"SiloChange"]), cex=0.5, col="grey50", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange > 0 & silosbyfraction$SiloChange <= 50,"SiloChange"]), cex=0.5, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange > 50 & silosbyfraction$SiloChange <= 100,"SiloChange"]), cex=1, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange > 100 & silosbyfraction$SiloChange <= 300,"SiloChange"]), cex=1.8, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange > 300 & silosbyfraction$SiloChange <= 500,"SiloChange"]), cex=2.5, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange > 500 & silosbyfraction$SiloChange <= 700,"SiloChange"]), cex=3, col="red", pch=10, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange < 0 & silosbyfraction$SiloChange > -50,"SiloChange"]), cex=0.5, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange < -50 & silosbyfraction$SiloChange > -100,"SiloChange"]), cex=1, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange < -100 & silosbyfraction$SiloChange > -300,"SiloChange"]), cex=1.8, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange < -300 & silosbyfraction$SiloChange > -500,"SiloChange"]), cex=2.5, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$SiloChange < -500,"SiloChange"]), cex=3, col="blue", pch=10, add=TRUE)
text(544000,4020700, "Change in silo-pits \nper fraction (1853-1854)", cex=0.8, font=3)
points(535000,4014000, cex=0.5, col="grey50", pch=1)
points(535000,4010000, cex=1, col="grey50", pch=1)
points(535000,4006000, cex=1.5, col="grey50", pch=1)
points(535000,4001000, cex=2.5, col="grey50", pch=1)
points(535000,3994500, cex=3, col="grey50", pch=10)
text(541000,4014000, "0-50", cex=0.7, adj=0)
text(541000,4010000, "50-100", cex=0.7, adj=0)
text(541000,4006000, "100-300", cex=0.7, adj=0)
text(541000,4001000, "300-500", cex=0.7, adj=0)
text(541000,3994500, "500-800", cex=0.7, adj=0)
legend(532500,3990000, col=c("red","blue","grey50"), legend=c("increase","decrease","no change"), bty="n", cex=0.7, pch=15)
text(448000,3937000, "a", cex=1, font=2)
box()
## panel 2
par(mar=c(0.5, 0.5, 0.5, 0.5)) #c(bottom, left, top, right)
plot(st_geometry(alltribes), col="grey95", border=NA, xlim=c(448600,559000), ylim=c(3940000,4021000))
plot(st_geometry(tribesp), col="grey90", border="white", lwd=1, add=TRUE)
plot(st_geometry(tribesp), border="white", lwd=1, add=TRUE)
plot(st_geometry(subareas), lwd=3, col="grey75", lty="dashed", add=TRUE)
text(519000, 4018000, "North\nTell", font=4, cex=0.7, col="grey75")
text(475000, 3976000, "South\nTell", font=4, cex=0.7, col="grey75")
text(528000, 3941000, "High\nPlains", font=4, cex=0.7, col="grey75")
check <- tribesp$TribeID
text(st_coordinates(tribes[check,]), as.character(tribes$TribeID[check]), cex=0.6, font=2, col="grey75")
northArrow(464000, 3957750, r=4000, lwd=0.5)
scaleBar(454000, 3947500, width=20000, height=2500, lwd=0.5)
text(464000, 3950000, labels="20km", cex=0.75)
text(477000,4014500, "Medea", cex=0.6, col="grey75", font=2)
check0 <- !is.na(silosbyfraction$PercSiloChange)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange == 0,"PercSiloChange"]), cex=0.5, col="grey50", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange > 0 & silosbyfraction$PercSiloChange <= 50,"PercSiloChange"]), cex=0.5, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange > 50 & silosbyfraction$PercSiloChange <= 100,"PercSiloChange"]), cex=1, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange > 100 & silosbyfraction$PercSiloChange <= 300,"PercSiloChange"]), cex=1.8, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange > 300 & silosbyfraction$PercSiloChange <= 500,"PercSiloChange"]), cex=2.5, col="red", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange > 500 & silosbyfraction$PercSiloChange <= 700,"PercSiloChange"]), cex=3, col="red", pch=10, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange < 0 & silosbyfraction$PercSiloChange > -50,"PercSiloChange"]), cex=0.5, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange < -50 & silosbyfraction$PercSiloChange > -100,"PercSiloChange"]), cex=1, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange < -100 & silosbyfraction$PercSiloChange > -300,"PercSiloChange"]), cex=1.8, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange < -300 & silosbyfraction$PercSiloChange > -500,"PercSiloChange"]), cex=2.5, col="blue", pch=1, add=TRUE)
plot(st_geometry(silosbyfraction[check0 & silosbyfraction$PercSiloChange < -500,"PercSiloChange"]), cex=3, col="blue", pch=10, add=TRUE)
text(544000,4020700, "% change in silo-pits \nper fraction (1853-1854)", cex=0.8, font=3)
points(535000,4014000, cex=0.5, col="grey50", pch=1)
points(535000,4010000, cex=1, col="grey50", pch=1)
points(535000,4006000, cex=1.5, col="grey50", pch=1)
points(535000,4001000, cex=2.5, col="grey50", pch=1)
points(535000,3994500, cex=3, col="grey50", pch=10)
text(541000,4014000, "0-50%", cex=0.7, adj=0)
text(541000,4010000, "50-100%", cex=0.7, adj=0)
text(541000,4006000, "100-300%", cex=0.7, adj=0)
text(541000,4001000, "300-500%", cex=0.7, adj=0)
text(541000,3994500, "500-695%", cex=0.7, adj=0)
legend(532500,3990000, col=c("red","blue","grey50"), legend=c("increase","decrease","no change"), bty="n", cex=0.7, pch=15)
text(448000,3937000, "b", cex=1, font=2)
box()
dev.print(device=pdf, file=paste("outputs/pdf/figureSM1.pdf", sep=""))
dev.off()

## Spatial autocorrelation test (Moran's I, using inverse distance as weights)
## All fractions
testdata1 <- silosbyfraction[!is.na(silosbyfraction$PercSiloChange),]
changedists <- 1/ as.matrix(dist(st_coordinates(testdata1)))
diag(changedists) <- 0
satest <- Moran.I(x=testdata1$PercSiloChange, w=changedists)
if (satest$p.value > 0.5){
    satestp <- 1 - satest$p.value
} else {
    satestp <- satest$p.value
}
satestp
## Only those fractions with more reliable territorial placement (mostly further north).
testdata2 <- silosbyfraction[silosbyfraction$LocQual != "random" & !is.na(silosbyfraction$PercSiloChange),]
changedists <- 1/ as.matrix(dist(st_coordinates(testdata2)))
diag(changedists) <- 0
satest <- Moran.I(x=testdata2$PercSiloChange, w=changedists)
if (satest$p.value > 0.5){
    satestp <- 1 - satest$p.value
} else {
    satestp <- satest$p.value
}
satestp
