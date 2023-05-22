
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") #MacOSX

## Libraries
library(terra)

## Load data
silos <- read.table("data/raw/tsv/fractions_silos.tsv", header=TRUE, sep="\t", dec=".")
silos <- silos[silos$Year != 1884 & !silos$TribeID %in% c(9,10),]
silos <- silos[!is.na(silos$SiloCount),]
tribes <- read.table("data/raw/tsv/tribes.tsv", header=TRUE, sep="\t", dec=".")
alltribesp <- vect("data/raw/vector/tribes/tribes.shp")
alltribesp <- project(alltribesp, "+init=epsg:32631")
alltribesp$Hectares <- NULL
df <- data.frame(alltribesp)
tribes <- tribes[tribes %in% silos$TribeID,]

## Precipitation
prec <- rast("data/raw/raster/prec.tif")
tmp <- extract(prec, alltribesp)
df$AnnPrec <- round(aggregate(tmp$prec, by=list(ID=tmp$ID), FUN="mean", na.rm=TRUE)$x, 0)
## Proportion of thicker calcareous soils
calc <- rast("data/raw/raster/calcthick.tif")
tmp <- extract(calc, alltribesp)
df$CalcThick <- round(aggregate(tmp$calc, by=list(ID=tmp$ID), FUN="mean", na.rm=TRUE)$x, 2)

## Mapped southern tribal areas are only based on later 1860s-1880s cantonnement of groups that were even more mobile before. On balance therefore, better to have an average figure for these tribes.
## Use average of variables for Titteri, Deimat, Souary
mytribeIDs <- c(5,16,18)
df[df$TribeID %in% mytribeIDs,"AnnPrec"] <- round(mean(df[df$TribeID %in% mytribeIDs,"AnnPrec"]), 0)
df[df$TribeID %in% mytribeIDs,"CalcThick"] <- round(mean(df[df$TribeID %in% mytribeIDs,"CalcThick"]), 2)
## Use average of variables for Mouïdat and Ouled Mokhtar
mytribeIDs <- c(7,8)
df[df$TribeID %in% mytribeIDs,"AnnPrec"] <- round(mean(df[df$TribeID %in% mytribeIDs,"AnnPrec"]), 0)
df[df$TribeID %in% mytribeIDs,"CalcThick"] <- round(mean(df[df$TribeID %in% mytribeIDs,"CalcThick"]), 2)
## Use average of variables for Sahari Ouled Brahim and Ouled Sidi Aïssa el Adab
mytribeIDs <- c(9,10)
df[df$TribeID %in% mytribeIDs,"AnnPrec"] <- round(mean(df[df$TribeID %in% mytribeIDs,"AnnPrec"]), 0)
df[df$TribeID %in% mytribeIDs,"CalcThick"] <- round(mean(df[df$TribeID %in% mytribeIDs,"CalcThick"]), 2)

## Write out
write.table(df, file="data/postprep/tsv/tribes_environment.tsv", sep="\t", dec=".", row.names=FALSE, na="")
