
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") #MacOSX

## Libraries
library(sf)

#### Preliminaries ####

## Load data
silos <- read.table("data/raw/tsv/fractions_silos.tsv", header=TRUE, sep="\t", dec=".")
silos <- silos[silos$Year != 1884 & !silos$TribeID %in% c(9,10),]
silos <- silos[!is.na(silos$SiloCount),]
fractions <- read.table("data/raw/tsv/fractions.tsv", header=TRUE, sep="\t", dec=".")
fractionstats <- read.table("data/raw/tsv/fractions_censuses.tsv", header=TRUE, sep="\t", dec=".")
tribes <- read.table("data/raw/tsv/tribes.tsv", header=TRUE, sep="\t", dec=".")
tribespopagri <- read.table("data/raw/tsv/tribes_popagri.tsv", header=TRUE, sep="\t", dec=".")
markets <- read.table("data/raw/tsv/towns_markets.tsv", header=TRUE, sep="\t", dec=".")

## Handle tribes or fractions that are inconsistently treated across the 1853-1856 records.
silos[silos$TribeID==7,"FractionID"] <- 11 # lump Oulad Moktar
fractionstats[fractionstats$TribeID==7,"FractionID"] <- 11
silos[silos$TribeID==8,"FractionID"] <- 35 # lump Mouïdat
fractionstats[fractionstats$TribeID==8,"FractionID"] <- 35
silos[silos$FractionID==55,"TribeID"] <- 15 # make Ouled Sidi Nadji consistently a fraction of Ouled Deïd rather than sometimes a separate tribe
fractions[fractions$FractionID==55,"TribeID"] <- 15 # Ouled Sidi Nadji

## Keep only the relevant fractions and tribes across all datasets
myfractions <- unique(silos$FractionID)
mytribes <- unique(silos$TribeID)
fractions <- fractions[fractions$FractionID %in% myfractions,]
fractionstats <- fractionstats[fractionstats$FractionID %in% myfractions,]
tribes <- tribes[tribes$TribeID %in% mytribes,]
tribespopagri <- tribespopagri[tribespopagri$TribeID %in% mytribes,]

## Add environmental variables (tribe-level territorial averages)
tenv <- read.table("data/postprep/tsv/tribes_environment.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
tenv <- tenv[,c("TribeID","AnnPrec","CalcThick")]
fractions <- merge(fractions, tenv, by="TribeID", all.x=TRUE)

## Output container
mycols <- c("FractionID", "TribalFraction", "TribeID", "Tribe", "SiloCount1853", "SiloCount1854", "TotalPop", "Makhzen", "Berber", "HaCultPP", "WealthPP", "LWealthPP", "LWealthProp", "LivestockPP", "CamelSheepProp", "TentProp", "SaharaMarkets", "WeeklyMarkets", "AnnPrec", "CalcThick", "Longitude", "Latitude", "LocQual", "LonPlot",  "LatPlot","SubRegion","Comments")
fractioninfo <- as.data.frame(matrix(ncol=length(mycols), nrow=length(myfractions)), stringsAsFactors=FALSE)
names(fractioninfo) <- mycols
fractioninfo$FractionID <- myfractions

#### Silos and Basic Data ####

## Harmonise those cases where the location is recorded in two consecutive rows for the same fraction. These are likely slight subdivisions in the same general place but they are not always treated the same way each year. So, for consistency across years, lump those locations with the same SiteID in the same Year in the same FractionID together.
myyears <- c(1853,1854)
dupSiteIDs <- NULL
dupSRIDs <- NULL
for (a in 1:length(myyears)){
    myyear <- silos[silos$Year==myyears[a],]
    myfractions1 <- unique(myyear$FractionID)
    for (b in 1:length(myfractions1)){
        myfraction <- myyear[myyear$FractionID == myfractions1[b],]
        chck <- duplicated(myfraction$SiteID)
        if (any(chck)){
            duprecs <- myfraction[myfraction$SiteID %in% myfraction[chck,"SiteID"],]
            tmp1 <- data.frame(SRID=duprecs$SRID, Year=myyears[a], FractionID=myfractions1[b], SiteID=duprecs$SiteID, SiloCount=duprecs$SiloCount, stringsAsFactors=FALSE)
            mysites <- unique(tmp1$SiteID)
            for (d in 1: length(mysites)){
                tmp2 <- duprecs[duprecs$SiteID==mysites[d],][1,]
                tmp2$SiloCount <- sum(duprecs[duprecs$SiteID==mysites[d],"SiloCount"])
                if (is.null(dupSiteIDs)){
                    dupSiteIDs <- tmp2
                } else {
                    dupSiteIDs <- rbind(dupSiteIDs,tmp2)
                }
            }
            if (is.null(dupSRIDs)){
                dupSRIDs <- tmp1
            } else {
                dupSRIDs <- rbind(dupSRIDs,tmp1)
            }
        }
    }
}
nrow(silos)
silos <- rbind(silos[!silos$SRID %in% dupSRIDs$SRID,], dupSiteIDs)
nrow(silos)

## Aggregate silo counts by fraction
silosbyfraction <- aggregate(silos[silos$Year==1853, "SiloCount"], by=list(Category=silos[silos$Year==1853, "FractionID"]), FUN=sum)
names(silosbyfraction) <- c("FractionID", "SiloCount1853")
tmp <- aggregate(silos[silos$Year==1854, "SiloCount"], by=list(Category=silos[silos$Year==1854, "FractionID"]), FUN=sum)
names(tmp) <- c("FractionID", "SiloCount1854")
silosbyfraction <- merge(silosbyfraction, tmp, by="FractionID", all=TRUE)

#### Handling spatial coordinates and sub-regions ####
## Distinguish plotting LatLon from analytical LatLon
fractions$LonPlot <- fractions$Longitude
fractions$LatPlot <- fractions$Latitude
fractions$Longitude <- NULL
fractions$Latitude <- NULL
fractions <- merge(fractions, tribes[,c("TribeID","Longitude","Latitude"),], by="TribeID", all.x=TRUE)
fractions$Longitude[fractions$LocQual!="random"] <- fractions$LonPlot[fractions$LocQual!="random"]
fractions$Latitude[fractions$LocQual!="random"] <- fractions$LatPlot[fractions$LocQual!="random"]   
## Fill
for (a in 1:nrow(fractioninfo)){
    fractioninfo[a,"SiloCount1853"] <- silosbyfraction[silosbyfraction$FractionID==fractioninfo$FractionID[a],"SiloCount1853"]
    fractioninfo[a,"SiloCount1854"] <- silosbyfraction[silosbyfraction$FractionID==fractioninfo$FractionID[a],"SiloCount1854"]
    fractioninfo[a,"TribeID"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"TribeID"]
    fractioninfo[a,"TribalFraction"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"TribalFraction"]
    fractioninfo[a,"Tribe"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"Tribe"]
    fractioninfo[a,"Makhzen"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"Makhzen"]
    fractioninfo[a,"Berber"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"Berber"]
    fractioninfo[a,"AnnPrec"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"AnnPrec"]
    fractioninfo[a,"CalcThick"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"CalcThick"]
    fractioninfo[a,"Longitude"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"Longitude"]
    fractioninfo[a,"Latitude"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"Latitude"]
    fractioninfo[a,"LocQual"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"LocQual"]
    fractioninfo[a,"LonPlot"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"LonPlot"]
    fractioninfo[a,"LatPlot"] <- fractions[fractions$FractionID==fractioninfo$FractionID[a],"LatPlot"]
    fractioninfo$SubRegion[fractioninfo$TribeID %in% c(21:26,28:29)] <- "North Tell"
    fractioninfo$SubRegion[fractioninfo$TribeID %in% c(5,6,11:20,30)] <- "South Tell"
    fractioninfo$SubRegion[fractioninfo$TribeID %in% c(3,7,8)] <- "High Plains"
    fractioninfo$SubRegion[fractioninfo$TribeID %in% c(27)] <- "East Chelif"
}

#### Census Data ####

## Issues and how they have been handled.
## -- Abid-El Ahkoum and Abid-El Kessentia missing from 1854 silo count (seem to have just been omitted rather than amalgamated with another fraction). No action taken.
## -- Beni Hassen-Oulad Sidi Yagoub missing from 1854 silo count (seems to have just been omitted rather than amalgamated with another fraction). No action taken.
## -- Hassen Ben Ali-Oulad Maiza missing from 1853 (silo locations SiteID 427 and 540 in 1853 are probably the ones used by Oulad Maiza in 1854) -- could delete or create 1853 faction, but on balance no action taken.

## TotalPop
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
tpop1851 <- tribespopagri[tribespopagri$Year %in% c(1851) & !is.na(tribespopagri$TotalPop),]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872) & !is.na(fractionstats$TotalPop),]
pop0 <- data.frame(TribeID=c(5,15),TotalPop=c(1000,2000), stringsAsFactors=FALSE)
for (b in 1:length(myfractions)){
    pop18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"TotalPop"])
    tpop1851b <- tpop1851[tpop1851$TribeID==fractions[which(fractions$FractionID==myfractions[b]),"TribeID"],"TotalPop"]
    mytribe <- fractions[which(fractions$FractionID==myfractions[b]),"TribeID"]
    if (mytribe==30){ mytribe <- 15 }
    tpop0 <- pop0[pop0$TribeID==mytribe,"TotalPop"]
    msilos <- mean(as.numeric(fractioninfo[fractioninfo$FractionID==myfractions[b],c("SiloCount1853","SiloCount1854")]), na.rm=TRUE)
    cofractions <- fractioninfo[fractioninfo$TribeID==mytribe,"FractionID"]
    cosilos <- fractioninfo[fractioninfo$TribeID==mytribe,c("SiloCount1853","SiloCount1854")]
    propbysilo <- msilos / sum(as.numeric(apply(cosilos,1,mean, na.rm=TRUE)))
    if (pop18551856>0){
        ## If good 1855-56 data exists, use it.
        fractioninfo[b,"TotalPop"] <- pop18551856
        is1855 <- fractionstats[fractionstats$Year==1855,"FractionID"]
        if (fractioninfo[b,"FractionID"] %in% is1855){
            fractioninfo[b,"Comments"] <- "1855"
        } else {
            fractioninfo[b,"Comments"] <- "1856"
        }
    } else if (length(tpop1851b)==1){
        ## Otherwise if 1851 population exists, downsample it to fraction level proportional to mean 1853-1854 silo count.
        fractioninfo[b,"TotalPop"] <- round(tpop1851b*propbysilo,0)
        fractioninfo[b,"Comments"] <- "1851/1872 imputed"
    } else {
        ## Otherwise make a more informal estimate of tribal population based on whatever is known (1843-1872), then downsample it to fraction level proportional to mean 1853-1854 silo count.
        fractioninfo[b,"TotalPop"] <- round(tpop0*propbysilo,0)
        fractioninfo[b,"Comments"] <- "Other/1872 imputed"
    }
}

##LivestockPP
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872),]
for (b in 1:length(myfractions)){
    b18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Cattle"], na.rm=TRUE)
    s18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Sheep"], na.rm=TRUE)
    g18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Goats"], na.rm=TRUE)
    h18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Horses"], na.rm=TRUE)
    md18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"MulesDonkeys"], na.rm=TRUE)
    c18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Camels"], na.rm=TRUE)
    all18551856 <- (b18551856+(s18551856*0.2)+(g18551856*0.2)+h18551856+md18551856+c18551856)
    lspp18551856 <- all18551856 / fractioninfo[fractioninfo$FractionID==myfractions[b],"TotalPop"]
    b1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Cattle"], na.rm=TRUE)
    s1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Sheep"], na.rm=TRUE)
    g1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Goats"], na.rm=TRUE)
    h1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Horses"], na.rm=TRUE)
    md1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"MulesDonkeys"], na.rm=TRUE)
    c1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Camels"], na.rm=TRUE)
    all1872 <- (b1872+(s1872*0.2)+(g1872*0.2)+h1872+md1872+c1872)
    lspp1872 <- all1872 / fractioninfo[fractioninfo$FractionID==myfractions[b],"TotalPop"]
    if (all18551856>0){
        ## If good 1855-56 fraction data exists, use it.
        fractioninfo[b,"LivestockPP"] <- round(lspp18551856,3)
    } else if (all1872>0){
        ## Otherwise use 1872 fraction data.
        fractioninfo[b,"LivestockPP"] <- round(lspp1872,3)
    }
}
## Fix a few stray fractions without either 1855-56 or 1872 data by imputing from the average of the other fractions in their tribe.
fractioninfo[is.na(fractioninfo$LivestockPP) & fractioninfo$TribeID==25,"LivestockPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LivestockPP) & fractioninfo$TribeID==25,"LivestockPP"]),3)
fractioninfo[fractioninfo$FractionID==19,"LivestockPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LivestockPP) & fractioninfo$TribeID==5,"LivestockPP"]),3)
fractioninfo[fractioninfo$FractionID==43,"LivestockPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LivestockPP) & fractioninfo$TribeID==11,"LivestockPP"]),3)
fractioninfo[fractioninfo$FractionID==58,"LivestockPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LivestockPP) & fractioninfo$TribeID==15,"LivestockPP"]),3)
 
##CamelSheepProp
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872),]
for (b in 1:length(myfractions)){
    b18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Cattle"], na.rm=TRUE)
    s18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Sheep"], na.rm=TRUE)
    g18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Goats"], na.rm=TRUE)
    h18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Horses"], na.rm=TRUE)
    md18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"MulesDonkeys"], na.rm=TRUE)
    c18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Camels"], na.rm=TRUE)
    all18551856 <- (b18551856+(s18551856*0.2)+(g18551856*0.2)+h18551856+md18551856+c18551856)
    propsc18551856 <- ((s18551856*0.2)+c18551856) / all18551856
    b1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Cattle"], na.rm=TRUE)
    s1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Sheep"], na.rm=TRUE)
    g1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Goats"], na.rm=TRUE)
    h1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Horses"], na.rm=TRUE)
    md1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"MulesDonkeys"], na.rm=TRUE)
    c1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Camels"], na.rm=TRUE)
    all1872 <- (b1872+(s1872*0.2)+(g1872*0.2)+h1872+md1872+c1872)
    propsc1872 <- ((s1872*0.2)+c1872) / all1872
    if (all18551856>0){
        ## If good 1855-56 fraction data exists, use it.
        fractioninfo[b,"CamelSheepProp"] <- round(propsc18551856,3)
    } else if (all1872>0){
        ## Otherwise use 1872 fraction data.
        fractioninfo[b,"CamelSheepProp"] <- round(propsc1872,3)
    }
}
## Fix a few stray fractions without either 1855-56 or 1872 data by imputing from the average of the other fractions in their tribe.
fractioninfo[is.na(fractioninfo$CamelSheepProp) & fractioninfo$TribeID==25,"CamelSheepProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$CamelSheepProp) & fractioninfo$TribeID==25,"CamelSheepProp"]),3)
fractioninfo[fractioninfo$FractionID==19,"CamelSheepProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$CamelSheepProp) & fractioninfo$TribeID==5,"CamelSheepProp"]),3)
fractioninfo[fractioninfo$FractionID==43,"CamelSheepProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$CamelSheepProp) & fractioninfo$TribeID==11,"CamelSheepProp"]),3)
fractioninfo[fractioninfo$FractionID==58,"CamelSheepProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$CamelSheepProp) & fractioninfo$TribeID==15,"CamelSheepProp"]),3)

##HaCultPP
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
stats185556$HaCultTMP <- NA
stats185556$HaCultTMP[!is.na(stats185556$Zouidja)] <- stats185556$Zouidja[!is.na(stats185556$Zouidja)] * 10
stats185556$HaCultTMP[is.na(stats185556$Zouidja)] <- stats185556$UnderCropHa[is.na(stats185556$Zouidja)]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872),]
for (b in 1:length(myfractions)){
    ha18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"HaCultTMP"], na.rm=TRUE)
    mytribe <- fractions[which(fractions$FractionID==myfractions[b]),"TribeID"]
    if (mytribe==30){ mytribe <- 15 }
    ha1853t <- tribespopagri[tribespopagri$TribeID==mytribe & tribespopagri$Year==1853,"Zouidja"] * 10
    cofractions <- fractioninfo[fractioninfo$TribeID==mytribe,"FractionID"]
    ha18551856t <- sum(stats185556[stats185556$FractionID %in% cofractions,"HaCultTMP"])
    fprop <- ha18551856 / ha18551856t
    ha1853f <- (ha1853t * fprop)
    happ1853f <- ha1853f / fractioninfo[fractioninfo$FractionID==myfractions[b],"TotalPop"]
    copops <- fractioninfo[fractioninfo$TribeID==mytribe,"TotalPop"]
    popprop <- fractioninfo[fractioninfo$FractionID==myfractions[b],"TotalPop"] / sum(copops)
    ha1853fp <- (ha1853t * popprop)
    happ1853fp <- ha1853fp / fractioninfo[fractioninfo$FractionID==myfractions[b],"TotalPop"]
    if (ha18551856>0){
        ## If good 1855-56 fraction data exists, use it.
        fractioninfo[b,"HaCultPP"] <- round(happ1853f,3)
    } else if (ha1853t>0){
        ## Otherwise distribute by population
        fractioninfo[b,"HaCultPP"] <- round(happ1853fp,3)
    }
}

##LWealthPP
## Two output containers
mycols <- c("FractionID", "TotalPop","Cattle","Sheep","Goats","Horses","MulesDonkeys","Camels")
fractioninfo1856 <- as.data.frame(matrix(ncol=length(mycols), nrow=length(myfractions)), stringsAsFactors=FALSE)
names(fractioninfo1856) <- mycols
fractioninfo1856$FractionID <- myfractions
fractioninfo1872<- as.data.frame(matrix(ncol=length(mycols), nrow=length(myfractions)), stringsAsFactors=FALSE)
names(fractioninfo1872) <- mycols
fractioninfo1872$FractionID <- myfractions
## Census Sets
## 1856/56
stats1856 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
for (b in 1:length(myfractions)){   
    fractioninfo1856[b,"TotalPop"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"TotalPop"])
    fractioninfo1856[b,"Cattle"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"Cattle"])
    fractioninfo1856[b,"Sheep"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"Sheep"])
    fractioninfo1856[b,"Goats"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"Goats"])
    fractioninfo1856[b,"Horses"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"Horses"])
    fractioninfo1856[b,"MulesDonkeys"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"MulesDonkeys"])
    fractioninfo1856[b,"Camels"] <- sum(stats1856[stats1856$FractionID==myfractions[b],"Camels"])
}
fractioninfo1856[fractioninfo1856$TotalPop == 0,"TotalPop"] <- NA
fractioninfo1856[fractioninfo1856$Cattle == 0 & is.na(fractioninfo1856$TotalPop),"Cattle"] <- NA
fractioninfo1856[fractioninfo1856$Sheep == 0 & is.na(fractioninfo1856$TotalPop),"Sheep"] <- NA
fractioninfo1856[fractioninfo1856$Goats == 0 & is.na(fractioninfo1856$TotalPop),"Goats"] <- NA
fractioninfo1856[fractioninfo1856$Horses == 0 & is.na(fractioninfo1856$TotalPop),"Horses"] <- NA
fractioninfo1856[fractioninfo1856$MulesDonkeys == 0 & is.na(fractioninfo1856$TotalPop),"MulesDonkeys"] <- NA
fractioninfo1856[fractioninfo1856$Camels == 0 & is.na(fractioninfo1856$TotalPop),"Camels"] <- NA
## 1872
stats1872<- fractionstats[fractionstats$Year %in% c(1872) & !is.na(fractionstats$TotalPop),]
for (b in 1:length(myfractions)){   
    fractioninfo1872[b,"TotalPop"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"TotalPop"])
    fractioninfo1872[b,"Cattle"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"Cattle"])
    fractioninfo1872[b,"Sheep"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"Sheep"])
    fractioninfo1872[b,"Goats"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"Goats"])
    fractioninfo1872[b,"Horses"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"Horses"])
    fractioninfo1872[b,"MulesDonkeys"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"MulesDonkeys"])
    fractioninfo1872[b,"Camels"] <- sum(stats1872[stats1872$FractionID==myfractions[b],"Camels"])
}
fractioninfo1872[fractioninfo1872$TotalPop == 0,"TotalPop"] <- NA
fractioninfo1872[fractioninfo1872$Cattle == 0 & is.na(fractioninfo1872$TotalPop),"Cattle"] <- NA
fractioninfo1872[fractioninfo1872$Sheep == 0 & is.na(fractioninfo1872$TotalPop),"Sheep"] <- NA
fractioninfo1872[fractioninfo1872$Goats == 0 & is.na(fractioninfo1872$TotalPop),"Goats"] <- NA
fractioninfo1872[fractioninfo1872$Horses == 0 & is.na(fractioninfo1872$TotalPop),"Horses"] <- NA
fractioninfo1872[fractioninfo1872$MulesDonkeys == 0 & is.na(fractioninfo1872$TotalPop),"MulesDonkeys"] <- NA
fractioninfo1872[fractioninfo1872$Camels == 0 & is.na(fractioninfo1872$TotalPop),"Camels"] <- NA
## Add weighted wealth
fractioninfo1856$LWealthPP <- ((fractioninfo1856$Cattle*30)+(fractioninfo1856$Sheep*5)+(fractioninfo1856$Goats*5)+(fractioninfo1856$Horses*100)+(fractioninfo1856$MulesDonkeys*100)+(fractioninfo1856$Camels*150)) / fractioninfo1856$TotalPop
fractioninfo1872$LWealthPP <- ((fractioninfo1872$Cattle*30)+(fractioninfo1872$Sheep*5)+(fractioninfo1872$Goats*5)+(fractioninfo1872$Horses*100)+(fractioninfo1872$MulesDonkeys*100)+(fractioninfo1872$Camels*150)) / fractioninfo1872$TotalPop
## Combine
names(fractioninfo1856)[-1] <- paste(names(fractioninfo1856)[-1],"1856", sep="")
names(fractioninfo1872)[-1] <- paste(names(fractioninfo1872)[-1],"1872", sep="")
df <- dfall <- merge(fractioninfo1856,fractioninfo1872,by="FractionID")
df <- df[!is.na(df$TotalPop1856) & !is.na(df$TotalPop1872),]
## Model livestock wealth using 1872
modlwealth <- step(lm(LWealthPP1856 ~ 0 + TotalPop1872+ Cattle1872+ Sheep1872+ Goats1872+ Horses1872+ MulesDonkeys1872+ Camels1872+ LWealthPP1872, data=df), trace=0)
summary(modlwealth)
## plot(predict(modlwealth), df$LWealthPP1856, type="p", pch=1, main="Livestock Wealth")
## Predict
finaldf <- dfall[,c("FractionID","LWealthPP1856","LWealthPP1872")]
finaldf$PredLWealthPP <- NA
finaldf <- finaldf[with(finaldf, order(FractionID)), ]
finaldf$PredLWealthPP <- predict(modlwealth, newdata=dfall)
finaldf$LWealthPP <- finaldf$PredLWealthPP
finaldf$Imputed <- "yes"
finaldf$LWealthPP[!is.na(finaldf$LWealthPP1856)] <- finaldf$LWealthPP1856[!is.na(finaldf$LWealthPP1856)]
finaldf$Imputed[!is.na(finaldf$LWealthPP1856)] <- "no"
finaldf$Imputed[is.na(finaldf$LWealthPP)] <- NA
finaldf <- merge(fractioninfo[,"FractionID",drop=FALSE],finaldf, by="FractionID", sort=FALSE)
fractioninfo[,"LWealthPP"] <- round(finaldf$LWealthPP,1)
## Fix a few stray fractions without either 1855-56 or 1872 data by imputing from the average of the other fractions in their tribe.
fractioninfo[is.na(fractioninfo$LWealthPP) & fractioninfo$TribeID==25,"LWealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LWealthPP) & fractioninfo$TribeID==25,"LWealthPP"]),3)
fractioninfo[fractioninfo$FractionID==19,"LWealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LWealthPP) & fractioninfo$TribeID==5,"LWealthPP"]),3)
fractioninfo[fractioninfo$FractionID==43,"LWealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LWealthPP) & fractioninfo$TribeID==11,"LWealthPP"]),3)
fractioninfo[fractioninfo$FractionID==58,"LWealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$LWealthPP) & fractioninfo$TribeID==15,"LWealthPP"]),3)

##WealthPP
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872),]
tribetrees <- tribespopagri[tribespopagri$Year==1872,]
tribetrees$BothTrees <- (tribetrees$FigTrees + tribetrees$OrangeTrees)*5
for (b in 1:length(myfractions)){
    mytribe <- fractions[which(fractions$FractionID==myfractions[b]),"TribeID"]
    cofractions <- fractioninfo[fractioninfo$TribeID==mytribe,"FractionID"]
    tribetreesb <- tribetrees[tribetrees$TribeID==mytribe,"BothTrees"]
    if (length(tribetreesb)==0){ tribetreesb <- 0 }
    fprop <- fractioninfo[fractioninfo$FractionID ==myfractions[b],"TotalPop"]/sum(fractioninfo[fractioninfo$FractionID %in% cofractions,"TotalPop"])
    fractiontreewealth <- ((tribetreesb * fprop) *5) / fractioninfo[fractioninfo$FractionID ==myfractions[b],"TotalPop"]
    oo18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"OilL"], na.rm=TRUE)
    t18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"TobaccoKg"], na.rm=TRUE)
    othwealth18551856 <- ((oo18551856*1)+(t18551856*0.5))/sum(stats185556[stats185556$FractionID==myfractions[b],"TotalPop"])
    oo1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"OilL"], na.rm=TRUE)
    t1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"TobaccoKg"], na.rm=TRUE)
    othwealth1872 <- ((oo1872*1)+(t1872*0.5))/sum(stats1872[stats1872$FractionID==myfractions[b],"TotalPop"])
    if (any(stats185556$FractionID==myfractions[b])){
        ## If good 1855-56 fraction data exists, use it.
        fractioninfo[b,"WealthPP"] <- round((fractioninfo[b, "HaCultPP"] + fractioninfo[b,"LWealthPP"] + othwealth18551856 + fractiontreewealth),1)
    } else if (any(stats1872$FractionID==myfractions[b])){
        ## Otherwise use 1872
        fractioninfo[b,"WealthPP"] <- round((fractioninfo[b, "HaCultPP"] + fractioninfo[b,"LWealthPP"] + othwealth1872 + fractiontreewealth),1)
    }
}
## Fix a few stray fractions without either 1855-56 or 1872 data by imputing from the average of the other fractions in their tribe.
fractioninfo[is.na(fractioninfo$WealthPP) & fractioninfo$TribeID==25,"WealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$WealthPP) & fractioninfo$TribeID==25,"WealthPP"]),3)
fractioninfo[fractioninfo$FractionID==19,"WealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$WealthPP) & fractioninfo$TribeID==5,"WealthPP"]),3)
fractioninfo[fractioninfo$FractionID==43,"WealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$WealthPP) & fractioninfo$TribeID==11,"WealthPP"]),3)
fractioninfo[fractioninfo$FractionID==58,"WealthPP"] <- round(mean(fractioninfo[!is.na(fractioninfo$WealthPP) & fractioninfo$TribeID==15,"WealthPP"]),3)

##LWealthProp
fractioninfo$LWealthProp <- round(fractioninfo$LWealthPP / fractioninfo$WealthPP, 3)

##TentProp
stats185556 <- fractionstats[fractionstats$Year %in% c(1855,1856) & !is.na(fractionstats$TotalPop),]
stats1872 <- fractionstats[fractionstats$Year %in% c(1872),]
for (b in 1:length(myfractions)){
    t18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Tents"], na.rm=TRUE)
    g18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Gourbis"], na.rm=TRUE)
    h18551856 <- sum(stats185556[stats185556$FractionID==myfractions[b],"Houses"], na.rm=TRUE)
    all18551856 <- sum(t18551856,g18551856,h18551856)
    propt18551856 <- t18551856 / all18551856
    t1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Tents"], na.rm=TRUE)
    g1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Gourbis"], na.rm=TRUE)
    h1872 <- sum(stats1872[stats1872$FractionID==myfractions[b],"Houses"], na.rm=TRUE)
    all1872 <- sum(t1872,g1872,h1872)
    propt1872 <- t1872 / all1872    
    if (all18551856>0){
        ## If good 1855-56 fraction data exists, use it.
        fractioninfo[b,"TentProp"] <- round(propt18551856,3)
    } else if (all1872>0){
        ## Otherwise use 1872 fraction data.
        fractioninfo[b,"TentProp"] <- round(propt1872,3)
    }
}
## Fix a few stray fractions without either 1855-56 or 1872 data by imputing from the average of the other fractions in their tribe.
fractioninfo[is.na(fractioninfo$TentProp) & fractioninfo$TribeID==25,"TentProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$TentProp) & fractioninfo$TribeID==25,"TentProp"]),3)
fractioninfo[fractioninfo$FractionID==19,"TentProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$TentProp) & fractioninfo$TribeID==5,"TentProp"]),3)
fractioninfo[fractioninfo$FractionID==43,"TentProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$TentProp) & fractioninfo$TribeID==11,"TentProp"]),3)
fractioninfo[fractioninfo$FractionID==58,"TentProp"] <- round(mean(fractioninfo[!is.na(fractioninfo$TentProp) & fractioninfo$TribeID==15,"TentProp"]),3)

##SaharaMarkets
b <- 0.15
smarkets <- markets[grepl("seasonal", markets$Type),]
smarkets <- st_as_sf(smarkets, coords=c("Longitude","Latitude"), crs=4326)
smarkets <- st_transform(smarkets, crs=32631)
fractioninfosp <- st_as_sf(fractioninfo, coords=c("Longitude","Latitude"), crs=4326)
fractioninfosp <- st_transform(fractioninfosp, crs=32631)
dijkm <- as.matrix(as.data.frame(st_distance(fractioninfosp,smarkets)) /  1000)
tmpmat <- exp(-1 * b * dijkm)
resc <- function(x){ (x-min(x))/(max(x) - min(x))}
fractioninfo$SaharaMarkets <- round(resc(rowSums(tmpmat)),6)

##WeeklyMarkets
wmarkets <- markets[grepl("weekly", markets$Type),]
wmarkets <- st_as_sf(wmarkets, coords=c("Longitude","Latitude"), crs=4326)
wmarkets <- st_transform(wmarkets, crs=32631)
dijkm <- as.matrix(as.data.frame(st_distance(fractioninfosp,wmarkets)) /  1000)
tmpmat <- exp(-1 * b * dijkm)
largesizeaccess <- rowSums(tmpmat[,wmarkets$SizeClass=="large", drop=FALSE])*5
mediumsizeaccess <- rowSums(tmpmat[,wmarkets$SizeClass=="medium", drop=FALSE])*3
smallsizeaccess <- rowSums(tmpmat[,wmarkets$SizeClass=="small", drop=FALSE])*1
uncertainsizeaccess <- rowSums(tmpmat[,wmarkets$SizeClass=="uncertain", drop=FALSE])*2
fractioninfo$WeeklyMarkets <- round(resc(largesizeaccess+mediumsizeaccess+smallsizeaccess+uncertainsizeaccess),6)

## Clean-up
fractioninfo$LWealthPP <- NULL
fractioninfo$LWealthProp <- NULL

## Write out
write.table(fractioninfo, file="data/postprep/tsv/fractions_variables.tsv", sep="\t", dec=".", row.names=FALSE, na="")
