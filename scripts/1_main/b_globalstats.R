
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") # MacOS

## Load data
silos <- read.table("data/raw/tsv/fractions_silos.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
silos <- silos[!is.na(silos$SiloCount),]
tribes <- read.table("data/raw/tsv/tribes.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")

## Estimated average silos in the whole region
silos1853 <- silos[silos$Year == 1853, ]
silos1854 <- silos[silos$Year == 1854, ]
silotribes1853 <- unique(silos1853$TribeID)
silotribes1854 <- unique(silos1854$TribeID)
missingtribes1853 <- tribes[!tribes$TribeID %in% silotribes1853 & tribes$TribeID %in% silotribes1854,c("TribeID", "Tribe")]
missingtribes1854 <- tribes[!tribes$TribeID %in% silotribes1854 & tribes$TribeID %in% silotribes1853,c("TribeID", "Tribe")]
missingtribes1853 # Sahari Ouled Brahim, Ouled Sidi Aïssa el Adab
missingtribes1854 # Gherib, Ouamri, Beni Bou Yacoub (and Ouled Sidi Nadji now mergedwith Oulad Deid)
missingtribes1854nadj <- missingtribes1854[missingtribes1854$TribeID != 30,"TribeID"]
## Ignore missing 1853 tribes as they are far southern tribes that are only rarely included in Medea province and for which there is no 1853 achour information.
## % Change for tribes with overlap
finalsum1853 <- sum(silos1853$SiloCount)
s1853overlap <- sum(silos1853[!silos1853$TribeID %in% missingtribes1854nadj,"SiloCount"])
s1854overlap <- sum(silos1854[silos1854$TribeID %in% silotribes1853,"SiloCount"])
rawchange <- s1853overlap - s1854overlap
perchange <- (rawchange / s1853overlap) * 100
rawchange ## 964 more silos in 1853 where tribes overlap
perchange ## 4.8% more silos in 1853 where tribes overlap

## Now estimate th 1854 count for the missing tribes and create a final rconstructed 1854 count.
wt <- 1 - (rawchange / s1853overlap)
reconmissing1854silos <- round((sum(silos1853[silos1853$TribeID %in% missingtribes1854nadj,"SiloCount"]) * wt),0)
finalreconsum1854 <- s1854overlap + reconmissing1854silos
finalsum1853 #22959
s1854overlap #18988
finalreconsum1854 #21850

## Hence implied silo pit size assuming the whole harvest went into these pits
35000000/22000 #lower estimate
40000000/22000 #higher estimate
