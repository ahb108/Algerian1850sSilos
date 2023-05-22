
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") # MacOS

## Load data
sites <- read.table("data/raw/tsv/sites_toponyms.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
sites1853 <- sites[!is.na(sites$SiloCount1853),]
names(sites1853)
sites1853$TotalFeatures <- rowSums(sites1853[,13:73], na.rm=TRUE)
sites1853tp <- sites1853[sites1853$TotalFeatures > 0,]
sites1853tp$Region <- NA
sort(unique(sites1853tp$TribeID))
sites1853tp$Region[sites1853tp$TribeID %in% c(21:26,28:29)] <- "North Tell"
sites1853tp$Region[sites1853tp$TribeID %in% c(5,6,11:20,30)] <- "South Tell"
sites1853tp$Region[sites1853tp$TribeID %in% c(3,7,8)] <- "High Plains"
sites1853tp$Region[sites1853tp$TribeID %in% c(27)] <- "East Chelif"
sites1853tp <- sites1853tp[sites1853tp$Region %in% c("North Tell","South Tell"),]
sites1853tp1to50 <- sites1853tp[sites1853tp$SiloCount1853 < 51,]
sites1853tp51to580 <- sites1853tp[sites1853tp$SiloCount1853 > 50,]

## Create regrouped contingency table
mycols <- c("Group", "Keywords", "North Tell", "South Tell", "Total", "ST1to50", "ST51to580")
mygroups <- c("Orchards and Vines", "Other Trees", "Inhabited Places: Houses and Gourbis", "Inhabited Places: Winter Village", "Hay and Threshing", "Hagionyms/ Holy Places", "Inhabited Places: Estates and Larger Farms","Escarpments, Mountains, Plateaux", "Plains and Prairies", "Routeways", "Wells, Fountains, Cisterns, Small Dams","Markets and Assembly Places", "Natural Hydrology", "Small Hills and Outcrops", "Political and Honorific Titles", "Other Features")
myctab <- as.data.frame(matrix(ncol=length(mycols), nrow=length(mygroups)))
names(myctab) <- mycols

## Add non count information
##
myctab$Group <- mygroups
myctab[1,"Keywords"] <- paste0(c("amandier", "figuier", "lemonnier", "grenadier", "terebinthe", "caroubier", "olivier", "vigne"), collapse=", ")
chkcols <- c("Vines","LemonTree","AlmondTree","OliveTree","FigTree","Terebinth","PomegranateTree","CarobTree")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[1,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[1,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[1,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[1,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[1,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[2,"Keywords"] <- paste0(c("chêne","peuplier","tremble", "orme"), collapse=", ")
chkcols <- c("OakTree","ElmTree","PoplarTree","UndiffTree")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[2,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[2,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[2,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[2,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[2,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[3,"Keywords"] <- paste0(c("gourbi","gourbis","maison","dechera","bit", "bordj"), collapse=", ")
chkcols <- c("House","Gourbi","GourbiMulti", "Dechera", "Bordj")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[3,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[3,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[3,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[3,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[3,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[4,"Keywords"] <- paste0(c("mechta"), collapse=", ")
chkcols <- c("Mechta")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[4,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[4,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[4,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[4,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[4,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[5,"Keywords"] <- paste0(c("noueder", "paille", "grange"), collapse=", ")
chkcols <- c("TheshingFloors","Grange","HayStraw")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[5,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[5,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[5,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[5,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[5,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[6,"Keywords"] <- paste0(c("marabout", "sidi", "kadi","mosquée", "cimetière"), collapse=", ")
chkcols <- c("Marabout","Sidi","Mosque","Kadi", "Cemetery")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[6,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[6,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[6,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[6,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[6,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[7,"Keywords"] <- paste0(c("haouch", "ferme", "beylik"), collapse=", ")
chkcols <- c("Haouch","Farm","Beylik")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[7,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[7,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[7,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[7,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[7,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[8,"Keywords"] <- paste0(c("escarpement", "montagne", "plateau", "kef", "dera/draa","kala"), collapse=", ")
chkcols <- c("Escarpment","Mountain","Plateau")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[8,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[8,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[8,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[8,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[8,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[9,"Keywords"] <- paste0(c("plaine", "prairie", "plateau", "debdeb", "daiyra", "merdja"), collapse=", ")
chkcols <- c("Plain","Prairie","DryHardGround", "Depression")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[9,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[9,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[9,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[9,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[9,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[10,"Keywords"] <- paste0(c("chemin", "route", "faïdja", "firdjen", "regba", "teniet"), collapse=", ")
chkcols <- c("MajorRoute","Path","Pass", "Depression")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[10,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[10,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[10,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[10,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[10,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[11,"Keywords"] <- paste0(c("puits", "citerne", "fontaine", "bir", "sed"), collapse=", ")
chkcols <- c("WellCistern","Fontaine", "SmallDam")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[11,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[11,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[11,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[11,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[11,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[12,"Keywords"] <- paste0(c("marché", "djema", "madjlès"), collapse=", ")
chkcols <- c("Market","AssemblyPlace")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[12,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[12,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[12,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[12,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[12,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[13,"Keywords"] <- paste0(c("rivière", "ruisseau", "ravine", "source", "aïn", "nahr", "chabet", "oued", "kheneg", "fawar", "fiadh", "hania"), collapse=", ")
chkcols <- c("River","Stream","Ravine","Spring", "Wadi")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[13,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[13,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[13,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[13,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[13,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[14,"Keywords"] <- paste0(c("mamelon", "rocher", "coudiet"), collapse=", ")
chkcols <- c("Hill","Rock")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[14,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[14,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[14,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[14,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[14,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[15,"Keywords"] <- paste0(c("Agha", "Caïd", "Cheikh", "Chaouch"), collapse=", ")
chkcols <- c("Agha","Caïd", "Cheikh", "Chaouch")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[15,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[15,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[15,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[15,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[15,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))
##
myctab[16,"Keywords"] <- paste0(c("ruines", "cave", "terres","matmora", "kherba", "bordj"), collapse=", ")
chkcols <- c("Ruin","Cave","Fields","Matmora","Bordj")
tmp <- sites1853tp[,chkcols, drop=FALSE]
myctab[16,"Total"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="North Tell",chkcols, drop=FALSE]
myctab[16,"North Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp[sites1853tp$Region=="South Tell",chkcols, drop=FALSE]
myctab[16,"South Tell"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp1to50[sites1853tp1to50$Region=="South Tell",chkcols, drop=FALSE]
myctab[16,"ST1to50"] <- sum(colSums(tmp,na.rm=TRUE))
tmp <- sites1853tp51to580[sites1853tp51to580$Region=="South Tell",chkcols, drop=FALSE]
myctab[16,"ST51to580"] <- sum(colSums(tmp,na.rm=TRUE))

## Add percentages, standardised residuals and column totals
myctab$"% North" <- round(myctab[,"North Tell"] / myctab[,"Total"] * 100,1)
X2 <- chisq.test(myctab[ ,c("North Tell","South Tell")])
X2
myctab$StdResid <- round(X2$residuals,2)[,1]
myctab <- myctab[with(myctab, order(-StdResid)), ]

## Binomial tests on those South Tell sets with more obvious divergence of smallr and larger-sized silo groups
c(myctab[myctab$Group=="Escarpments, Mountains, Plateaux","ST51to580"],myctab[myctab$Group=="Escarpments, Mountains, Plateaux","ST1to50"])
binom.test(c(myctab[myctab$Group=="Escarpments, Mountains, Plateaux","ST51to580"],myctab[myctab$Group=="Escarpments, Mountains, Plateaux","ST1to50"]), p=sum(myctab$ST51to580)/(sum(myctab$ST1to50)+sum(myctab$ST51to580)))
c(myctab[myctab$Group=="Routeways","ST51to580"],myctab[myctab$Group=="Routeways","ST1to50"])
binom.test(c(myctab[myctab$Group=="Routeways","ST51to580"],myctab[myctab$Group=="Routeways","ST1to50"]), p=sum(myctab$ST51to580)/(sum(myctab$ST1to50)+sum(myctab$ST51to580)))

## Write out
write.table(myctab, file="outputs/tsv/tell_toponyms1853.tsv", sep="\t", dec=".", row.names=FALSE, na="")
