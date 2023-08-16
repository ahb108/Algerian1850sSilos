
## PLEASE ALTER WORKING DIRECTORY
setwd("~/Documents/research/papers/AlgerianSilos/digitalarchive/Algerian1850sSilos") # MacOS

## Libraries
library(lme4)
library(MASS)
library(rstanarm)

## Load fraction data
fractioninfo <- read.table("data/postprep/tsv/fractions_variables.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
## Log-transform those variables with particularly skewed distributions and rescale all covariates to [0,1]
sfinfo <- fractioninfo
resc <- function(x){ (x-min(x))/(max(x) - min(x))}
torescale <- c("TotalPop","Makhzen","Marabout","Berber","HaCultPP","WealthPP","LivestockPP","CamelSheepProp","TentProp","SaharaMarkets","WeeklyMarkets","AnnPrec","CalcThick","Longitude","Latitude")
sfinfo$TotalPop <- log(sfinfo$TotalPop)
sfinfo$SaharaMarkets <- log(sfinfo$SaharaMarkets+0.01)
sfinfo$WealthPP <- log(sfinfo$WealthPP)
sfinfo$LivestockPP <- log(sfinfo$LivestockPP)
sfinfo$HaCultPP <- log(sfinfo$HaCultPP)
for (a in 1:length(torescale)){
    sfinfo[,torescale[a]] <- resc(sfinfo[,torescale[a]])
}
summary(sfinfo)
## Export the covariate correlation table
predcors <- round(cor(sfinfo[!is.na(sfinfo$SiloCount1853),7:19]),3)
diag(predcors) <- NA
predcors[upper.tri(predcors)] <- NA
predcors <- as.data.frame(predcors)
predcors <- cbind(row.names(predcors),predcors)
names(predcors)[1] <- "Covariates"
write.table(predcors, file="outputs/tsv/covariate_correlations.tsv", sep="\t", dec=".", row.names=FALSE, na="")

## Load silos sites 
silos <- read.table("data/raw/tsv/fractions_silos.tsv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE, sep="\t", dec=".")
silos <- silos[silos$Year %in% c(1853,1854),]
silos <- silos[silos$FractionID %in% unique(fractioninfo$FractionID),]
silos <- silos[!is.na(silos$SiloCount),]
silosinfo <- merge(silos[,c("Year","FractionID","PlaceName","SiteID","SiloCount")], sfinfo[,c("FractionID","TribeID","TotalPop","Makhzen","Marabout","Berber","HaCultPP","WealthPP","LivestockPP","CamelSheepProp","TentProp","SaharaMarkets","WeeklyMarkets","AnnPrec","CalcThick")], by="FractionID")

############################################################ 

## 1853 ##
## Poisson
mod1 <- glmer(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick + (1|TribeID/FractionID), data=silosinfo[silosinfo$Year==1853,], family="poisson", control=glmerControl(optimizer="bobyqa"))
summary(mod1)
## Overdispersion
rdf <- df.residual(mod1)
Pearson.chisq <- sum(residuals(mod1,type="pearson")^2)
od1 <- c(chisq=Pearson.chisq,ratio=Pearson.chisq/rdf,rdf=rdf,p=pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE))
print(od1)
## Quasi-poisson
mod2 <- glmmPQL(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick, random= ~1 | TribeID/FractionID, family=quasipoisson(link="log"), data=silosinfo[silosinfo$Year==1853,])
summary(mod2)
## Negative binomial
mod3 <- glmer.nb(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick + (1 | TribeID/FractionID), data=silosinfo[silosinfo$Year==1853,], control=glmerControl(optimizer="bobyqa"))
summary(mod3)
## Negative binomial via Bayesian inference
mod3stan <- stan_glmer(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick+(1 | TribeID/FractionID), data=silosinfo[silosinfo$Year==1853,], family=neg_binomial_2(link="log"), seed=123)
mod3stan

## 1854 ##
## Negative binomial
mod3a <- glmer.nb(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick + (1 | TribeID/FractionID), data=silosinfo[silosinfo$Year==1854,], control=glmerControl(optimizer="bobyqa"))
summary(mod3a)
## Does not converge.
## Negative binomial via Bayesian inference
mod3astan <- stan_glmer(SiloCount ~ TotalPop + Makhzen + Marabout + Berber + HaCultPP + WealthPP + LivestockPP + CamelSheepProp + TentProp + SaharaMarkets + WeeklyMarkets + AnnPrec + CalcThick+(1 | TribeID/FractionID), data=silosinfo[silosinfo$Year==1854,], family=neg_binomial_2(link="log"), seed=123)
mod3astan

## Assessment of quasi-Poisson vs Negative Binomial models (see Ver Hoef and Boveng 2007)
## quasi-Poisson
p3rmean <- c(5,15,25,35,45,55,70,90,125,175)
p3thresh <- c(10,20,30,40,50,60,80,100,150)
grqp <- rep(p3rmean[1],length(silosinfo[silosinfo$Year==1853,"SiloCount"]))
grqp[exp(fitted(mod2))>p3thresh[1]] <- p3rmean[2]
grqp[exp(fitted(mod2))>p3thresh[2]] <- p3rmean[3]
grqp[exp(fitted(mod2))>p3thresh[3]] <- p3rmean[4]
grqp[exp(fitted(mod2))>p3thresh[4]] <- p3rmean[5]
grqp[exp(fitted(mod2))>p3thresh[5]] <- p3rmean[6]
grqp[exp(fitted(mod2))>p3thresh[6]] <- p3rmean[7]
grqp[exp(fitted(mod2))>p3thresh[7]] <- p3rmean[8]
grqp[exp(fitted(mod2))>p3thresh[8]] <- p3rmean[9]
grqp[exp(fitted(mod2))>p3thresh[9]] <- p3rmean[10]
p3vars <- numeric(0)
sqrp <- (silosinfo[silosinfo$Year==1853,"SiloCount"]-exp(fitted(mod2)))^2
for(i in 1:length(p3rmean)){
    p3vars[i] <- mean(sqrp[grqp==p3rmean[i]])
}
p3means <- by(exp(fitted(mod2)),as.factor(grqp),mean)
## negative binomial
grb <- rep(p3rmean[1],length(silosinfo[silosinfo$Year==1853,"SiloCount"]))
grb[fitted(mod3)>p3thresh[1]] <- p3rmean[2]
grb[fitted(mod3)>p3thresh[2]] <- p3rmean[3]
grb[fitted(mod3)>p3thresh[3]] <- p3rmean[4]
grb[fitted(mod3)>p3thresh[4]] <- p3rmean[5]
grb[fitted(mod3)>p3thresh[5]] <- p3rmean[6]
grb[fitted(mod3)>p3thresh[6]] <- p3rmean[7]
grb[fitted(mod3)>p3thresh[7]] <- p3rmean[8]
grb[fitted(mod3)>p3thresh[8]] <- p3rmean[9]
grb[fitted(mod3)>p3thresh[9]] <- p3rmean[10]
b3vars <- numeric(0)
sqrb <- (silosinfo[silosinfo$Year==1853,"SiloCount"]-fitted(mod3))^2
for(i in 1:length(p3rmean)){
    b3vars[i] <- mean(sqrb[grb==p3rmean[i]])
}
b3means <- by(fitted(mod3),as.factor(grb),mean)
## Plot
dev.new(device=pdf, height=5, width=6)
par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
plot(p3means,p3vars,xlim=c(0,225), ylim=c(0,45000),xlab="Fitted means",ylab="Variance", col="blue")
points(p3means,p3vars,type="l", col="blue")
points(b3means,b3vars,col="red")
points(b3means,b3vars,type="l",col="red")
legend(10, 40000,c("quasi-Poisson","negative binomial"),col=c("blue","red"),pch=1,lty=1, bty="n", title="", title.font=2)

## Prioritise the negative binomial regression result from 1853 for the main text and export it.
coefdf <- as.data.frame(coef(summary(mod3)))
coefdf[,"z value"] <- NULL
coefdf <- cbind(row.names(coefdf), coefdf)
rownames(coefdf) <- NULL
names(coefdf) <- c("Variable", "Estimate","Std.Error","p")
coefdf$Estimate <- round(coefdf$Estimate, 3)
coefdf$Std.Error <- round(coefdf$Std.Error, 3)
coefdf$p <- round(coefdf$p, 3)
coefdf
## Write out
write.table(coefdf, file="outputs/tsv/regression.tsv", sep="\t", dec=".", row.names=FALSE, na="")





