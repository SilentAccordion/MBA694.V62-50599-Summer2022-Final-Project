library(knitr)
library(mosaic)
library(psych)
library(car)
library(MuMIn)
library(tidyverse)

options(na.action="na.omit")
recs2015data <- read_csv("./recs2015_public_v4.csv")


require(parallel)
require(snow)
# https://stackoverflow.com/q/55858799
#detects number of cores available to use for parallel package
nCores <- detectCores(logical = FALSE)
# cat(nCores, " cores detected.")  

# detect threads with parallel()
nThreads<- detectCores(logical = TRUE)
# cat(nThreads, " threads detected.")

clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"

clust <- try(makeCluster(getOption("cl.cores", 11), type = clusterType))

allVariables <- c("HHAGE","VCR","PLAYSTA","TVSIZE1","TVCOLOR","CROCKPOT","TOAST","OUTGRILL","MICRO")

recs2015data_subset <- recs2015data[,allVariables]

# Remove rows where there is any missing data
recs2015_subset_complete <- na.omit(recs2015data_subset) 
# Now refit the reduced model with only complete cases
reducedModel_comp <- lm(HHAGE ~ VCR + PLAYSTA + TVSIZE1 +  
                          TVCOLOR +  CROCKPOT + 
                          TOAST + OUTGRILL + MICRO, 
                        data=recs2015_subset_complete)
options(na.action="na.fail", width=120)
# aicOutput <- dredge(fullModel_comp, rank="AIC", 
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# kable(head(aicOutput,n=15),caption="YOUR Caption here")


clusterExport(clust, "recs2015_subset_complete")

aicOutput_reduced <- dredge(reducedModel_comp, cluster = clust, rank="AIC",
                            extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared)
head(aicOutput_reduced,n=15)
kable(head(aicOutput_reduced,n=15),caption="YOUR Caption here")

kable(summary(reducedModel_comp)$coefficients,caption="Table 15: Reduced Model Coefficients")




######################


allVariables <- c("HHAGE","VCR","PLAYSTA","TVSIZE1","TVCOLOR","CROCKPOT","TOAST","OUTGRILL","MICRO","SWIMPOOL",
                  "RECBATH",
                  "OVENUSE",
                  "COFFEE",
                  "FOODPROC",
                  "RICECOOK",
                  "BLENDER",
                  "DVD",
                  "INTSTREAM",
                  "TVAUDIOSYS",
                  "NUMTABLET")




recs2015data_subset <- recs2015data[,allVariables]

# Remove rows where there is any missing data
recs2015_subset_complete <- na.omit(recs2015data_subset) 
# Now refit the reduced model with only complete cases
reducedModel_comp <- lm(HHAGE ~ VCR + PLAYSTA + TVSIZE1 +  
                          TVCOLOR +  CROCKPOT + 
                          TOAST + OUTGRILL + MICRO + SWIMPOOL + RECBATH + OVENUSE + COFFEE + FOODPROC + RICECOOK + BLENDER + DVD + INTSTREAM + TVAUDIOSYS + NUMTABLET, 
                        data=recs2015_subset_complete)
options(na.action="na.fail", width=120)
# aicOutput <- dredge(fullModel_comp, rank="AIC", 
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# kable(head(aicOutput,n=15),caption="YOUR Caption here")


clusterExport(clust, "recs2015_subset_complete")

aicOutput_reduced <- dredge(reducedModel_comp, cluster = clust, rank="AIC",
                            extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared)
head(aicOutput_reduced,n=15)
kable(head(aicOutput_reduced,n=15),caption="YOUR Caption here")


######################


allVariables <- c("HHAGE","VCR","PLAYSTA","TVSIZE1","TVCOLOR","CROCKPOT","TOAST","OUTGRILL","MICRO","SWIMPOOL",
                  "RECBATH",
                  "OVENUSE",
                  "COFFEE",
                  "FOODPROC",
                  "RICECOOK",
                  "BLENDER",
                  "DVD",
                  "INTSTREAM",
                  "TVAUDIOSYS",
                  "NUMTABLET")




recs2015data_subset <- recs2015data[,allVariables]

# Remove rows where there is any missing data
recs2015_subset_complete <- na.omit(recs2015data_subset) 
# Now refit the reduced model with only complete cases
reducedModel_comp <- lm(HHAGE ~ VCR + PLAYSTA + TVSIZE1 +  
                          TVCOLOR +  CROCKPOT + 
                          TOAST + OUTGRILL + MICRO + SWIMPOOL + RECBATH + OVENUSE + COFFEE + FOODPROC + RICECOOK + BLENDER + DVD + INTSTREAM + TVAUDIOSYS + NUMTABLET, 
                        data=recs2015_subset_complete)
options(na.action="na.fail", width=120)
# aicOutput <- dredge(fullModel_comp, rank="AIC", 
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# kable(head(aicOutput,n=15),caption="YOUR Caption here")


clusterExport(clust, "recs2015_subset_complete")

aicOutput_reduced <- dredge(reducedModel_comp, cluster = clust, rank="AIC",
                            extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared)
head(aicOutput_reduced,n=15)
kable(head(aicOutput_reduced,n=15),caption="YOUR Caption here")




######################


allVariables <- c("HHAGE","VCR","PLAYSTA","TVCOLOR","CROCKPOT")




recs2015data_subset <- recs2015data[,allVariables]

# Remove rows where there is any missing data
recs2015_subset_complete <- na.omit(recs2015data_subset) 
# Now refit the reduced model with only complete cases
reducedModel_comp <- lm(HHAGE ~ VCR + PLAYSTA + TVCOLOR +  CROCKPOT, 
                        data=recs2015_subset_complete)
options(na.action="na.fail", width=120)
# aicOutput <- dredge(fullModel_comp, rank="AIC", 
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# kable(head(aicOutput,n=15),caption="YOUR Caption here")


clusterExport(clust, "recs2015_subset_complete")

aicOutput_reduced <- dredge(reducedModel_comp, cluster = clust, rank="AIC",
                            extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
# extra=c("R^2",adjRsq=function(x) summary(x)$adj.r.squared)
head(aicOutput_reduced,n=15)
kable(head(aicOutput_reduced,n=15),caption="YOUR Caption here")






