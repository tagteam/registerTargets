### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:37) 
## Version: 
## Last-Updated: Dec 11 2022 (18:47) 
##           By: Thomas Alexander Gerds
##     Update #: 15
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(targets)
library(data.table)
setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/data_creator/")
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("../ami/functions/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("../ami/secrets/",pattern = "R$",full.names = TRUE)){source(f)}
tar_make()
tar_load_everything()
setkey(cpr,pnr)
setkey(lmdb,pnr)
setkey(lpr,pnr)
setkey(ami_pop,pnr)
cpr[,end_fup := NULL]
lpr[,end_fup := NULL]
## Cdrugs <- grep("^C10AA|^C07|^C08",heaven::atccodes$ATC,value = TRUE)
Cdrugs <- grep("^C07",heaven::atccodes$ATC,value = TRUE)
# young female without event
tmp <- secret_baseline_pop
tmp <- tmp[sex == "Female"&age<70&event == 0]
tmp[,M := 3]
lmdb_plus  <- tmp[sample(1:.N,size = 0.3*.N,replace = FALSE),{
    eksd = index - rbinom(M, 1, 0.95)*runif(M,0,365)
    atc = sample(Cdrugs,size=M,replace=TRUE)
    data.table(eksd = eksd,atc = atc)
},by = pnr]
lmdb <- rbind(lmdb,lmdb_plus)
# old male with event
tmp <- secret_baseline_pop
tmp <- tmp[sex == "Male"&age>65&event == 1]
tmp[,M := 3]
lmdb_plus  <- tmp[sample(1:.N,size = 0.3*.N,replace = FALSE),{
    eksd = index - rbinom(M, 1, 0.95)*runif(M,0,365)
    atc = sample(Cdrugs,size=M,replace=TRUE)
    data.table(eksd = eksd,atc = atc)
},by = pnr]
lmdb <- rbind(lmdb,lmdb_plus)
setkey(lmdb,pnr,eksd)
message("saving in amis/rawdata folder")
fwrite(cpr,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/cpr.csv",quote=TRUE)
fwrite(lpr,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/lpr.csv",quote=TRUE)
fwrite(lmdb,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/lmdb.csv",quote=TRUE)
#

setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami")
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("secrets/",pattern = "R$",full.names = TRUE)){source(f)}
tar_make(script = "secret_targets.R")
tar_load_everything()
secret_baseline_pop[,table(bb,event)]
## secret_baseline_pop[pnr == "99955",.(pnr,index,time,event,bb)]
## lmdb_plus[pnr == "99955"]
## lmdb[pnr == "99955"]

fit <- coxph(Surv(time,event)~bb,data = secret_baseline_pop)
fit
hazard_ratio


## day 2
setwd("/home/tag/metropolis/Teaching/targetedRegisterAnalysis/exercises/spaghetti/")
pop <- fread("popami.csv",keepLeadingZeros = TRUE,
             colClasses = c("character","character",rep("Date",5),"numeric"))
pop[,time := as.numeric(end_fup-index)/365.25]
pop[,event := 0]
pop[!is.na(death_date),event := 1]
library(prodlim)
fit <- prodlim(Hist(time,event)~sex,data = pop)
plot(fit)
u = summary(fit,percent = TRUE,surv = FALSE,times = c(1,5,10))[,c("time","sex","cuminc")]
setDT(u)
print(dcast(u,sex~time,value.var = "cuminc",fun = mean))

pop[,time := as.numeric(end_fup-index)/365.25]
pop[,event := 0]
pop[!is.na(death_date),event := 1]
library(prodlim)
fit <- prodlim(Hist(time,event)~sex,data = pop)
summary(fit,times = c(1,5,10))[,c("sex","surv")]
plot(fit)
     ## sex  surv
## 1 Female 0.922
## 2 Female 0.725
## 3 Female 0.592
## 4   Male 0.909
## 5   Male 0.696
## 6   Male 0.562



######################################################################
### run_targets.R ends here
