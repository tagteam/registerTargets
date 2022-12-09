### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:00) 
## Version: 
## Last-Updated: Dec  9 2022 (06:35) 
##           By: Thomas Alexander Gerds
##     Update #: 77
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
library(heaven)
library(data.table)
for (f in list.files("functions/",pattern = "R$",recursive = TRUE,full.names = TRUE)){source(f)}
list(
    tar_target(N,99999),
    tar_target(adm,{
        adm <- data.table(pnr = 1:N)
        adm[,M := rbinom(N,5,.4)+rpois(N,.7)]
        adm <- adm[,.(pnr = rep(pnr,M))]
        adm[, diag:= sample(c(rep(grep(c("DI2[12]"),heaven::icdcodes$diag,value = TRUE),10),heaven::icdcodes$diag),size = .N,replace = TRUE)]
        adm[, inddto:= as.Date("2000-01-01")+runif(.N,0,8381)]
        make_pnr(adm,len=nchar(as.character(N)))
        setkey(adm,pnr)
        adm[]
    }),
    tar_target(ami_pop,{
        ami_pop <- adm[grep("DI2[12]",diag)]
        ami_pop <- ami_pop[ami_pop[,.I[1],by="pnr"]$V1]
        ami_pop <- ami_pop[,.(pnr,inddto)]
        ami_pop[,.(pnr = pnr,index = inddto)]
    }),
    tar_target(not_ami_pop,{
        not_ami <- adm[!(pnr%in%ami_pop$pnr)]
        not_ami <- not_ami[not_ami[,.I[1],by="pnr"]$V1]
        not_ami[]
    }),
    tar_target(demo,{
        demo <- data.table(pnr=1:N,
                           sex=rbinom(N,1,.42),
                           birth_date=as.Date("1950-01-01")+runif(N,-365.25*20,365.25*20))
        demo[,death_date:= birth_date+(50+rnorm(N,mean = 20,sd = 5))*365.25]
        emigration <- rbinom(N,1,.01)
        demo[,emigration_date:= death_date-(rnorm(N,mean = 10,sd = 5))*365.25]
        demo[emigration == FALSE,emigration_date := NA]
        demo[death_date>as.Date("2022-12-12"),death_date := NA]
        demo[emigration_date>as.Date("2022-12-12"),emigration_date := NA]
        demo[emigration_date >= death_date,emigration_date := NA]
        demo[emigration_date<death_date,death_date := NA]
        demo[,end_fup := pmin(emigration_date,death_date,as.Date("2022-12-12"),na.rm = TRUE)]
        make_pnr(demo,len=nchar(as.character(N)))
        demo[]
    }),
    tar_target(lmdb,{
        drugs <- c(c(grep("C10AA",atccodes$ATC,value = TRUE)),atccodes[sample(1:.N,size=13)]$ATC)
        tmp_ami <- ami_pop[,.(pnr,index,M = sample(c(rep(0,10),1:20),size = NROW(ami_pop),replace = TRUE))]
        tmp_not <- not_ami_pop[,.(pnr,index = as.Date("1995-01-01"),M = sample(c(rep(0,30),1:20),size = NROW(not_ami_pop),replace = TRUE))]
        tmp_pop <- rbind(tmp_not,tmp_ami,fill = TRUE)
        setkey(demo,pnr)
        setkey(tmp_pop,pnr)
        tmp_pop <- demo[,.(pnr,end_fup)][tmp_pop]
        tmp_pop[,range := (end_fup-index)]
        tmp_pop[range<0,M := 0]
        lmdb  <- tmp_pop[,{
            eksd = index + rbinom(M, 1, 0.95)*runif(M,0,range)
            atc = sample(drugs,size=M,replace=TRUE)
            data.table(eksd = eksd,atc = atc)
        },by = pnr]
        setkey(lmdb,pnr)
        lmdb[]
    }),
    tar_target(output,{
        setkey(demo,pnr)
        setkey(lmdb,pnr)
        setkey(adm,pnr)
        setkey(ami_pop,pnr)
        demo[,end_fup := NULL]
        fwrite(demo,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/cpr.csv",quote=TRUE)
        fwrite(adm,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/lpr.csv",quote=TRUE)
        fwrite(lmdb,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/ami/rawdata/lmdb.csv",quote=TRUE)
    })
)

######################################################################
### _targets.R ends here
