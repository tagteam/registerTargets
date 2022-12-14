### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:00) 
## Version: 
## Last-Updated: Dec 11 2022 (17:43) 
##           By: Thomas Alexander Gerds
##     Update #: 138
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
    tar_target(cpr,{
        cpr <- data.table(pnr=1:N,
                          sex=rbinom(N,1,.42),
                          birth_date=as.Date("1950-01-01")+runif(N,-365.25*20,365.25*20))
        cpr[,death_date:= birth_date+(50+rnorm(N,mean = 20,sd = 5))*365.25]
        # cheat a little
        cpr[death_date <= as.Date("1995-01-01"),death_date := death_date+20*365.25]
        emigration <- rbinom(N,1,.01)
        cpr[,emigration_date:= death_date-(rnorm(N,mean = 10,sd = 5))*365.25]
        # cheat a little more
        cpr[emigration_date <= as.Date("1995-01-01"),emigration_date := NA]
        cpr[emigration == FALSE,emigration_date := NA]
        # protect women
        cpr[sex == 0 & death_date <= as.Date("2005-01-01"),death_date := death_date+runif(.N,0,365.25*1.5)]
        cpr[sex == 0 & death_date>as.Date("2005-01-01"),death_date := death_date+runif(.N,0,365.25*2.5)]
        cpr[death_date>as.Date("2022-12-12"),death_date := NA]
        cpr[emigration_date>as.Date("2022-12-12"),emigration_date := NA]
        cpr[emigration_date >= death_date,emigration_date := NA]
        cpr[emigration_date<death_date,death_date := NA]
        cpr[,end_fup := pmin(emigration_date,death_date,as.Date("2022-12-12"),na.rm = TRUE)]
        make_pnr(cpr,len=nchar(as.character(N)))
        cpr[]
    }),
    tar_target(lpr,{
        lpr <- data.table(pnr = 1:N)
        make_pnr(lpr,len=nchar(as.character(N)))
        lpr[,M := rbinom(N,5,.4)+rpois(N,.7)]
        lpr <- lpr[,.(pnr = rep(pnr,M))]
        setkey(cpr,pnr)
        setkey(lpr,pnr)
        lpr = cpr[,.(pnr = pnr,end_fup = end_fup)][lpr]
        lpr[, diag:= sample(c(rep(grep(c("DI2[12]"),heaven::icdcodes$diag,value = TRUE),10),heaven::icdcodes$diag),size = .N,replace = TRUE)]
        lpr[, inddto:= end_fup-runif(.N,0,8381)]
        # cheating is fun
        lpr[inddto <= as.Date("1995-01-01"),inddto := inddto+20*365.25]
        lpr[inddto>end_fup,inddto := end_fup-runif(.N,1,1000)]
        make_pnr(lpr,len=nchar(as.character(N)))
        setkey(lpr,pnr)
        lpr[]
    }),
    tar_target(ami_pop,{
        ami_pop <- lpr[grep("DI2[12]",diag)]
        ami_pop <- ami_pop[ami_pop[,.I[1],by="pnr"]$V1]
        ami_pop <- ami_pop[,.(pnr,inddto)]
        ami_pop[,.(pnr = pnr,index = inddto)]
    }),
    tar_target(not_ami_pop,{
        not_ami <- lpr[!(pnr%in%ami_pop$pnr)]
        not_ami <- not_ami[not_ami[,.I[1],by="pnr"]$V1]
        not_ami[]
    }),
    tar_target(lmdb,{
        ## drugs <- c(c(grep("C10AA",atccodes$ATC,value = TRUE)),atccodes[sample(1:.N,size=13)]$ATC)
        drugs <- c(rep(c(grep("^C10AA|^C07|^C08",heaven::atccodes$ATC,value = TRUE)),10),heaven::atccodes$ATC)
        tmp_ami <- ami_pop[,.(pnr,start = as.Date("1995-01-01"),index,M = sample(c(rep(0,10),1:20),size = NROW(ami_pop),replace = TRUE))]
        tmp_not <- not_ami_pop[,.(pnr,start = as.Date("1995-01-01"),index = as.Date(NA),M = sample(c(rep(0,30),1:20),size = NROW(not_ami_pop),replace = TRUE))]
        tmp_pop <- rbind(tmp_not,tmp_ami,fill = TRUE)
        setkey(cpr,pnr)
        setkey(tmp_pop,pnr)
        tmp_pop <- cpr[,.(pnr,end_fup)][tmp_pop]
        tmp_pop[,range := as.numeric(end_fup-start)]
        tmp_pop[range<0,M := 0]
        lmdb  <- tmp_pop[M>0,{
            eksd = start + rbinom(M, 1, 0.95)*runif(M,0,range)
            atc = sample(drugs,size=M,replace=TRUE)
            data.table(eksd = eksd,atc = atc)
        },by = pnr]
        setkey(lmdb,pnr)
        lmdb[]
    })
)

######################################################################
### _targets.R ends here
