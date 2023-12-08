### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:00) 
## Version: 
## Last-Updated: Dec  8 2023 (08:10) 
##           By: Thomas Alexander Gerds
##     Update #: 175
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
library(lava)
library(data.table)
try(setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/data_creator/"))
tar_source("functions")
## for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){print(f);source(f)}
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
    }),
    # simulate data alike stroke outcome dual treatment GS analysis
    tar_target(coefs,{
        source("input/coefs.txt")
        coefs
    }),
    tar_target(lava_model,{
        get_lava_model(coefs,time_horizon = 10)
    }),
    tar_target(sim_data,{
        sd = setDT(sim(lava_model,23149))
        sd[,pnr := 1:.N]
        for (a in grep("agegroups.+",names(sd),value = TRUE)) set(sd,j = a,value = NULL)
        for (a in grep("tertile_income_",names(sd),value = TRUE)) set(sd,j = a,value = NULL)
        for (a in grep("diabetes_duration.+",names(sd),value = TRUE)) set(sd,j = a,value = NULL)
        for (a in grep("education.+",names(sd),value = TRUE)) set(sd,j = a,value = NULL)
        setnames(sd,"sexMale","sex")
        setnames(sd,"index_heart_failureYes","index_heart_failure")
        sd[]
    }),
    tar_target(sim_time_covariates, {
        sim_data
        sim_time_covariates <- sim_data[,c("pnr",grep(paste(unique(unlist(lapply(c("mace","statin"),function(x){gsub("_[^_]*$", "", x)}))), collapse = "|"),names(sim_data),value = TRUE)), with = FALSE]
        sim_time_covariates[,(names(sim_time_covariates)[-1]):=lapply(.SD, as.numeric), .SDcols = names(sim_time_covariates)[-1]]
        sim_time_covariates[]
    }),
    tar_target(sim_baseline_covariates,sim_data[,c("pnr","sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]),
    tar_target(sim_regimen,sim_data[,grep("pnr|GS|B", names(sim_data)), with = FALSE]),
    tar_target(sim_outcome,sim_data[,grep("pnr|stroke_|Censored|Dead", names(sim_data)), with = FALSE]),
    tar_target(export,{
        fwrite(sim_data,file = "../register_project/data/register_data.csv")
        fwrite(sim_regimen,file = "../register_project/data/regimen_data.csv")
        fwrite(sim_outcome,file = "../register_project/data/outcome_data.csv")
        fwrite(sim_time_covariates,file = "../register_project/data/time_covariates.csv")
        fwrite(sim_baseline_covariates,file = "../register_project/data/baseline_covariates.csv")
    }),
    tar_target(test_run,{
        run_ltmle(name_outcome="stroke",
                  time_horizon=c(4),
                  outcome_data=sim_outcome,
                  regimen_data=list(GS = sim_regimen),
                  baseline_data=sim_baseline_covariates,
                  timevar_data=sim_time_covariates,
                  censor_others=FALSE,
                  abar = list(rep(1,4),rep(0,4)),
                  SL.library="glm",
                  verbose=TRUE)
    })
)

######################################################################
### _targets.R ends here
