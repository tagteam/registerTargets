### create_nutshell_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  5 2022 (10:07) 
## Version: 
## Last-Updated: Dec  6 2022 (10:07) 
##           By: Thomas Alexander Gerds
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary:
## Generate project data for exercises
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(heaven)
library(data.table)
make_pnr <- function(data,len){
    data[,pnr:=as.character(pnr)]
    N <- nchar(data$pnr)
    len <- pmax(len,max(N))
    for (m in 1:(len-1)){
        v=paste0(paste0(rep("0",len-m),collapse=""),data$pnr[N==m])
        set(data,j="pnr",i=which(N==m),value=v)
    }
    data
}
set.seed(9)
adm <- simAdmissionData(9999,diagnoses=c(c("DI21","DI22"),icdcodes[sample(1:.N,size=13)]$diag))
make_pnr(adm,len=6)
fwrite(adm,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/nutshell/rawdata/lpr.csv",quote=TRUE)
adm[,indexdate := NULL]
adm[,recnum := NULL]
pop <- adm[grep("DI2[12]",diag)]
pop <- pop[pop[,.I[1],by="pnr"]$V1]
pop <- pop[,.(pnr,inddto,uddto)]
set.seed(9)
N <- 100000
demo <- data.table(pnr=1:N,sex=rbinom(N,1,.42),birth_date=as.Date("1950-01-01")+runif(N,-365.25*20,365.25*20))
make_pnr(demo,len=6)
fwrite(demo,"~/metropolis/Teaching/targetedRegisterAnalysis/exercises/nutshell/rawdata/cpr.csv",quote=TRUE)

######################################################################
### create_nutshell_data.R ends here
