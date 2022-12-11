### get_drug_list.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 11 2022 (12:59) 
## Version: 
## Last-Updated: Dec 11 2022 (18:46) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_drug_list <- function(atc_codes){
    lmdb <- fread("rawdata/lmdb.csv",
                  keepLeadingZeros = TRUE,
                  colClasses = c("character","Date","character"))
    x = lapply(names(atc_codes),function(drug){
        out = lmdb[grep(paste0(paste0("^",atc_codes[[drug]]),collapse = "|"),atc)]
        # remove duplicated entries with same admission date
        out <- out[out[,.I[1],by=c("pnr","eksd")]$V1]
        out[,X := drug]
        out[]
    })
    names(x) = names(atc_codes)
    x
}


######################################################################
### get_drug_list.R ends here
