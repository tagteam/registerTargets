### make_pnr.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:03) 
## Version: 
## Last-Updated: Dec  8 2022 (16:04) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
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

######################################################################
### make_pnr.R ends here
