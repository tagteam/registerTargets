### map_intervals.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (13:47) 
## Version: 
## Last-Updated: Dec 14 2022 (13:47) 
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
map_intervals <- function(grid,data,name,rollforward,values=c(1,0),fill=NA,X.factor=FALSE){
    require(data.table)
    setkey(grid,pnr,date)
    # make sure that only patients who have a date are rolled
    data=data[!is.na(date)]
    if (length(data)==0) return(NULL)
    setkey(data,pnr,date)
    data[,X:=values[[1]]]
    grid <- data[grid,roll=rollforward]
    # missing value means no event in this interval
    grid[is.na(grid$X),X:=values[[2]]]
    setkey(grid,pnr,interval)
    wide <- dcast(grid,pnr~interval,value.var="X",sep="_",fill=fill)
    if (X.factor) {
        # this is for ltmle censored/uncensored
        for (cc in names(wide)[-1]){
            set(wide,j=cc,value=factor(wide[[cc]],levels=values))
        }
    }
    grid[,X:=NULL]
    data[,X:=NULL]
    # dcast assigns numeric column names when value.var
    # has length one
    setnames(wide,c("pnr",paste0(name,"_",names(wide)[-1])))
    setkey(wide,pnr)
    wide[]
}


######################################################################
### map_intervals.R ends here
