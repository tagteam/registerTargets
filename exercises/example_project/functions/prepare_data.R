### prepare_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (15:40)
## Comment: Function that prepares the data for the analysis
### Code:
prepare_data <- function(rawdata){
    # change order of columns
    setcolorder(rawdata,c("time","event","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10"))
    # sort by time
    setkey(rawdata,time)
    # change column names
    setnames(rawdata,c("X1","X2","X6","X7"),c("sex","treatment","age","biomarker"))
    # change factor labels. use Publish::lazyFactorCoding(rawdata)
    rawdata[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    # return 
    rawdata[]
}


######################################################################
### prepare_data.R ends here
