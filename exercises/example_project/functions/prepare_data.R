### prepare_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (15:40)
## Comment: Function that prepares the data for the analysis
### Code:
prepare_data <- function(rawdata){
    prep_data = copy(rawdata)
    # remove unwanted columns
    prep_data$eventtime <- NULL
    prep_data$censtime <- NULL
    # change order of columns
    setcolorder(prep_data,c("time","event","X1","X2","X3","X4"))
    # sort by time
    setkey(prep_data,time)
    # change column names
    setnames(prep_data,c("X1","X2","X3","X4"),c("sex","treatment","age","biomarker"))
    # change factor labels. use Publish::lazyFactorCoding(prep_data)
    prep_data[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    # return 
    prep_data[]
}


######################################################################
### prepare_data.R ends here
