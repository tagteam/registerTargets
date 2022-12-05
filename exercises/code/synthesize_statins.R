### synthesize_statins.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov 26 2022 (10:54) 
## Version: 
## Last-Updated: Nov 30 2022 (14:29) 
##           By: Thomas Alexander Gerds
##     Update #: 22
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Synthesize wide format register data based on parametric fits to structural equation model
##'
##' No details available
##' @title Synthesize wide format register data
##' @param coefs Regression coefficients of the 
##' @return lava object
##' @seealso lava::sim
##' @examples
##' library(data.table)
##' library(lava)
##' cc <- fread("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/code/statin_coeffients.txt")
##' u=synthesize_statins(cc)
##' d=sim(u,100000)
##' setDT(d)
##' d[,table(depression_10)]
##' d[,table(ASCVD_10)]
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
synthesize_statins <- function(coefs,continuous = NULL){
    requireNamespace("lava")
    coefficients <- data.table(coefs)
    XNAMES <- names(coefficients)[-(1:2)]
    BETA <- coefficients[,-(1:2),with=0L]
    INTERCEPT <- coefficients[["Intercept"]]
    names(INTERCEPT) = coefficients[["variable"]]
    # empty lava model for simulation
    m <- lava::lvm()
    # loop across time and variables
    for (j in 1:NROW(coefficients)){
        V <- coefficients$var[j]
        beta <- unlist(BETA[j,])
        X <- XNAMES[!is.na(beta)]
        beta <- beta[!is.na(beta)]
        # add V ~ Intercept + beta X
        if (V %in% names(continuous)){
            lava::distribution(m,V) <- lava::normal.lvm(mean = INTERCEPT[V],sd = continuous[[V]]$sd)
            if (length(beta)>0){
                cat(V,": ",round(INTERCEPT[j],5),"+",paste0(paste0(round(beta,2),"*", X),collapse = "+"),"\n")
                lava::regression(m,from=X,to=V) <- round(beta,2)
            }
        }else{
            lava::distribution(m,V) <- lava::binomial.lvm(p = lava::expit(INTERCEPT[j]))
            if (length(beta)>0){
                cat(V,": ",round(INTERCEPT[j],5),"+",paste0(paste0(round(beta,2),"*", X),collapse = "+"),"\n")
                lava::regression(m,from=X,to=V) <- round(beta,2)
            }
        }
    }
    class(m) <- c("synthesizeDD",class(m))
    return(m)
}




######################################################################
### synthesize_statins.R ends here
