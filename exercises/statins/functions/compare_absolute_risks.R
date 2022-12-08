### compare_absolute_risks.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  1 2022 (09:31) 
## Version: 
## Last-Updated: Dec  1 2022 (11:56) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
compare_absolute_risks <- function(x,y,contrast = "RR"){
    r1 <- x$estimate[["tmle"]]
    r2 <- y$estimate[["tmle"]]
    IC1 <- x$IC[["tmle"]]
    IC2 <- y$IC[["tmle"]]
    D <- r1-r2
    RR <- r1/r2
    n = length(IC1)
    se.D <- sd(IC1 - IC2)/sqrt(n)
    RR <- r1/r2
    IClogr1 <- (1/r1)*IC1
    IClogr2 <- (1/r2)*IC2
    sediffLog <- sd(IClogr1-IClogr2)/sqrt(n)
    lower.RR <- RR*exp(-qnorm(0.975)*sediffLog)
    upper.RR <- RR*exp(+qnorm(0.975)*sediffLog)
    c(D,se.D,RR,se.RR,lower.RR,upper.RR)
}

compare_absolute_risks1 <- function(x,y,contrast = "RR"){
    x.est <- x$estimate[["tmle"]]
    y.est <- y$estimate[["tmle"]]
    x.IC <- x$IC[["tmle"]]
    y.IC <- y$IC[["tmle"]]
    beta <- c(logit(y.est),logit(y.est)-logit(x.est))
    IC <- cbind(logit(y.IC),logit(x.IC))
    y0 <- plogis(beta[1])
    y1 <- plogis(beta[1] + beta[2])
    names(y0) <- names(y1) <- NULL
    eff.list <- list(treatment = list(long.name = "Treatment Estimate",est = y1,gradient = c(y1 * (1 - y1), y1 * (1 - y1)),log.std.err = FALSE,CIBounds = 0:1),
                     control = list(long.name = "Control Estimate",est = y0,gradient = c(y0 * (1 - y0), 0),log.std.err = FALSE,CIBounds = 0:1),
                     ATE = list(long.name = "Additive Treatment Effect",est = y1 - y0,gradient = c(y1 * (1 - y1) - y0 * (1 - y0), y1 * (1 - y1)),log.std.err = FALSE,CIBounds = c(-1, 1)),
                     RR = list(long.name = "Relative Risk",est = y1/y0,gradient = c(y0 - y1, 1 - y1),log.std.err = TRUE,CIBounds = c(0,Inf)))
    n <- nrow(IC)
    measures.IC <- lapply(eff.list, GetSummary, var(IC), n)    


}


######################################################################
### compare_absolute_risks.R ends here
