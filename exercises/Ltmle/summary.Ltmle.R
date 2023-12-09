summary.Ltmle <- function(object,estimator,...){
    if (length(names(object$info$abar)) == 0)
        regimen_names = c("Risk(A=1)","Risk(A=0)")
    else
        regimen_names = paste0("Risk(",names(object$info$abar),")")
    if (missing(estimator)) estimator = object$info$estimator
        ## if (object$gcomp) estimator = "gcomp" else estimator = "tmle"
    MySummaryLtmle <- function (object, estimator = ifelse(object$gcomp, "gcomp", "tmle"),
                                ...)
    {
        IC.variance <- var(object$IC[[estimator]])
        if (estimator == "tmle" && !is.null(object$variance.estimate)) {
            v <- max(IC.variance, object$variance.estimate)
        }
        else {
            v <- IC.variance
        }
        variance.estimate.ratio = v/IC.variance
        if (object$binaryOutcome) {
            CIBounds <- c(0, 1)
        }
        else {
            CIBounds <- c(-Inf, Inf)
        }
        treatment <- GetSummary(list(long.name = NULL, est = object$estimates[estimator],
                                     gradient = 1, log.std.err = FALSE, CIBounds = CIBounds),
                                v,
                                n = length(object$IC[[estimator]]))
        ans <- list(treatment = treatment, call = object$call, estimator = estimator,
                    variance.estimate.ratio = variance.estimate.ratio)
        class(ans) <- "summary.ltmle"
        return(ans)
    }
    summi <- function(x,target){
        with(x,data.table::data.table(
                               Target_parameter=target,
                               Estimator=estimator,
                               estimate=estimate,
                               std.err=std.dev,
                               lower=CI[[1]],
                               upper=CI[[2]],
                               pvalue=pvalue))
    }
    if (length(object$estimates)>0){
        x=MySummaryLtmle(object,estimator=estimator)
        risk = summi(x=x$treatment,regimen_names[[1]])
        risk
    }else{
        x=summary.ltmleEffectMeasures(object,estimator=estimator)
        # check if outcome is continuous (else the estimate is called risk)
        if ("estimate"%in%names(x$effect.measures$treatment)){
            name_treatment="estimate"
            name_control="estimate"
        } else{
            name_treatment=regimen_names[[1]]
            name_control=regimen_names[[2]]
        }
        treatment = summi(x=x$effect.measures$treatment,target=name_treatment)
        treatment$Target_parameter=regimen_names[[1]]
        control = summi(x=x$effect.measures$control,target=name_control)
        control$Target_parameter=regimen_names[[2]]
        ate = summi(x=x$effect.measures$ATE,"ATE")
        if ("RR"%in%names(x$effect.measures)){
            RR = summi(x=x$effect.measures$RR,"Ratio")
        }else{
            RR=NULL
        }
        out=rbind(treatment,control,ate,RR)
        out
    }
}
