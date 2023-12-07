Ltmle_working_horse <- function (data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes,
                                 survivalOutcome = NULL, Qform = NULL, gform = NULL, abar,
                                 rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                                 deterministic.Q.function = NULL,
                                 stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                                 estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, 
                                 variance.method = "ic", observation.weights = NULL, id = NULL,info = NULL,
                                 reduce=TRUE,verbose=FALSE)
{
    require(matrixStats)
    if (length(Dnodes)>0){
        deterministic.Q.function <- function(data, current.node, nodes, called.from.estimate.g){
            death.index <- grep(paste0("Dead", "_"),names(data))
            if(length(death.index)==0){
                message("No death/terminal event node found")
                return(NULL)
            }
            hist.death.index <- death.index[death.index < current.node]
            if(length(hist.death.index)==0)
                return(NULL)
            else{
                is.deterministic <- Reduce("+",lapply(data[,hist.death.index,drop=FALSE],
                                                      function(dd){x=dd;x[is.na(dd)] <- 0;x}))>=1
                # should be unnecessary to exclude those who readily
                # have a missing value for death, but it does not hurt either
                is.deterministic[is.na(is.deterministic)] <- FALSE
                list(is.deterministic=is.deterministic, Q.value=0)
            }
        }
    }else deterministic.Q.function <- NULL
    data <- CheckData(data)
    msm.inputs <- GetMSMInputsForLtmle(data, abar, rule, gform)
    inputs <- CreateInputs(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes,
                           Lnodes = Lnodes, Ynodes = Ynodes, survivalOutcome = survivalOutcome,
                           Qform = Qform, gform = msm.inputs$gform, Yrange = Yrange,
                           gbounds = gbounds, deterministic.g.function = deterministic.g.function,
                           SL.library = SL.library, SL.cvControl = SL.cvControl,
                           regimes = msm.inputs$regimes, working.msm = msm.inputs$working.msm,
                           summary.measures = msm.inputs$summary.measures, final.Ynodes = msm.inputs$final.Ynodes,
                           stratify = stratify, msm.weights = msm.inputs$msm.weights,
                           estimate.time = estimate.time, gcomp = gcomp, iptw.only = iptw.only,
                           deterministic.Q.function = deterministic.Q.function,
                           variance.method = variance.method, observation.weights = observation.weights,
                           id = id, verbose = verbose)
    result <- LtmleFromInputs(inputs)
    result$call <- match.call()
    result$info <- result$call$info
    if (reduce){
        result$call <- NULL
        result$cum.g <- result$cum.g.used <- result$cum.g.unbounded <- NULL
        result$Qstar <- NULL
        result$fit$Qstar <- NULL        
    }
    class(result) <- "Ltmle"
    return(result)
}
