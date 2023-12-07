# library(lava)
# library(data.table)
# library(foreach)

get_lava_model <- function(time_horizon,
                           coefs){
    time_grid = 0:time_horizon
    K = length(time_grid)
    vectorize_coefs <- function(x){
        if (is.data.table(x)){
            tmp <- c(x$beta)
            names(tmp) <- x$X
            tmp
        }else{
            x
        }
    }
    outcome_coefs=vectorize_coefs(coefs[["outcome_coef"]])
    comp.event_coefs=vectorize_coefs(coefs[["comp.event_coef"]])
    baseline_coefs=vectorize_coefs(coefs[["baseline_coef"]])
    timevar_coefs=vectorize_coefs(coefs[["timevar_coef"]])
    censoring_coefs=vectorize_coefs(coefs[["censoring_coef"]])
    regimen_coefs=vectorize_coefs(coefs[["regimen_coef"]])
    is_binary_baseline=sapply(baseline_coefs,length)==2
    binary_baseline_coefs=baseline_coefs[is_binary_baseline]
    categorized_baseline_coefs=baseline_coefs[!is_binary_baseline]
    m <- lvm()
    ## NOTE: All baseline and time varying variables at t = 0 are assumed to be independent of one another
    ## We assume that all variables (except possibly some of the baseline covariates) are binary
    
    ## Baseline variables are divided into binary and categorical which are then transformed into dummy variables
    for (b in names(binary_baseline_coefs)){
      distribution(m, paste0(b, names(binary_baseline_coefs[[b]])[[2]])) <- binomial.lvm(p = binary_baseline_coefs[[b]][[2]])
    }
    for (cat in names(categorized_baseline_coefs)){
      levs=names(categorized_baseline_coefs[[cat]])
      m <- categorical(m,cat,K=length(levs),labels=levs,p=categorized_baseline_coefs[[cat]][-length(categorized_baseline_coefs[[cat]])])
      # Loop does not work, but evaluating for each level in levs with paste0 in the following ways does
      # gsub is unnecessary if we have no labels containing space
      eval(parse(text = paste0("transform(m,formula(",paste0(cat,gsub(" ", "", levs),"~",cat), 
                               ")) <- function(x){1*(x[[1]]=='", levs, "')}")))
    }
    
    ## Time varying variables for t = 0. All of these are assumed to be binary with only an intercept coefficient
    for(tv in names(timevar_coefs)){
      distribution(m, paste0(tv,"_",0)) <- binomial.lvm()
      intercept(m, paste0(tv,"_",0)) <- timevar_coefs[[tv]][[1]][1]
    }
    ## Treatment variable for t = 0
    regimen <- unique(unlist(lapply(names(regimen_coefs), function(x){gsub("_[^_]*$", "", x)}))) ## Get treatment names
    for(reg in regimen){
      distribution(m, paste0(reg,"_",0)) <- binomial.lvm(link = "logit")
      intercept(m, paste0(reg,"_",0)) <- regimen_coefs[[paste0(reg,"_",0)]][1]
      regression(m, to = paste0(reg,"_",0), 
                 # Get names of dependent variables but remove space and add space around -
                 from = unlist(lapply(names(regimen_coefs[[paste0(reg,"_",0)]][-1]), function(x){gsub("-", " - ", gsub(" ", "", x))}))) <- 
        regimen_coefs[[paste0(reg,"_",0)]][-1]
    }
    
    outcome <- unique(unlist(lapply(names(outcome_coefs), function(x){gsub("_[^_]*$", "", x)})))
    censoring <- unique(unlist(lapply(names(censoring_coefs), function(x){gsub("_[^_]*$", "", x)})))
    comp.event <- unique(unlist(lapply(names(comp.event_coefs), function(x){gsub("_[^_]*$", "", x)})))
    message("Outcome")
    for(k in time_grid[-1]){
        print(k)
        distribution(m, paste0(outcome,"_",k)) <- binomial.lvm()
        if (is.data.table(outcome_coefs[[k]])){
            intercept(m, paste0(outcome,"_",k)) <- outcome_coefs[[k]][1,]$beta
            from_k=unlist(lapply(names(outcome_coefs[[paste0(outcome,"_",k)]][-1]),function(x){ gsub("-", " - ", gsub(" ", "", x))}))
            regression(m, to = paste0(outcome,"_",k), from =from_k) <- outcome_coefs[[paste0(outcome,"_",k)]][-1]
        }else{
            intercept(m, paste0(outcome,"_",k)) <- outcome_coefs[[k]][1]
            from_k=unlist(lapply(names(outcome_coefs[[paste0(outcome,"_",k)]][-1]),function(x){ gsub("-", " - ", gsub(" ", "", x))}))
            regression(m, to = paste0(outcome,"_",k), from =from_k) <- outcome_coefs[[paste0(outcome,"_",k)]][-1]
        }

    }
    message("censoring")
    for(k in time_grid[-1]){      
        print(k)
        distribution(m, paste0(censoring,"_",k)) <- binomial.lvm()
        intercept(m, paste0(censoring,"_",k)) <- censoring_coefs[[paste0(censoring,"_",k)]][1]
        from_k=unlist(lapply(names(censoring_coefs[[paste0(censoring,"_",k)]][-1]),function(x){gsub("-", " - ", gsub(" ", "", x))}))
        regression(m, to = paste0(censoring,"_",k), from = from_k) <- censoring_coefs[[paste0(censoring,"_",k)]][-1]
    }
    message("competing")
    for(k in time_grid[-c(1,K)]){
        ## Competing event
        message(k)
        distribution(m, paste0(comp.event,"_",k)) <- binomial.lvm()
        intercept(m, paste0(comp.event,"_",k)) <- comp.event_coefs[[paste0(comp.event,"_",k)]][1]
        from_k=unlist(lapply(names(comp.event_coefs[[paste0(comp.event,"_",k)]][-1]), 
                             function(x){gsub("-", " - ", gsub(" ", "", x))}))
        regression(m, to = paste0(comp.event,"_",k), from = from_k) <- comp.event_coefs[[paste0(comp.event,"_",k)]][-1]
    }
    message("regimens")
    for(k in time_grid[-K]){      
        message(k)
        ## Regimen
        for(reg in regimen){
            message(reg)
            distribution(m, paste0(reg,"_",k)) <- binomial.lvm()
            intercept(m, paste0(reg,"_",k)) <- regimen_coefs[[paste0(reg,"_",k)]][1]
            from_k=unlist(lapply(names(regimen_coefs[[paste0(reg,"_",k)]][-1]), 
                                 function(x){gsub("-", " - ", gsub(" ", "", x))}))
            regression(m, to = paste0(reg,"_",k), from = from_k) <- regimen_coefs[[paste0(reg,"_",k)]][-1]
        }
    }
    for(k in time_grid[-K]){              
        message(k)
        ## Time varying covariates - we assume that all covariates are binary (for now)
        for(tv in names(timevar_coefs)){
            message(tv)
            distribution(m, paste0(tv,"_",k)) <- binomial.lvm()
            intercept(m, paste0(tv,"_",k)) <- timevar_coefs[[tv]][[paste0(tv,"_",k)]][1]
            from_k=unlist(lapply(names(timevar_coefs[[tv]][[paste0(tv,"_",k)]][-1]), function(x){gsub("-", " - ", gsub(" ", "", x))}))
            regression(m, to = paste0(tv,"_",k), from = from_k) <- timevar_coefs[[tv]][[paste0(tv,"_",k)]][-1]
        }
    }
    return(m)
}

