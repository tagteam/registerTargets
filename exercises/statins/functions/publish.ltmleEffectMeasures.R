publish.ltmleEffectMeasures <- function(x,...){
    sx = summary(x)$effect.measures
    # remove odds ratios: who wants to see them?
    if (match("OR",names(sx),nomatch = 0)>0)
        sx <- sx[-match("OR",names(sx))]
    out <- data.table(
        X = names(sx),
        estimate = sapply(sx,function(x)x$estimate),
        lower = sapply(sx,function(x)x$CI[[1]]),
        upper = sapply(sx,function(x)x$CI[[1]]),
        Estimate.CI = sapply(names(sx),function(n){
            u = sx[[n]]
            if (n == "RR"){
                formatCI(x = u$estimate,lower = u$CI[[1]],upper = u$CI[[2]],show.x = TRUE)
            } else{
                formatCI(x = 100*u$estimate,lower = 100*u$CI[[1]],upper = 100*u$CI[[2]],show.x = TRUE)
            }})
    )
    out
}
