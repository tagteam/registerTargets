### SL.ranger2.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  9 2023 (07:23) 
## Version: 
## Last-Updated: Dec  9 2023 (07:35) 
##           By: Thomas Alexander Gerds
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

SL.ranger2 <- function (Y, X, newX, family, obsWeights, num.trees = 5, mtry = floor(sqrt(ncol(X))), 
                        write.forest = TRUE, probability = family$family == "binomial", 
                        min.node.size = ifelse(family$family == "gaussian", 5, 1), 
                        replace = TRUE, sample.fraction = ifelse(replace, 1, 0.632), 
                        num.threads = 1, verbose = T, ...) 
{
    SuperLearner:::.SL.require("ranger")
    if (family$family == "binomial" && length(unique(Y)) == 2) {
        Y = as.factor(Y)
    }
    if (is.matrix(X)) {
        X = data.frame(X)
    }
    fit <- ranger::ranger(`_Y` ~ ., data = cbind(`_Y` = Y, X), 
                          num.trees = num.trees, mtry = mtry, min.node.size = min.node.size, 
                          replace = replace, sample.fraction = sample.fraction, 
                          case.weights = obsWeights, write.forest = write.forest, 
                          probability = probability, num.threads = num.threads, 
                          verbose = verbose)
    pred <- predict(fit, data = newX)$predictions
    if (family$family == "binomial") {
        pred = pred[, "1"]
    }
    fit <- list(object = fit, verbose = verbose)
    class(fit) <- c("SL.ranger")
    out <- list(pred = pred, fit = fit)
    return(out)
}

######################################################################
### SL.ranger2.R ends here
