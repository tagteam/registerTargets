ltmle.glmnet <- function(Y,
                         X,
                         newX,
                         family,
                         obsWeights,
                         id,
                         alpha = 1,
                         nfolds = 10,
                         selector="undersmooth",
                         nlambda = 100,
                         useMin = TRUE,
                         loss = "deviance",
                         ...){
    requireNamespace("glmnet")
    Xnames=attr(newX,"Xnames")
    if (!is.matrix(X)) {
        X <- model.matrix(~-1 + ., X)
        newX <- model.matrix(~-1 + ., newX)
    }
    FAM <- ifelse(length(unique(Y))>2,"gaussian","binomial")
    ## print("gaussian")
    ## tictoc::tic()
    if (length(selector)>0&&selector=="undersmooth")
        uoh <- try(fit <- glmnet::glmnet(X,Y,weights = obsWeights,lambda = NULL,alpha = alpha,nlambda = nlambda,trace.it = 0L,family=FAM,...))
    else
        uoh <- try(fit <- glmnet::cv.glmnet(x = X,y = Y,weights = obsWeights,lambda = NULL,type.measure = loss,nfolds = nfolds,family = FAM,alpha = alpha,nlambda = nlambda,...))
    ## tictoc::toc()
    if (inherits(uoh,"try-error")) browser()
    ## print(c(fit$lambda.min,fit$lambda.1se))
    if (length(selector)>0&&selector=="undersmooth"){
        pred <- c(predict(fit, newx = newX, type = "response", s = fit$lambda[length(fit$lambda)]))
    } else{
        pred <- c(predict(fit, newx = newX, type = "response", s = ifelse(useMin,"lambda.min","lambda.1se")))
    }
    class(fit) <- c("ltmle.glmnet")
    if (length(selector)>0&&selector=="undersmooth"){
        beta <- data.table::data.table(beta=fit$beta[,NCOL(fit$beta),drop=TRUE])

    } else{
        beta <- data.table::data.table(beta=fit$beta[,match(fit$lambda.min,colnames(beta)),drop=TRUE])
    }
    if (length(Xnames)==NROW(beta)) beta=cbind(X=Xnames,beta)
    attr(fit,"selector") <- selector
    fit$selected_beta <- beta
    out <- list(predicted.values = pred, fit = fit)
    return(out)
}

predict.ltmle.glmnet <- function(object,newX,...){
    selector <- attr(object,"selector")
    if (!is.matrix(newX)){
        newX <- model.matrix(~-1 + ., newX)
    }
    if (length(selector)>0&&selector=="undersmooth"){
        class(object)="glmnet"
        uha <- try(pred <- c(predict(object,
                          newx = newX,
                          type = "response",
                          s = object$lambda[length(object$lambda)],
                          ...)))
        if (inherits(uha,"try-error"))
            browser()
        else
            pred
    } else{
        pred <- c(predict(object,
                          newx = newX,
                          type = "response",
                          s = "lambda.min",
                          ... ))
    }
}
