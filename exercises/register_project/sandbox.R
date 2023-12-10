### sandbox.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (19:05) 
## Version: 
## Last-Updated: Dec 10 2023 (14:30) 
##           By: Thomas Alexander Gerds
##     Update #: 18
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_source("functions")
tar_make()
tar_manifest()
tar_visnetwork()
show_performance()
show_warnings()
tar_load_everything()

#--------------------------------------------------------
# day 2
#--------------------------------------------------------

# part 1
tar_load(raw_baseline_covariates)
print(raw_baseline_covariates)

tar_make()
tar_load(baseline_covariates)

# adapt the table_1 target defined in example_project (yesterday exercise)

tar_load(time_covariates)
time_covariates

## part 2
tar_make()
tar_load(ltmle_fit_death_1)
tar_load(table_ltmle_death_1)

tar_load(ltmle_fit_death_2)
tar_load(table_ltmle_death_2)

#--------------------------------------------------------
# day 3
#--------------------------------------------------------

# part 1
tar_load_everything()
ps <- glm(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
          family = "binomial",
          data = register_data)
summary(ps)

library(riskRegression)
library(targets)
library(ggplot2)
tar_load_everything()
ps <- glm(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
ps_interaction <- glm(Drug_0~ sex * agegroups + tertile_income * education + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
# add predicted propensity scores as variables to data.table
register_data[,propensity_score:=predictRisk(ps,newdata= register_data)]
register_data[,propensity_score_interaction:=predictRisk(ps_interaction,newdata= register_data)]
g <- ggplot(register_data,aes(x= propensity_score, y= propensity_score_interaction))+geom_point()+xlim(c(0,1))+ylim(c(0,1))
g

library(SuperLearner)
# SuperLearner does not provide a formula interface
X=register_data[,c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]
Y=register_data[["Drug_0"]]
# without interaction
sl_ps <- SuperLearner(Y=Y,X=X,SL.library="SL.glm",family="binomial")
# with interaction
sl_ps_interaction <- SuperLearner(Y=Y,X=X,SL.library="SL.glm.interaction",family="binomial")

register_data[,sl_propensity:=predict(sl_ps,newdata=X)$pred]
ggplot(register_data,aes(x= propensity_score, y= sl_propensity))+geom_point()+xlim(c(0,1))+ylim(c(0,1))

register_data[,sl_propensity_interaction:=predict(sl_ps_interaction,newdata=X)$pred]
ggplot(register_data,aes(x= propensity_score_interaction, y= sl_propensity_interaction))+geom_point()+xlim(c(0,1))+ylim(c(0,1))


summary(ps)
summary(sl_ps$fitLibrary[[1]]$object)
summary(ps_interaction)
summary(sl_ps_interaction$fitLibrary[[1]]$object)

library(riskRegression)
library(targets)
library(ggplot2)
tar_load_everything()
out <- glm(Dead_1~ Drug_0 + sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
out_interaction <- glm(Dead_1~ Drug_0 + sex * agegroups + tertile_income * education + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
# add predicted propensity scores as variables to data.table
register_data[,risk:=predictRisk(out,newdata= register_data)]
register_data[,risk_interaction:=predictRisk(out_interaction,newdata= register_data)]
ggplot(register_data,aes(x= risk, y= risk_interaction))+geom_point()+xlim(c(0,1))+ylim(c(0,1))

# part 2

library(SuperLearner)
X=register_data[,c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]
Y=register_data[["Drug_0"]]
set.seed(8)
# default value for cross-validation is 10-fold
sl_ps_super1 <- SuperLearner(Y=Y,
                             X=X,
                             SL.library=c("SL.glm","SL.glm.interaction"),
                             family="binomial")
# 2-fold is much faster
set.seed(8)
sl_ps_super_V2 <- SuperLearner(Y=Y, X=X,
                               SL.library=c("SL.glm","SL.glm.interaction"),
                               cvControl=list(V=2),
                               family="binomial")

library(SuperLearner)
X=register_data[,c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]
Y=register_data[["Drug_0"]]
set.seed(8)
sl_ps_super2 <- SuperLearner(Y=Y, X=X,
                             # speed up by using 2-fold
                             # cvControl=list(V=2)
                             SL.library=c("SL.mean","SL.glm","SL.glm.interaction","SL.step.interaction"),
                             family="binomial")

print(sl_ps_super1)
print(sl_ps_super2)


# part 3
library(glmnet)
tar_load_everything()
X=model.matrix(~ -1 + sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
               data = register_data)
Y=register_data[["Drug_0"]]
# set the seed because the cross-validation uses
# a random split
set.seed(3) 
lasso <- cv.glmnet(x = X,y = Y,alpha = 1)
coef(lasso)
ridge <- cv.glmnet(x = X,y = Y,alpha = 0)
coef(ridge)
elnet <- cv.glmnet(x = X,y = Y,alpha = 0.5)
coef(elnet)

# with some interactions
XX=model.matrix(~ -1 + sex * agegroups + education * tertile_income + index_heart_failure * diabetes_duration,
                data = register_data)
elnet_interaction <- cv.glmnet(x = XX,y = Y,alpha = 0.5)
coef(elnet_interaction)

# part 4

tar_load_everything()
library(ranger)
library(riskRegression)
# avoid that ranger treats our 0-1 variable Drug_0 as numeric
register_data[,Drug_0:=factor(Drug_0)]
# reserve a random tenth of the data for prediction
set.seed(9)
learning <- sample(1:NROW(register_data),size=NROW(register_data)*9/10)
learning_register_data  <- register_data[learning]
test_register_data  <- register_data[!learning]
ps1_ranger <- ranger(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
                     data = learning_register_data,
                     seed=8, classification = TRUE,
                     probability=TRUE)
# propensity score
propensity1_ranger <- predictRisk(ps1_ranger,newdata=test_register_data)

mean(propensity1_ranger)
mean(test_register_data[["Drug_0"]]==1)

ps_learn <- glm(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
          family = "binomial",
          data = learning_register_data)
propensity_test <- predictRisk(ps_learn,newdata=test_register_data)
mean(propensity_test)

library(ggplot2)
test_register_data[,propensity_test_logistic := predictRisk(ps_learn,newdata=test_register_data)]
test_register_data[,propensity_test_ranger := predictRisk(ps1_ranger,newdata=test_register_data)]
ggplot(test_register_data,aes(x= propensity_test_logistic, y= propensity_test_ranger))+geom_point()+xlim(c(0,1))+ylim(c(0,1))

#--------------------------------------------------------
# day 4
#--------------------------------------------------------


######################################################################
### sandbox.R ends here
