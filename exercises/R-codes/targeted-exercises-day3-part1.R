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
