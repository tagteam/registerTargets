### ate_analysis.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (17:58) 
## Version: 
## Last-Updated: Dec 14 2022 (18:44) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: Exercise 1 Day 4
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
library(Publish)
library(survival)
library(data.table)
library(ltmle)
library(riskRegression)
library(ranger)
# Load the wide format data set with the
# right censored time to death, age, sex, comorbidities and comedicine.
tar_load(wide_baseline_pop)
str(wide_baseline_pop)

# important: make variable factor variables factors
# lazyFactorCoding(wide_baseline_pop)
wide_baseline_pop[,myocardial.infarction:=factor(myocardial.infarction,levels=c("Yes"),labels=c("Yes"))]
wide_baseline_pop[,heart.failure:=factor(heart.failure,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,peripheral.vascular.disease:=factor(peripheral.vascular.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,cerebrovascular.disease:=factor(cerebrovascular.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,dementia:=factor(dementia,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,chronic.pulmonary.disease:=factor(chronic.pulmonary.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,rheumatic.disease:=factor(rheumatic.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,peptic.ulcer.disease:=factor(peptic.ulcer.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,mild.liver.disease:=factor(mild.liver.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,severe.liver.disease:=factor(severe.liver.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,diabetes.without.complications:=factor(diabetes.without.complications,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,diabetes.with.complications:=factor(diabetes.with.complications,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,hemiplegia.paraplegia:=factor(hemiplegia.paraplegia,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,renal.disease:=factor(renal.disease,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,any.malignancy:=factor(any.malignancy,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,metastatic.solid.tumor:=factor(metastatic.solid.tumor,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,AIDS.HIV:=factor(AIDS.HIV,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,leukemia:=factor(leukemia,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,lymphoma:=factor(lymphoma,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,bb:=factor(bb,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,ccb:=factor(ccb,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,rasi:=factor(rasi,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,thiazid:=factor(thiazid,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,loop:=factor(loop,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,mra:=factor(mra,levels=c("No"),labels=c("No"))]
wide_baseline_pop[,digoxin:=factor(digoxin,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,statin:=factor(statin,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,asa:=factor(asa,levels=c("No"),labels=c("No"))]
wide_baseline_pop[,adpi:=factor(adpi,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,vka:=factor(vka,levels=c("No"),labels=c("No"))]
wide_baseline_pop[,copd_med:=factor(copd_med,levels=c("No","Yes"),labels=c("No","Yes"))]
wide_baseline_pop[,dementia_med:=factor(dementia_med,levels=c("No"),labels=c("No"))]
wide_baseline_pop[,noac:=factor(noac,levels=c("No","Yes"),labels=c("No","Yes"))]

#
# Analysis
# 

# fit good old Cox regression (can add variables, interaction terms, splines ...)
cox_fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
            data = wide_baseline_pop,x = TRUE)
publish(cox_fit)

# fit propensity score model  (can add variables, interaction terms, splines ...)
propensity_fit <- glm(I(bb == "Yes")~+age+sex+any.malignancy+diabetes.with.complications,data = wide_baseline_pop,family = "binomial")
summary(propensity_fit)
publish(propensity_fit)

# fit censoring Cox model (can add variables, interaction terms, splines ...)
cens_fit = coxph(Surv(time,event == 0)~bb+age+sex+any.malignancy+diabetes.with.complications,
                 data = wide_baseline_pop,x = TRUE)
summary(cens_fit)
publish(cens_fit)

# can predict 5-year risks in training data
predcox5 <- predictRisk(cox_fit,times = 5,wide_baseline_pop)
predcox5

## Pseudo values can be used to approximate the survival status
## after 5 year risks and this approximate survival status variable
## can be used to fit a random forest or other machine learning methods
## in a fast way. See
## Mogensen et al. A random forest approach for competing
## risks based on pseudo-values. Statistics in medicine, 32(18):3102--3114,
## 2013.
# Kaplan-Meier estimator
km = prodlim(Hist(time,event)~1,data = wide_baseline_pop)
# add the pseudo value for 5-year outcome to the data
wide_baseline_pop[,pseudo5:= jackknife(km,times = 5)]
# fit a random forest (can change num.trees and also min.node.size, mtry ...)
## forest5 = ranger(pseudo5~ bb+ sex + age + myocardial.infarction + heart.failure,
## forest5 = ranger(pseudo5~ bb,
forest5 = ranger(pseudo5~ bb+ sex + age + myocardial.infarction + heart.failure + peripheral.vascular.disease + cerebrovascular.disease + dementia + chronic.pulmonary.disease + rheumatic.disease + peptic.ulcer.disease + mild.liver.disease + severe.liver.disease + diabetes.without.complications + diabetes.with.complications + hemiplegia.paraplegia + renal.disease + any.malignancy + metastatic.solid.tumor + AIDS.HIV + leukemia + lymphoma + ccb + rasi + thiazid + loop + mra + digoxin + statin + asa + adpi + vka + copd_med + dementia_med + noac,
                 data = wide_baseline_pop,
                 num.trees = 50)

# these are predictions of 5-year survival
survforest5 <- predict(forest5,data = wide_baseline_pop)$predictions
# predictions of 5-year risks
predforest5 <- 1-survforest5

# compare Cox versus forest
plot(predcox5,predforest5)

#
# G-formula estimate of average treatment effect
#
# base on Cox 
atecox <- ate(cox_fit,
              data = wide_baseline_pop,
              treatment = "bb",
              times = 5,
              verbose = FALSE)
summary(atecox)

# base on forest
data_bb <- copy(wide_baseline_pop)
data_bb[,bb := "Yes"]
data_nobb <- copy(wide_baseline_pop)
data_nobb[,bb := "No"]
# 5-year risk predicted in hypothetical worlds where alle/none take bb
forest5_bb_std <- 1-predict(forest5,data = data_bb)$predictions
forest5_nobb_std <- 1-predict(forest5,data = data_nobb)$predictions
# standardized 5-year risks for forest
mean(forest5_bb_std)
mean(forest5_nobb_std)
ate_forest <- mean(forest5_bb_std)-mean(forest5_nobb_std)

#
# TMLE estimate of average treatment effect
#
# use the pseudo value for 5-year outcome as above
# can add more variables, can change SL.library to include other learners
# into the superlearner, see SuperLearner::listWrappers for the available
wbp = wide_baseline_pop[,c("sex","age","diabetes.with.complications","any.malignancy","bb","pseudo5")]
wbp[,bb := as.numeric(bb == "Yes")]
tmle_fit = ltmle(data = wbp,
                 Anodes = "bb",
                 Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),
                 Ynodes = "pseudo5",
                 SL.library=c("SL.ranger","SL.glm"),
                 ## SL.library = "glm",
                 abar = list(1,0))
# note: these are estimates of standardized 5-year survival
summary(tmle_fit)

######################################################################
### ate_analysis.R ends here
