### sandbox.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (19:05) 
## Version: 
## Last-Updated: Dec 10 2023 (13:23) 
##           By: Thomas Alexander Gerds
##     Update #: 13
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

# part 3
library(glmnet)
tar_load_everything()
X=model.matrix(~ -1 + sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
               data = register_data)
Y=register_data[["Drug_0"]]
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

#--------------------------------------------------------
# day 4
#--------------------------------------------------------


######################################################################
### sandbox.R ends here
