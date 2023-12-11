tar_target(baseline_covariates,{
  bsl <- copy(raw_baseline_covariates)
  bsl[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
  bsl[,index_heart_failure:=factor(index_heart_failure,levels=c("0","1"),labels=c("No","Yes"))]
  bsl
})

formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration

formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0

formula <- Drug_0 ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0
