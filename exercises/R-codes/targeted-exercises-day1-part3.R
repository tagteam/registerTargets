tar_target(propensity_score_table,{
  formula <- treatment~sex+age
  ps <- glm(formula,data=data,family="binomial")
  publish(ps,print=FALSE)
})

tar_target(demo_table,{
  get_propensity_score_table(formula,data)
},packages=c("data.table","Publish"))
