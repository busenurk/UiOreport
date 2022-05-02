### function for pooling of all imputations
pre_res<-  function(data){ 
  
ps.model <- glm(formula = vaccination ~ ibu_prepreg+paracet_prepreg+opioid_prepreg+diabetes+depression_severity+pain+headache+pelvicgirdlepain+smoking+bmipp+age+parity+education+malform+sarscov2_infection,family  = binomial(link = "logit"), data = data)


data$propensity_vac <- predict(ps.model, type = "response")
## Predicted probability of being assigned to no vaccianated 
data$propensity_novac  <- 1 - data$propensity_vac

data$ate <- NA
data$ate <- ifelse(data$vaccination == 1, data$propensity_vac, data$propensity_novac )
data$prop.min <- pmin(data$propensity_vac,data$propensity_novac)

#### Propensity score matching weight #####
## Matching weight
data$ps_weights <- data$prop.min / data$ate
## Weighted data
data_svy <- svydesign(ids = ~ 1, data = data, weights = ~ ps_weights)


glm.weighted<-glm(formula = (gestational.age == 1) ~  vaccination, weights = ps_weights,family  = quasibinomial(), data  =  data)

return(glm.weighted)  }

