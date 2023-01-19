# library(rstan)
# library(rstanarm)
# library(HDInterval)

make_decision_trial <- function(res_list, 
                                #which_pop=c("TRD","PRD"), 
                                which_pop,
                                #which_admin=c("pill","nasal","IV"),
                                which_admin=ways_of_administration,
                                which_treat, 
                                which_measure = c(1,2),
                                test_type = c("freq","bayes","both"), 
                                interim = FALSE,
                                control_type = c("all", "concurrent"),
                                hdi_perc = NULL, 
                                p_val = NULL, 
                                sided="two_sided") {
  

  # which control-cohort is needed for the comparison, gives the position in res_list
  control_needed <- which(names(res_list[[which_pop]]) == paste0(which_admin,"_Control",sep=""))
  
  
  # Treatment group difference
  baseline_treat <- res_list[[which_pop]][[which_treat]]$data[,1]
  endpoint_treat <- res_list[[which_pop]][[which_treat]]$data[,1+which_measure]
  
 
  # Placebo group
    if (control_type == "all") {
      baseline_contr <- res_list[[which_pop]][[control_needed]]$data[,1]
      endpoint_contr <- res_list[[which_pop]][[control_needed]]$data[,1+which_measure]
     }
    
    if (control_type == "concurrent") {
       conc_times <- unique(res_list[[which_pop]][[which_treat]]$data[,"timestamp"])
       conc_controls <- res_list[[which_pop]][[control_needed]]$
                          data[which(res_list[[which_pop]][[control_needed]]$data[,"timestamp"]>=min(conc_times) 
                                   & res_list[[which_pop]][[control_needed]]$data[,"timestamp"]<=max(conc_times)),]
       baseline_contr <- conc_controls[,1]
       endpoint_contr <- conc_controls[,1+which_measure]
       }
    
  #calculation of effect estimate
  cohensD <- (mean(baseline_treat-endpoint_treat) - mean(baseline_contr-endpoint_contr)) / 
              ((((length(endpoint_contr)-1)*sd(baseline_contr-endpoint_contr)) + 
                  ((length(endpoint_treat)-1)*sd(baseline_treat-endpoint_treat))) / 
                 (length(endpoint_contr)+length(endpoint_treat)-2))

  response_data <- data.frame(baseline = c(baseline_treat, baseline_contr),
                              endpoint = c(endpoint_treat, endpoint_contr),
                              arm = factor(c(rep(1,length(endpoint_treat)),
                                             rep(0,length(endpoint_contr)))))
  
   ########## Bayesian Two-Arm Superiority Criteria ###############
  
  if(test_type %in% c("bayes","both")){
  
  if (interim) {
    hdi_percUSE <- hdi_perc[1]
  } else {
    hdi_percUSE <- hdi_perc[2]
  }
  
  model_bayes <- stan_glm(endpoint ~ baseline + arm, data=response_data)
  posteriors <- insight::get_parameters(model_bayes)
  hd_int <- unname(hdi(posteriors$arm1, credMass = hdi_percUSE)[1:2])
  cont0_bayes <- hd_int[1] <= 0 &  hd_int[2] >= 0
  
  
  res_bayesLM <- list(mean_effect = mean(posteriors$arm1),
                      median_effect = median(posteriors$arm1),
                      HighestDensityInterval = hd_int,
                      decision = ifelse(cont0_bayes == TRUE, "failure", "success"))
}
  ########### P-Value Superiority Criteria ##########
  
  if(test_type %in% c("freq","both")){ 
  
    if (interim) {
      p_valUSE <- p_val[1]
    } else {
      p_valUSE <- p_val[2]
    }
    
  model_freq <- lm(endpoint ~ baseline + arm, data=response_data)
  
  if(sided=="one_sided"){
    # if estimate <0, then the treatment arm has a favorable effect
    # in that case, for one-sided p-value, divide two-sided p-value provided by lm by 2
    one_sided_pvalue <- ifelse(summary(model_freq)$coefficients["arm1",1] < 0, 
                               summary(model_freq)$coefficients["arm1",4]/2, 
                               1-summary(model_freq)$coefficients["arm1",4]/2)
    # two-sided confidence interval on level of the significance level at final analysis: one-sided alpha of 5% corresponds to two-sided 1-2*alpha, i.e. 90% CI 
    conf_int <- unname(confint(model_freq, level = 1-2*p_val[2])["arm1",])
    # check whether the p-value is below threshold
    success_pval <- one_sided_pvalue < p_valUSE
  } else if(sided=="two_sided") {
    conf_int <- unname(confint(model_freq, level = 1-p_val[2])["arm1",])
    success_pval <- (summary(model_freq)$coefficients["arm1",1] < 0) & 
                    (summary(model_freq)$coefficients["arm1",4] < p_valUSE)
  }

      
  ###  
  #alternative way to specify the effect size: use standardized regression coefficient
  #cohensD <- unname(lm(scale(diff) ~ scale(as.numeric(arm)), data=response_data)$coefficients[2])
  ###
    
  res_freqLM <- list(mean_effect = summary(model_freq)$coefficients["arm1",1],
                     ConfidenceInterval = conf_int,
                     decision = ifelse(interim==TRUE, 
                                       ifelse(success_pval==FALSE, "stopped early", "continue"), 
                                       ifelse(success_pval==FALSE, "failure", "success")),
                     n_tested = nrow(response_data), 
                     n_control = nrow(response_data[response_data$arm==0,]), 
                     n_treatment = nrow(response_data[response_data$arm==1,]))
   
  }  
if(test_type == "both"){
  return(list(res_bayesLM, res_freqLM, cohensD))} else
    if(test_type == "bayes"){
      return(list(res_bayesLM, cohensD))
    } else
      if(test_type=="freq"){
        return(list(res_freqLM, cohensD))}
  
  
}

# make_decision_trial(res_list = res_list, which_pop="PRD", 
#                     which_admin="pill", 
#                     which_treat=1, which_measure = 1,
#                     test_type = "bayes", interim = TRUE,
#                     control_type = "all",
#                     hdi_perc = c(0.9,0.95), p_val = c(0.25,0.25))

