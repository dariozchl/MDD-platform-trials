library(rstan)
library(rstanarm)
library(HDInterval)

make_decision_trial <- function(results, which_pop=c("TRD","PRD"), 
                                which_admin=c("pill","nasal","IV"), 
                                which_treat, which_measure = c(1,2),
                                test_type = c("freq","bayes","both"), 
                                interim = FALSE,
                                control_type = c("all", "concurrent"),
                                hdi_perc = NULL, p_val = NULL) {
  

  #which control-cohort needed
  control_needed <- which(names(results[[which_pop]]) == 
                            paste0(which_admin,"_Control",sep=""))
  
  
  # Treatment group difference
  diff_after_treat <- results[[which_pop]][which_treat]$endpoint[,1] -
                      results[[which_pop]][which_treat]$endpoint[,1+which_measure]
  
 
  # Placebo group
    if (control_type == "all") {
      diff_after_cont <- results[[which_pop]][[control_needed]]$endpoint[,1] -
        results[[which_pop]][[control_needed]]$endpoint[,1+which_measure]
     }
    
    if (control_type == "concurrent") {
       conc_times <- unique(results[[which_pop]][which_treat]$endpoint[,which(colnames(results[[which_pop]][which_treat]$endpoint) == "timestamp")])
       conc_controls <- results[[which_pop]][[control_needed]]$endpoint[
         which(results[[which_pop]][[control_needed]]$endpoint[,which(colnames(results[[which_pop]][which_treat]$endpoint) == 
                 "timestamp")] %in% conc_times == TRUE),]
       diff_after_cont <- conc_controls[,1] - conc_controls[,1+which_measure]
       }
    
  cohensD <- sqrt(abs(mean(diff_after_cont) - mean(diff_after_treat))/
                (((length(diff_after_cont)-1)*sd(diff_after_cont))+
                    ((length(diff_after_treat)-1)*sd(diff_after_treat)))/
                    (length(diff_after_cont)+length(diff_after_treat)-2))
  
  response_data <- data.frame(diff = c(diff_after_treat, diff_after_cont),
                              arm = factor(c(rep(1,length(diff_after_treat)),
                                      rep(0,length(diff_after_cont)))))
   ########## Bayesian Two-Arm Superiority Criteria ###############
  
  if(test_type %in% c("bayes","both")){
  
  if (interim) {
    hdi_percUSE <- hdi_perc[1]
  } else {
    hdi_percUSE <- hdi_perc[2]
  }
  
  model_bayes <- stan_glm(diff~arm, data=response_data)
  posteriors <- insight::get_parameters(model_bayes)
  hd_int <- unname(hdi(posteriors$arm1, credMass = hdi_percUSE)[1:2])
  cont0_bayes <- hd_int[1] <= 0 &  hd_int[2] >= 0
  
  
  res_bayesLM <- list(mean_effect = mean(posteriors$arm1),
                      median_effect = median(posteriors$arm1),
                      HighestDensityInterval = hd_int,
                      decision = ifelse(cont0_bayes == TRUE, "unsuccessfull", 
                                        "successfull"))
}
  # ########## P-Value Superiority Criteria ##########
  
  if(test_type %in% c("freq","both")){ 
  
    if (interim) {
      p_valUSE <- p_val[1]
    } else {
      p_valUSE <- p_val[2]
    }
    
  model_freq <- lm(diff~arm, data=response_data)
  conf_int <- unname(confint(model_freq, level = 1-p_valUSE)[2,])
  cont0_freq <- conf_int[1] <= 0 &  conf_int[2] >= 0
  
  res_freqLM <- list(mean_effect = unname(model_freq$coefficients[2]),
                      ConfidenceInterval = conf_int,
                      decision = ifelse(cont0_freq == TRUE, ifelse(interim ==TRUE,"stopped early",
                      "failure"), "successfull"))
   
  }  
if(test_type == "both"){
  return(list(res_bayesLM, res_freqLM, cohensD))} else
    if(test_type == "bayes"){
      return(list(res_bayesLM, cohensD))
    } else
      if(test_type=="freq"){
        return(list(res_freqLM, cohensD))}
  
  
}

# make_decision_trial(results = results, which_pop="PRD", 
#                     which_admin="pill", 
#                     which_treat=1, which_measure = 1,
#                     test_type = "bayes", interim = TRUE,
#                     control_type = "all",
#                     hdi_perc = c(0.9,0.95), p_val = c(0.25,0.25))

