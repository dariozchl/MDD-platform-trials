operating_characteristics <- function(res_list){
  
  # decisions for each treatment and their actual treatment effect
  decisions_TRD <- sapply(res_list$TRD, function(y) {ifelse(y$decision[1] == "STOP", "stopped early", 
                                                            ifelse(y$decision[2] == "SUCCESS", "success", 
                                                                   ifelse(y$decision[2] == "FAILURE", "failure", "ERROR")))})
  decisions_PRD <- sapply(res_list$PRD, function(y) {ifelse(y$decision[1] == "STOP", "stopped early", 
                                                            ifelse(y$decision[2] == "SUCCESS", "success", 
                                                                   ifelse(y$decision[2] == "FAILURE", "failure", "ERROR")))})
  
  decisions <- cbind(as.data.frame(decisions_TRD), as.data.frame(decisions_PRD))
  
  # effect sizes: using the fact that X - Y ~ N(mu_X - mu_Y, sd_X^2 + sd_Y^2 - 2 * cov_XY)
  
  
  cohens_d_TRD <- c()
  for(way_of_administration in c("pill", "IV", "nasal")) {
    mean_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4]^2 + res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1]^2 - 2 * res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_TRD <- c(cohens_d_TRD,
                      sapply(res_list$TRD[grep(pattern=way_of_administration, names(res_list$TRD))], function(y) {
                        mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                        var_baseline_change <- y$response$sigma[4]^2 + y$response$sigma[1]^2 - 2 * y$response$sigma[2]
                        return( (mean_baseline_change - mean_baseline_change_control) / (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                      }))
  }
  
  cohens_d_PRD <- c()
  for(way_of_administration in c("pill", "IV", "nasal")) {
    mean_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4]^2 + res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1]^2 - 2 * res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_PRD <- c(cohens_d_PRD,
                      sapply(res_list$PRD[grep(pattern=way_of_administration, names(res_list$PRD))], function(y) {
                        mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                        var_baseline_change <- y$response$sigma[4]^2 + y$response$sigma[1]^2 - 2 * y$response$sigma[2]
                        return( (mean_baseline_change - mean_baseline_change_control) / (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                      }))
  }
  
  
  
  merge(decisions, cbind(as.data.frame(cohens_d_TRD), as.data.frame(cohens_d_PRD)), by="row.names")
  
    
  # total number of patients
  # number of patients on effective treatments
  # number of patients on ineffective treatments
  # number of patients on control treatments
  
  # estimated treatment effect and difference to true treatment effect
  
  # duration of the trial
  
  # total number of treatments per way of administration
  
  
}