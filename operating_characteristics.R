operating_characteristics <- function(res_list){
  
  # decisions for each treatment and their actual treatment effect
  decisions_TRD <- sapply(res_list$TRD, function(y) {ifelse(y$decision[1] == "stopped early", "stopped early", 
                                                            ifelse(y$decision[2] == "success", "success", 
                                                                   ifelse(y$decision[2] == "failure", "failure", "ERROR")))})
  decisions_PRD <- sapply(res_list$PRD, function(y) {ifelse(y$decision[1] == "stopped early", "stopped early", 
                                                            ifelse(y$decision[2] == "success", "success", 
                                                                   ifelse(y$decision[2] == "failure", "failure", "ERROR")))})
  
  decisions <- cbind(as.data.frame(decisions_TRD), as.data.frame(decisions_PRD))
  

  # effect sizes: using the fact that X - Y ~ N(mu_X - mu_Y, sd_X^2 + sd_Y^2 - 2 * cov_XY)
  cohens_d_TRD <- c(); cohens_d_TRD_est <- c()
  for(way_of_administration in c("pill", "IV", "nasal")) {
    mean_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4] + res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1] - 2 * res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_TRD <- c(cohens_d_TRD,
                      sapply(res_list$TRD[grep(pattern=way_of_administration, names(res_list$TRD))], function(y) {
                        mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                        var_baseline_change <- y$response$sigma[4] + y$response$sigma[1] - 2 * y$response$sigma[2]
                        return( (mean_baseline_change_control - mean_baseline_change) / (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                      }))
    
    cohens_d_TRD_est <- c(cohens_d_TRD_est, 
                          sapply(res_list$TRD[grep(pattern=way_of_administration, names(res_list$TRD))], function(y) {return(ifelse(is.null(y$endpoint$cohens_d), 0, y$endpoint$cohens_d))}))
  }
  
  cohens_d_PRD <- c(); cohens_d_PRD_est <- c()
  for(way_of_administration in c("pill", "IV", "nasal")) {
    mean_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4] + res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1] - 2 * res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_PRD <- c(cohens_d_PRD,
                      sapply(res_list$PRD[grep(pattern=way_of_administration, names(res_list$PRD))], function(y) {
                        mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                        var_baseline_change <- y$response$sigma[4] + y$response$sigma[1] - 2 * y$response$sigma[2]
                        return( (mean_baseline_change_control - mean_baseline_change) / (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                      }))
    cohens_d_PRD_est <- c(cohens_d_PRD_est, 
                          sapply(res_list$PRD[grep(pattern=way_of_administration, names(res_list$PRD))], function(y) {return(ifelse(is.null(y$endpoint$cohens_d), 0, y$endpoint$cohens_d))}))
    
  }
  
  
  ocs <- merge(decisions, cbind(as.data.frame(cohens_d_TRD), as.data.frame(cohens_d_TRD_est), as.data.frame(cohens_d_PRD), as.data.frame(cohens_d_PRD_est)), by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  # total number of patients
  n_TRD <- sapply(res_list$TRD, function(y) {nrow(y$data)})
  n_PRD <- sapply(res_list$PRD, function(y) {nrow(y$data)})
  
  n <- cbind(as.data.frame(n_TRD), as.data.frame(n_PRD))
  
  ocs <- merge(ocs, n, by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  return(ocs)
  
  
  # duration of the trial
  
  # total number of treatments per way of administration
  
  
}


operating_characteristics(results) 

# probability that an effect size > crit_d is claimed as success
# probability that an effect size = 0 is claimed as success
# library(tidyverse)
# ocs %>% filter(cohens_d_TRD > 0.29) %>% group_by(decisions_TRD) %>% summarise(n=n())
