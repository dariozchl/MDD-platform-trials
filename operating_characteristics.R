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
  
  # number of patients per arm
  n_TRD <- sapply(res_list$TRD, function(y) {nrow(y$data)})
  n_PRD <- sapply(res_list$PRD, function(y) {nrow(y$data)})
  
  n <- cbind(as.data.frame(n_TRD), as.data.frame(n_PRD))
  
  
  # merge ocs and n
  ocs <- merge(ocs, n, by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  # duration of each arm
  first_timestamp_TRD <- sapply(res_list$TRD, function(y) min(y$data[,3]))
  first_timestamp_PRD <- sapply(res_list$PRD, function(y) min(y$data[,3]))
  last_timestamp_TRD <- sapply(res_list$TRD, function(y) max(y$data[,3]))
  last_timestamp_PRD <- sapply(res_list$PRD, function(y) max(y$data[,3]))
  last_timestamp <- cbind(as.data.frame(last_timestamp_TRD), as.data.frame(last_timestamp_PRD))
  first_timestamp <- cbind(as.data.frame(first_timestamp_TRD), as.data.frame(first_timestamp_PRD))
  duration_each_arm <- last_timestamp - first_timestamp
  names(duration_each_arm) <- c("duration_TRD", "duration_PRD")
  
  ocs <- merge(ocs, cbind(duration_each_arm, first_timestamp_TRD, last_timestamp_TRD, first_timestamp_PRD, last_timestamp_PRD), by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  
  return(ocs)
  
}












