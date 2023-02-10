operating_characteristics <- function(res_list){
  
  # decisions for each treatment and their actual treatment effect
  decisions_TRD <- sapply(res_list$TRD, 
                          function(y) {ifelse(y$decision[1] == "stopped early", "stopped early", 
                                              ifelse(y$decision[2] == "success", "success", 
                                                     ifelse(y$decision[2] == "failure", "failure", 
                                                            "ERROR")))})
  decisions_PRD <- sapply(res_list$PRD, 
                          function(y) {ifelse(y$decision[1] == "stopped early", "stopped early", 
                                              ifelse(y$decision[2] == "success", "success", 
                                                     ifelse(y$decision[2] == "failure", "failure", 
                                                            "ERROR")))})
  
  #decisions <- cbind(as.data.frame(decisions_TRD), as.data.frame(decisions_PRD))
  decisions <- merge(as.data.frame(decisions_TRD), as.data.frame(decisions_PRD), by="row.names", all = TRUE)#[-1]
  rownames(decisions) <- decisions$Row.names # merge() turns rownames into a new column "Row.names"
  decisions <- decisions[2:length(decisions)]
  

  # effect sizes: using the fact that X - Y ~ N(mu_X - mu_Y, sd_X^2 + sd_Y^2 - 2 * cov_XY)
  cohens_d_TRD <- c() 
  cohens_d_TRD_est <- c()
  for(way_of_administration in ways_of_administration) {
    mean_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - 
                                    res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4] + 
                                   res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1] - 
                                   2 * res_list$TRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_TRD <- c(cohens_d_TRD,
                      sapply(res_list$TRD[grep(pattern=way_of_administration, 
                                               names(res_list$TRD))], function(y) {
                                                 mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                                                 var_baseline_change <- y$response$sigma[4] + y$response$sigma[1] - 2 * y$response$sigma[2]
                                                 return( (mean_baseline_change_control - mean_baseline_change) / 
                                                           (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                                               }))
                        
    
    cohens_d_TRD_est <- c(cohens_d_TRD_est, 
                          sapply(res_list$TRD[grep(pattern=way_of_administration, 
                                                   names(res_list$TRD))], function(y) {
                                                     return(ifelse(is.null(y$endpoint$cohens_d), 0, y$endpoint$cohens_d))
                                                     }))
  }
  
  cohens_d_PRD <- c()
  cohens_d_PRD_est <- c()
  for(way_of_administration in ways_of_administration) {
    mean_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[2] - 
                                    res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$mean[1]
    var_baseline_change_control <- res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[4] + 
                                   res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[1] - 
                                   2 * res_list$PRD[[paste0(way_of_administration,"_Control")]]$response$sigma[2]
    
    cohens_d_PRD <- c(cohens_d_PRD,
                      sapply(res_list$PRD[grep(pattern=way_of_administration, 
                                               names(res_list$PRD))], function(y) {
                                                 mean_baseline_change <- y$response$mean[2] - y$response$mean[1]
                                                 var_baseline_change <- y$response$sigma[4] + y$response$sigma[1] - 2 * y$response$sigma[2]
                                                 return( (mean_baseline_change_control - mean_baseline_change) / 
                                                           (sqrt( (var_baseline_change + var_baseline_change_control) / 2 )) )
                                               }))
                        
    cohens_d_PRD_est <- c(cohens_d_PRD_est, 
                          sapply(res_list$PRD[grep(pattern=way_of_administration, 
                                                   names(res_list$PRD))], function(y) {
                                                     return(ifelse(is.null(y$endpoint$cohens_d), 0, y$endpoint$cohens_d))
                                                     }))
    
  }
  
  d_TRD <- cohens_d_TRD[order(names(cohens_d_TRD))]
  d_TRD_est <- cohens_d_TRD_est[order(names(cohens_d_TRD_est))]
  
  d_PRD <- cohens_d_PRD[order(names(cohens_d_PRD))]
  d_PRD_est <- cohens_d_PRD_est[order(names(cohens_d_PRD_est))]
  
  
  ocs <- merge(cbind(decisions, d_TRD, d_TRD_est),
               cbind(d_PRD, d_PRD_est),
               by="row.names", all = TRUE)
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  # number of patients per arm
  n_TRD <- sapply(res_list$TRD, 
                  function(y) {nrow(y$data)})
  n_TRD <- n_TRD[order(names(n_TRD))]
  n_PRD <- sapply(res_list$PRD, 
                  function(y) {
                    ifelse(is.null(y$data), 
                           NA, 
                           nrow(y$data))
                    })
  n_TRD <- n_TRD[order(names(n_TRD))]
  n <- merge(as.data.frame(n_TRD),as.data.frame(n_PRD),
             by="row.names", all = TRUE)
  rownames(n) <- n$Row.names # merge() turns rownames into a new column "Row.names"
  n <- n[2:length(n)]
  
  ocs <- merge(ocs, 
               n, 
               by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  # size of control being used for each decision (implement this for interim decisions, too?)
  n_treatment_TRD <- sapply(res_list$TRD, 
                            function(y) {
                              ifelse(is.null(y$endpoint$n_treatment), 
                                     NA, 
                                     y$endpoint$n_treatment)
                              })
  n_treatment_TRD <- n_treatment_TRD[order(names(n_treatment_TRD))]
  n_treatment_PRD <- sapply(res_list$PRD, 
                            function(y) {
                              ifelse(is.null(y$endpoint$n_treatment), 
                                     NA, 
                                     y$endpoint$n_treatment)
                              })
  n_treatment_PRD <- n_treatment_PRD[order(names(n_treatment_PRD))]
  n_control_comparators_TRD <- sapply(res_list$TRD, 
                                      function(y) {
                                        ifelse(is.null(y$endpoint$n_control), 
                                               NA, 
                                               y$endpoint$n_control)
                                        })
  n_control_comparators_TRD <- n_control_comparators_TRD[order(names(n_control_comparators_TRD))]
  n_control_comparators_PRD <- sapply(res_list$PRD, 
                                      function(y) {ifelse(is.null(y$endpoint$n_control), 
                                                          NA, 
                                                          y$endpoint$n_control)
                                        })
  n_control_comparators_PRD <- n_control_comparators_PRD[order(names(n_control_comparators_PRD))]
  n_control_comparators <- merge(cbind(n_treatment_TRD, 
                                       n_control_comparators_TRD),
                                 cbind(n_treatment_PRD,
                                       n_control_comparators_PRD),
                                 by="row.names", all = TRUE)
  rownames(n_control_comparators) <- n_control_comparators$Row.names # merge() turns rownames into a new column "Row.names"
  n_control_comparators <- n_control_comparators[2:length(n_control_comparators)]
                                 
  ocs <- merge(ocs, 
               n_control_comparators, 
               by="row.names")
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  
  
  # duration of each arm
  first_timestamp_TRD <- sapply(res_list$TRD, 
                                function(y) {
                                  ifelse(is.null(y$data), 
                                         NA, 
                                         min(y$data[,3]))
                                  })
  first_timestamp_TRD <- first_timestamp_TRD[order(names(first_timestamp_TRD))]
  
  first_timestamp_PRD <- sapply(res_list$PRD, 
                                function(y) 
                                {
                                  ifelse(is.null(y$data), 
                                         NA, 
                                         min(y$data[,3]))
                                })
  first_timestamp_PRD <- first_timestamp_PRD[order(names(first_timestamp_PRD))]
  
  last_timestamp_TRD <- sapply(res_list$TRD, 
                               function(y) {
                                 ifelse(is.null(y$data), 
                                        NA, 
                                        max(y$data[,3]))
                               })
  last_timestamp_TRD <- last_timestamp_TRD[order(names(last_timestamp_TRD))]
  
  last_timestamp_PRD <- sapply(res_list$PRD, 
                               function(y) {
                                 ifelse(is.null(y$data), 
                                        NA, 
                                        max(y$data[,3]))
                               })
  last_timestamp_PRD <- last_timestamp_PRD[order(names(last_timestamp_PRD))]
  
  duration_TRD <- last_timestamp_TRD - first_timestamp_TRD
  duration_PRD <- last_timestamp_PRD - first_timestamp_PRD
  
  # mean number of active arms (patient level)
  mean_active_arms_TRD <- sapply(res_list$TRD, 
                                 function(y) {
                                   ifelse(is.null(y$data),
                                          NA, 
                                          mean(y$data[,5]))
                                 })
  mean_active_arms_TRD <- mean_active_arms_TRD[order(names(mean_active_arms_TRD))]
  
  mean_active_arms_PRD <- sapply(res_list$PRD, 
                                 function(y) {
                                   ifelse(is.null(y$data),
                                          NA, 
                                          mean(y$data[,5]))
                                 })
  mean_active_arms_PRD <- mean_active_arms_PRD[order(names(mean_active_arms_PRD))]

  ocs <- merge(cbind(ocs,
                     duration_TRD, 
                     first_timestamp_TRD, 
                     last_timestamp_TRD,
                     mean_active_arms_TRD),
               cbind(duration_PRD,
                     first_timestamp_PRD, 
                     last_timestamp_PRD,
                     mean_active_arms_PRD), 
               by="row.names", all = TRUE)
  rownames(ocs) <- ocs$Row.names # merge() turns rownames into a new column "Row.names"
  ocs <- ocs[2:length(ocs)]
  

  

  return(ocs)
  
}












