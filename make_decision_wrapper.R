# checks whether the required sample size for the interim or final analysis is reached and makes a decision when this condition is fulfilled for the first time
make_decision_wrapper <- function(res_list, 
                                  p_val_interim, 
                                  p_val_final,
                                  ancova_period,
                                  sided, 
                                  n_fin, 
                                  n_int, 
                                  timestamp) { 
  for(population in 1:2){
    for(i in 1:length(ways_of_administration)){
      # names of all treatment arms in this way of administration in the population
      arms_within_administration <- grep(ways_of_administration[i], 
                                         names(res_list[[population]]), 
                                         value=TRUE)[-1] # first one is always control, for which no decision should be made
      for(j in arms_within_administration){
        if(!is.null(res_list[[population]][[j]]$data)){
          
          # checks if at least n_fin people are already recruited
          if(nrow(res_list[[population]][[j]]$data) >= n_fin[[population]] & 
             res_list[[population]][[j]]$decision[2] == "none"){
            remove_compound <<- TRUE
            output <- make_decision_trial(res_list = res_list, 
                                          which_pop=c("TRD", "PRD")[population],
                                          which_admin=ways_of_administration[i], 
                                          which_treat=j, 
                                          which_measure = 1,
                                          test_type = "freq",
                                          ancova_period = ancova_period,
                                          interim = FALSE,
                                          control_type = "concurrent",
                                          hdi_perc = c(0.9,0.95), 
                                          p_val = c(p_val_interim,p_val_final), 
                                          sided=sided)
            res_list[[population]][[j]]$decision[2] <- output[[1]]$decision
            res_list[[population]][[j]]$endpoint <- list("mean_effect"=output[[1]][[1]], 
                                                         "conf_int"=output[[1]][[2]], 
                                                         "cohens_d"=output[[2]], 
                                                         "n_tested"=output[[1]][[4]], 
                                                         "n_control"=output[[1]][[5]], 
                                                         "n_treatment"=output[[1]][[6]])
            res_list[[population]][[j]]$end_timestamp <- timestamp
            
            # checks if at least n_int people are already recruited
          } else if(nrow(res_list[[population]][[j]]$data) >= n_int[[population]] & 
                    res_list[[population]][[j]]$decision[1] == "none"){
            output <- make_decision_trial(res_list = res_list, 
                                          which_pop=c("TRD", "PRD")[population], 
                                          which_admin=ways_of_administration[i], 
                                          which_treat=j, 
                                          which_measure = 1,
                                          test_type = "freq",
                                          ancova_period = ancova_period,
                                          interim = TRUE,
                                          control_type = "concurrent",
                                          hdi_perc = c(0.9,0.95), 
                                          p_val = c(p_val_interim,p_val_final), 
                                          sided=sided)
            
            res_list[[population]][[j]]$decision[1] <- output[[1]]$decision
            
            if(output[[1]]$decision=="stopped early"){
              remove_compound <<- TRUE
              res_list[[population]][[j]]$endpoint <- list("mean_effect"=output[[1]][[1]], 
                                                           "conf_int"=output[[1]][[2]], 
                                                           "cohens_d"=output[[2]], 
                                                           "n_tested"=output[[1]][[4]], 
                                                           "n_control"=output[[1]][[5]], 
                                                           "n_treatment"=output[[1]][[6]])
              res_list[[population]][[j]]$end_timestamp <- timestamp
            }
            
          }
        }
      }
    }
  }
            
  return(res_list)
}






