make_decision_wrapper <- function(res_list) { 
  for(population in 1:2){
    for(i in 1:length(ways_of_administration)){
      arms_within_administration <- grep(ways_of_administration[i], names(res_list[[population]]), value=TRUE)[-1] # first one is always control, for which no decision should be made
      for(j in arms_within_administration){
        if(!is.null(res_list[[population]][[j]]$data)){
          if(nrow(res_list[[population]][[j]]$data) > 100 & res_list[[population]][[j]]$decision[2] == "none"){
            output <- make_decision_trial(results = res_list, which_pop=c("TRD", "PRD")[population], 
                                          which_admin=ways_of_administration[i], 
                                          which_treat=j, which_measure = 1,
                                          test_type = "freq", interim = FALSE,
                                          control_type = "concurrent",
                                          hdi_perc = c(0.9,0.95), p_val = c(0.25,0.05))
            res_list[[population]][[j]]$decision[2] <- output[[1]]$decision
            res_list[[population]][[j]]$endpoint <- list("mean_effect"=output[[1]][[1]], "conf_int"=output[[1]][[2]], "cohens_d"=output[[2]])
            } else if(nrow(res_list[[population]][[j]]$data) > 50 & res_list[[population]][[j]]$decision[1] == "none"){
            output <- make_decision_trial(results = res_list, which_pop=c("TRD", "PRD")[population], 
                                            which_admin=ways_of_administration[i], 
                                            which_treat=j, which_measure = 1,
                                            test_type = "freq", interim = TRUE,
                                            control_type = "concurrent",
                                            hdi_perc = c(0.9,0.95), p_val = c(0.25,0.25))
            res_list[[population]][[j]]$decision[1] <- output[[1]]$decision
            res_list[[population]][[j]]$endpoint <- list("mean_effect"=output[[1]][[1]], "conf_int"=output[[1]][[2]], "cohens_d"=output[[2]])
          }
        }
      }
    }
  }
  return(res_list)
}






