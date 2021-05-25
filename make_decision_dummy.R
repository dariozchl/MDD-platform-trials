# a dummy make_decision function that randomly makes decisions
make_decision_dummy <- function(res_list) { 
  for(population in 1:2){
    for(i in 1:length(ways_of_administration)){
      arms_within_administration <- grep(ways_of_administration[i], names(res_list[[population]]), value=TRUE)[-1] # first one is always control, for which no decision should be made
      for(j in arms_within_administration){
        if(!is.null(res_list[[population]][[j]]$endpoint)){
          if(nrow(res_list[[population]][[j]]$endpoint) > 100 & res_list[[population]][[j]]$decision[2] == "none"){
            res_list[[population]][[j]]$decision[2] <- sample(c("SUCCESS", "FAILURE"), size=1)
          } else if(nrow(res_list[[population]][[j]]$endpoint) > 50 & res_list[[population]][[j]]$decision[1] == "none"){
            res_list[[population]][[j]]$decision[1] <- sample(c("CONTINUE", "STOP"), size=1)
          }
        }
      }
    }
  }
  return(res_list)
}


