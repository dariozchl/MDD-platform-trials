
#' Helper Function: Update Allocation Ratio
#' @export
update_alloc_ratio <- function(res_list, 
                               ways_of_administration) {
  
  # the [-c(1:3)] drops the controls from the assessment, since controls are always "active"
  cohorts_left <- coh_left_check(res_list)[-c(1:length(ways_of_administration)),] 
  
  k_vector <- rep(0,length(ways_of_administration)) # vector that will contain the number of active treatments per admin
  
  for(population in 1:2){
    active_admin <- unique(gsub(pattern="_.*", "", (rownames(cohorts_left))[(cohorts_left[,population])])) 
    number_of_active_admin <- length(active_admin)
    
    # all arms in inactive admins should have alloc_ratio=0 and prob_admin=0
    inactive_admins <- unique(gsub(pattern="_.*", "", (rownames(cohorts_left))[!(cohorts_left[,population])])) 
    for(inactive_admin in inactive_admins){
      inactive_admin_arms <- grep(inactive_admin, ((names(res_list[[population]]))), value=TRUE)
      for(inactive_arm in inactive_admin_arms){
        res_list[[population]][[inactive_arm]]$prob_admin <- 0
        res_list[[population]][[inactive_arm]]$alloc_ratio <- 0
      }
    }
    
    # for all active admins and arms, we need to assign a specific allocation ratio
    if(length(active_admin) >= 1) {
      for(i in 1:length(active_admin)){
        
        active_arms_in_admin_index <- grep(active_admin[i], 
                                           ((names(res_list[[population]]))[-c(1:3)])[cohorts_left[,population]])
        active_arms_in_admin <- (((names(res_list[[population]]))[-c(1:3)])[cohorts_left[,population]])[active_arms_in_admin_index]
        
        inactive_arms_in_admin_index <- grep(active_admin[i], 
                                             ((names(res_list[[population]]))[-c(1:3)])[!cohorts_left[,population]])
        inactive_arms_in_admin <- (((names(res_list[[population]]))[-c(1:3)])[!cohorts_left[,population]])[inactive_arms_in_admin_index]
        
        k <- length(active_arms_in_admin) # number of treatment arms (without control)
        k_vector[which(ways_of_administration==active_admin[i])] <- k 
        if(k >= 1){ # if there are active treatments
          if(k>3){alloc_ratio <- c(0.35, rep(0.65/k,k))} else {
            # standard 1:(1/sqrt(k)) allocation with cap at 0.35 to control
            alloc_ratio <- c(1/sqrt(k), rep(1/k,k)) / sum(c(1/sqrt(k), rep(1/k,k)))} 
          res_list[[population]][[paste0(active_admin[i], "_Control")]]$alloc_ratio <- alloc_ratio[1]
        } else { # if there is only control active
          alloc_ratio <- 0
        }
        
        if(length(active_arms_in_admin) >= 1){
          for(j in active_arms_in_admin){
            res_list[[population]][[j]]$alloc_ratio <- (alloc_ratio[-1])[which(j==active_arms_in_admin)]
          }
        }
        if(length(inactive_arms_in_admin) >= 1){
          for(j in inactive_arms_in_admin){
            res_list[[population]][[j]]$alloc_ratio <- 0
          }
        }
      }
    }
      
    # set allocation ratio across domains relative to number of treatment arms and control size
    # weight of all compounds
    weight_vector <- sapply(X=k_vector,FUN=function(x) x+sqrt(x))
    # for each arm in each admin, change prob_admin
    for(i in 1:length(ways_of_administration)){
      for(j in grep(ways_of_administration[i],names(res_list[[population]]), value = TRUE)){
        res_list[[population]][[j]]$prob_admin <- ifelse(sum(weight_vector)==0, 0, weight_vector[i]/sum(weight_vector)) 
      }
    }
  }
  
  return(res_list)
}
