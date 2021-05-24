
#' Helper Function: Update Allocation Ratio
#' @export
update_alloc_ratio <- function(res_list) {

  for(population in 1:2){
    cohorts_left <- coh_left_check(res_list)[-c(1:3),] # the [-c(1:3)] drops the controls from the assessment, since controls are always "active"
    active_admin <- unique(gsub(pattern="_.*", "", (rownames(cohorts_left))[(cohorts_left[,population])])) 
    number_of_active_admin <- length(active_admin)
    
    # all arms in inactive admins should have prob_admin = 0
    inactive_admins <- unique(gsub(pattern="_.*", "", (rownames(cohorts_left))[!(cohorts_left[,population])])) 
    for(inactive_admin in inactive_admins){
      inactive_admin_arms <- grep(inactive_admin, ((names(res_list[[population]]))), value=TRUE)
      for(inactive_arm in inactive_admin_arms){
        res_list[[population]][[inactive_arm]]$prob_admin <- 0
        res_list[[population]][[inactive_arm]]$alloc_ratio <- 0
      }
    }
    
    # for all active arms, we need to assign a specific allocation ratio
    if(length(active_admin) >= 1) {
      for(i in 1:length(active_admin)){
        
        active_arms_in_admin_index <- grep(active_admin[i], ((names(res_list[[population]]))[-c(1:3)])[cohorts_left[,population]])
        active_arms_in_admin <- (((names(res_list[[population]]))[-c(1:3)])[cohorts_left[,population]])[active_arms_in_admin_index]
        
        inactive_arms_in_admin_index <- grep(active_admin[i], ((names(res_list[[population]]))[-c(1:3)])[!cohorts_left[,population]])
        inactive_arms_in_admin <- (((names(res_list[[population]]))[-c(1:3)])[!cohorts_left[,population]])[inactive_arms_in_admin_index]
        
        k <- length(active_arms_in_admin) # number of treatment arms (without control)
        
        if(k >= 1){ # if there are active treatments
          alloc_ratio <- c(1/sqrt(k), rep(1/k,k)) / sum(c(1/sqrt(k), rep(1/k,k))) # standard 1:(1/sqrt(k)) allocation
          res_list[[population]][[paste0(active_admin[i], "_Control")]]$alloc_ratio <- alloc_ratio[1]
          res_list[[population]][[paste0(active_admin[i], "_Control")]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
        } else { # if there is only control active
          alloc_ratio <- 0
        }
        
        if(length(active_arms_in_admin) >= 1){
          for(j in active_arms_in_admin){
            res_list[[population]][[j]]$alloc_ratio <- alloc_ratio[which(j==active_arms_in_admin)]
            res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
          }
        }
        if(length(inactive_arms_in_admin) >= 1){
          for(j in inactive_arms_in_admin){
            res_list[[population]][[j]]$alloc_ratio <- 0
            res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
          }
        }
      }
    }
  }

  return(res_list)
}

# res_list <- update_alloc_ratio(res_list)
# View(res_list)

