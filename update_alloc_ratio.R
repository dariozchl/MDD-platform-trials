
#' Helper Function: Update Allocation Ratio
#' @export
update_alloc_ratio <- function(res_list) {

  for(population in 1:2){
    cohorts_left <- coh_left_check(res_list)
    active_admin <- unique(gsub(pattern="_.*", "", rownames(cohorts_left)[cohorts_left[,population]]))
    number_of_active_admin <- length(active_admin)
    for(i in 1:length(ways_of_administration)){

        active_arms_in_admin_index <- grep(ways_of_administration[i], names(res_list[[population]])[cohorts_left[,population]])
        active_arms_in_admin <- names(res_list[[population]])[active_arms_in_admin_index]

        inactive_arms_in_admin_index <- grep(ways_of_administration[i], names(res_list[[population]])[!cohorts_left[,population]])
        inactive_arms_in_admin <- names(res_list[[population]])[inactive_arms_in_admin_index]

        k <- length(active_arms_in_admin)-1 # number of treatment arms
        alloc_ratio <- c(1/sqrt(k), rep(1/k,k)) / sum(c(1/sqrt(k), rep(1/k,k))) # standard 1:(1/sqrt(k)) allocation
        
        if(length(active_arms_in_admin_index) >= 1){
          for(j in active_arms_in_admin_index){
            res_list[[population]][[j]]$alloc_ratio <- alloc_ratio[which(j==active_arms_in_admin_index)]
            res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
          }
        }
        if(length(inactive_arms_in_admin_index) >= 1){
          for(j in inactive_arms_in_admin_index){
            res_list[[population]][[j]]$alloc_ratio <- NULL
            res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
          }
        }
    }
  }

  return(res_list)
}

# res_list <- update_alloc_ratio(res_list)
# View(res_list)

