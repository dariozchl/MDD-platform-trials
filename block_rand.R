
update_alloc_ratio_block <- function(res_list,
                                     n,
                                     ways_of_administration,
                                     applicable_to_PRD,
                                     timestamp=0, 
                                     time_periode=0,
                                     rand_type="block" # "block", "block_1", "block_k", "block_sqrt"
                                     ) {
  
  cohorts_left <- coh_left_check(res_list, applicable_to_PRD)
  # the [-c(1:length(ways_of_administration))] drops the controls from the assessment, since controls are always "active"
  treatments_left <- coh_left_check(res_list, applicable_to_PRD)[-c(1:length(ways_of_administration)),] 
  # initialize vector that will contain the number of active treatments per admin
  #k_vector <- rep(0,length(ways_of_administration))
  
  for(population in 1:2){
    # names of currently active ways of administration
    active_admin <- unique(gsub(pattern="_.*", "", (rownames(treatments_left))[(treatments_left[,population])]))
    # only counts treatments without control
    active_admin_rep <- table(gsub(pattern="_.*", "", (rownames(treatments_left))[(treatments_left[,population])]))
    number_of_active_admin <- length(active_admin)
    
    if(number_of_active_admin >= 1) {
      ## allocate patients to way of administration
      # initialize vector for number of active treatments per domain
      n_treatments_per_domain <- rep(0, number_of_active_admin)
      # fill vector
      for(i in 1:number_of_active_admin){
        n_treatments_per_domain[i] <- active_admin_rep[active_admin[i]]
      }
      
      # set randomization ratio to each way of administration depending on type of randomization to control
      #ratio_admin <- rep(1, number_of_active_admin)
      if(rand_type == "block_1"){
        ratio_admin <- sapply(X=n_treatments_per_domain,FUN=function(x) x+1)
      } else if(rand_type == "block_k"){
        ratio_admin <- n_treatments_per_domain
      } else if(rand_type == "block_sqrt"){
        ratio_admin <- sapply(X=n_treatments_per_domain,FUN=function(x) x+sqrt(x))
      } else if(rand_type == "block"){
        #cap at 35% allocation to control, transform percentage to ratio via k*cap/(1-cap)
        ratio_admin <- sapply(X=n_treatments_per_domain,FUN=function(x) x+max(sqrt(x), x*0.35/(1-0.35)))
      }
      
      # if sqrt
      ratio_admin_modified <- ratio_admin
      # the non-integer parts of the allocation rate
      # equals probability to include an additional control patient in the block
      prob_include_admin <- ratio_admin-floor(ratio_admin)
      
      # more than n entries should be generated
      rep_admin_block <- ceiling(n[population]/sum(floor(ratio_admin)))
      # initialize randomization list to the administrations
      rand_admin <- c()
      for (i in 1:rep_admin_block) {
        # checks if an additional control patient is added to the block length in case of non-integer ratios
        add_patients <- as.integer(runif(length(active_admin), 0, 1)<=prob_include_admin)
        # rounds control ratio up or down in case of non-integer ratios, else it leaves the ratio the same
        ratio_admin_modified <- floor(ratio_admin)+add_patients
        
        # sample one random block
        rand_block <- sample(rep(active_admin, ratio_admin))
        # add to existing randomization list to administration
        rand_admin <- c(rand_admin, rand_block)
      }
      # [1:n] only grabs the ones we need
      n_admins <- table(rand_admin[1:n[population]]) 
      
      ## allocate patients to treatment arms within one way of administration
      for(i in 1:number_of_active_admin){
        
        # grab number of patients that were randomized to this way of administration
        n_admin <- n_admins[active_admin[i]]
        
        # indices of the currently recruiting arms in this way of administration
        active_arms_in_admin_index <- grep(active_admin[i],
                                           ((names(res_list[[population]])))[cohorts_left[,population]])
        # names of the currently recruiting arms in this way of administration
        active_arms_in_admin <- (((names(res_list[[population]])))[cohorts_left[,population]])[active_arms_in_admin_index]
        # number of active treatment arms (without control)
        k <- length(active_arms_in_admin)-1
        
        # select mode of block randomization
        if(rand_type == "block_1"){
          control_ratio <- 1
        } else if(rand_type == "block_k"){
          control_ratio <- k
        } else if(rand_type == "block_sqrt"){
          control_ratio <- sqrt(k)
        } else if(rand_type == "block"){
          #cap at 35% allocation to control, transform percentage to ratio via k*cap/(1-cap)
          control_ratio <- max(sqrt(k), k*0.35/(1-0.35))
        }
        
        # randomization ratio control_ratio:1:1 ... :1
        # first element allocatio ratio to control, rest for active treatments
        ratio_alloc <- c(control_ratio,rep(1, k))
        
        # if sqrt
        ratio_alloc_modified <- ratio_alloc
        # the non-integer part of the allocation rate to control
        # equals probability to include an additional control patient in the block
        prob_include <- sum(ratio_alloc)-floor(sum(ratio_alloc))
        
        # more than n_admin entries should be generated
        rep_block <- ceiling(n_admin/sum(ratio_alloc))
        # initialize randomization list to arms within one administration
        rand_list <- c()
        
        for (i in 1:rep_block) {
          # checks if an additional control patient is added to the block length in case of non-integer ratios
          add_control <- as.integer(runif(1, 0, 1)<=prob_include)
          # rounds control ratio up or down in case of non-integer ratios, else it leaves the ratio the same
          ratio_alloc_modified[1] <- floor(ratio_alloc[1])+add_control
          
          # samples one block
          rand_block <- sample(rep(active_arms_in_admin, ratio_alloc))
          # adds this block to the randomization list
          rand_list<- c(rand_list,rand_block)
        }
        
        # [1:n] only grab the ones we need
        n_all_arms <- as.data.frame(table(rand_list[1:n[population]])) # turns rownames into a new column "Var1"
        rownames(n_all_arms) <- n_all_arms$Var1 
        n_all_arms <- n_all_arms[2:length(n_all_arms)]
        
        # get responses for the allocated number of patients per arm
        for (i in row.names(n_all_arms)) {
          # ^ asserts that we are at the start. $ asserts that we are at the end. 
          # If there were treatment1 and treatment10, then "treatment1" would be found twice.
          n_arm <- n_all_arms[grep(paste0("^", i, "$"), 
                                   rownames(n_all_arms)),1] 
          #response probability for the specific arm
          response <- res_list[[population]][[i]]$response
          
          if(n_arm >= 1){
            draw <- rmvnorm(n=n_arm, 
                            mean=response$mean, 
                            sigma=response$sigma)
            draw <- cbind(draw, 
                          timestamp, 
                          time_periode,
                          "active_arms"=sum(n_treatments_per_domain),
                          "active_arms_admin"=k) 
            res_list[[population]][[i]]$data <- rbind(res_list[[population]][[i]]$data, draw)
          }
        }
      }
    } # end if open admins
  }
  
  return(res_list)
}
    
