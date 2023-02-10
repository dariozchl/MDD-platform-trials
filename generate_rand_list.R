
# generates a randomization list for the different methods of allocation "block", "block_1", "block_k", "block_sqrt"
# returns object rand list
generate_rand_list <- function(res_list,
                               n_fin,
                               ways_of_administration,
                               applicable_to_PRD,
                               rand_type="block" # "block", "block_1", "block_k", "block_sqrt"
                               ) {
  
  #initialize a list with randomization lists for each population and way of administration
  rand_list <- list("TRD"=rep(list(NA),
                              length(ways_of_administration)+1),
                    "PRD"=rep(list(NA),
                              length(ways_of_administration)+1)
  )
  # name the list entries
  for (i in 1:length(ways_of_administration)) {
    names(rand_list$TRD)[i] <- ways_of_administration[i]
    names(rand_list$TRD)[length(ways_of_administration)+1] <- "admin"
    names(rand_list$PRD)[i] <- ways_of_administration[i]
    names(rand_list$PRD)[length(ways_of_administration)+1] <- "admin"
  }
  
  cohorts_left <- coh_left_check(res_list, applicable_to_PRD)
  # the [-c(1:length(ways_of_administration))] drops the controls from the assessment, since controls are always "active"
  treatments_left <- coh_left_check(res_list, applicable_to_PRD)[-c(1:length(ways_of_administration)),] 

  for(population in 1:2){
    # names of currently active ways of administration
    active_admin <- unique(gsub(pattern="_.*", "", (rownames(treatments_left))[(treatments_left[,population])]))
    # counts treatments without control
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
      
      # relevant if sqrt is used
      ratio_admin_modified <- ratio_admin
      # the non-integer parts of the allocation rate
      # equals probability to include an additional control patient in the block
      prob_include_admin <- ratio_admin-floor(ratio_admin)
      
      # how many blocks should be generated
      # number of blocks in the administration * max number of concurrent arms(+control) * number of ways of administration
      ##rep_admin_block <- (n_fin[[population]]*1.2)*10*length(ways_of_administration)
      rep_admin_block <- 500*2
      # more than n entries should be generated <- ceiling(n[population]/sum(floor(ratio_admin)))
      # initialize randomization list to the administrations
      rand_admin <- c()
      for (rep in 1:rep_admin_block) {
        # checks if an additional control patient is added to the block length in case of non-integer ratios
        add_patients <- as.integer(runif(length(active_admin), 0, 1)<=prob_include_admin)
        # rounds control ratio up or down in case of non-integer ratios, else it leaves the ratio the same
        ratio_admin_modified <- floor(ratio_admin)+add_patients
        
        # sample one random block
        rand_block <- sample(rep(active_admin, ratio_admin))
        # add to existing randomization list to administration
        rand_admin <- c(rand_admin, rand_block)
      }
      
      # writes the generated randomization list for the ways of administration in rand_list
      rand_list[[population]][[length(ways_of_administration)+1]] <- rand_admin

      ## allocate patients to treatment arms within one way of administration
      for(i in 1:number_of_active_admin){

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
        
        # relevant if sqrt is used
        ratio_alloc_modified <- ratio_alloc
        # the non-integer part of the allocation rate to control
        # equals probability to include an additional control patient in the block
        prob_include <- sum(ratio_alloc)-floor(sum(ratio_alloc))
        
        # how many blocks should be generated
        # in each block one patient is allocated to each treatment, so after n_fin blocks at least one arm should finish recruiting
        # in case of overrunning *1.2
        #####rep_block <- n_fin[[population]]*1.2
        rep_block <- 145*2
        # initialize randomization list to arms within one administration
        rand_arm <- c()
        
        for (rep in 1:rep_block) {
          # checks if an additional control patient is added to the block length in case of non-integer ratios
          add_control <- as.integer(runif(1, 0, 1)<=prob_include)
          # rounds control ratio up or down in case of non-integer ratios, else it leaves the ratio the same
          ratio_alloc_modified[1] <- floor(ratio_alloc[1])+add_control
          
          # samples one block
          rand_block <- sample(rep(active_arms_in_admin, ratio_alloc))
          # adds this block to the randomization list
          rand_arm<- c(rand_arm,rand_block)
        }
        
        # writes the generates randomization list for this arm in rand_list
        # active_admin[i] is the name of the currently relevant domain, grep gives the position in the rand_list
        rand_list[[population]][[grep(gsub(pattern = "_.*", "",active_admin[i]),names(rand_list[[population]]))]] <- rand_arm
        
      }
    } # end if open admins
  }
  
  return(rand_list)
}

