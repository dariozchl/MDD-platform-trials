# This is a function to run the simulations

library(doParallel)
library(tidyverse)
library(ggpubr)
library(viridis)

source("create_cohort_initial.R")
source("coh_left_check.R")
source("total_n.R")
source("update_alloc_ratio.R")
source("create_cohort_new.R")
source("make_decision_trialNEW.R")
source("make_decision_wrapper.R")
source("simulate_trial_MDD.R")
source("operating_characteristics.R")


cohorts_start <- list("pill"=3, "IV"=2, "nasal"=2)

# probability of a treatment being applicable to PRD and TRD patients
cohorts_start_applic_to_TRD <- list("pill"=3, "IV"=2, "nasal"=2)
cohorts_start_applic_to_PRD <- list("pill"=3, "IV"=2, "nasal"=2)

# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list(
  "TRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25))),
  
  "PRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25)))
  )


start.time <- Sys.time()
cores <- detectCores() * 3/4
cl <- makeCluster(cores)
registerDoParallel(cl)

# setting seed ?

nsim <- 1000
sim_results <- NULL

patients_per_timepoint = list(c(30,30), c(20,20))
n_fin <- list(list("TRD"=100,"PRD"=100), list("TRD"=80,"PRD"=80))
scenarios <- expand.grid("patients_per_timepoint"=patients_per_timepoint, "n_fin"=n_fin)


for(i in 1:nrow(scenarios)){
  sim_results_tmp <- foreach(nsim=1:nsim, .combine=rbind, .packages=c("tidyverse", "mvtnorm")) %dopar% {
    single_sim_results <- simulate_trial(cohorts_start=cohorts_start, 
                                         n_int=lapply(scenarios$n_fin[[i]], function(x) x/2), 
                                         n_fin=scenarios$n_fin[[i]],
                                         treatment_effects=treatment_effects, 
                                         ways_of_administration=c("pill", "IV", "nasal"),
                                         alloc_ratio_administration="fixed", 
                                         alloc_ratio_control="fixed",
                                         alloc_ratio_administration_values=NULL, 
                                         alloc_ratio_control_values=0.35,
                                         cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, 
                                         cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD,
                                         sharing_type="concurrent",
                                         patients_per_timepoint=scenarios$patients_per_timepoint[[i]], 
                                         cohorts_per_timepoint=c(0.1,0.05,0.05), 
                                         max_treatments=c(4,3,3), 
                                         trial_end="timepoint", latest_timepoint_treatment_added=60,
                                         #trial_end="pipeline", pipeline_size=c(10,4,4),
                                         p_val_interim=0.4, p_val_final=0.05)
    
    
    ocs <- data.frame(operating_characteristics(single_sim_results) %>% 
                        rownames_to_column("armID") %>% 
                        mutate(admin = gsub("_.*", "", armID), 
                               treatment_ID = case_when(grepl("Control", armID) ~ "Control", TRUE ~ gsub(".*_Treatment", "", armID))) %>% 
                        relocate(admin, treatment_ID, .before=armID) %>% select(-armID),
                      "nsim"=nsim)
    
    return(ocs)
  }
  sim_results <- rbind(sim_results, sim_results_tmp %>% add_column("scenarioID"=i))
  print(paste0(nsim, " Simulations took ", round(difftime(Sys.time(), start.time, units="min"), 2), " minutes"))
}
stopCluster(cl)
sim_results <- as_tibble(sim_results)

saveRDS(sim_results, "sim_results.rds")
sim_results <- readRDS("sim_results.rds")

sim_results <- sim_results %>% mutate(N = case_when(scenarioID %in% 1:2 ~ 100, TRUE ~ 80),
                                      patients_per_timepoint = case_when(scenarioID %in% c(1,3) ~ 30, TRUE ~ 20))


##################################################################


# PLOTS

##################################################################
### decisions
scenario_labeller <- labeller(
  `cohens_d` = c(`0` = "Cohen's d = 0", `0.22` = "Cohen's d = 0.22", `0.35` = "Cohen's d = 0.35", `0.5` = "Cohen's d = 0.50"),
  `N` = c(`80` = "N = 80", `100` = "N = 100")
)
# decisions
sim_results %>% filter(patients_per_timepoint==30) %>% 
  filter(treatment_ID != "Control") %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         cohens_d = factor(round(cohens_d_TRD,2))) %>% 
  group_by(admin, cohens_d, decisions_TRD, N, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(admin, cohens_d, N) %>% mutate(percentage = 100 * n / sum(n)) %>% 
  ggplot(., aes(admin, percentage, color=decisions_TRD)) + geom_point(size=2, alpha = 1, position=position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,by=20), minor_breaks = seq(0,100,by=10)) + 
  xlab("Way of administration") + 
  scale_color_viridis_d(begin = 0.1, end = 0.9, name="Decision") +
  ylab("Percentage") + #ggtitle(title) +
  geom_hline(data = data.frame(y = c(5, 80, 80, 80), cohens_d = as.factor(c(0,0.22,0.35,0.5))), aes(yintercept=y), linetype="dotted") +
  #geom_errorbar(aes(Prior, y=Proportion, ymin=lower, ymax=upper), size=0.5, position=position_dodge(width=0.5)) + 
  facet_grid(N ~ cohens_d, labeller = scenario_labeller) +
  theme_bw() + ggtitle("Percentage of each decision for various true effect sizes and under different sample sizes")

ggsave("decisions_TRD.tiff", device = "tiff", width=9, height=4)


##################################################################
### sample sizes 
### aggregated sample sizes 

# total number of patients per admin
N_per_admin <- sim_results %>% group_by(nsim, patients_per_timepoint, N, admin) %>% summarise(n_TRD = sum(n_TRD), n_PRD = sum(n_PRD))

N_per_admin %>% 
  pivot_longer(., cols=starts_with("n_"), names_to="pop", names_prefix="n_", values_to="n") %>%
  filter(pop=="TRD") %>%
  ggplot(.) + geom_violin(aes(x=admin, y=n, fill=admin)) +
  facet_grid(N~patients_per_timepoint, labeller=labeller(
    `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients"),
    `N` = c(`80`="N = 80", `100`="N = 100")
  )) +
  theme_bw() + ggtitle("Total number of patients per domain") + ylab("") + xlab("Domain") + theme(legend.position="none")
ggsave("N_per_domain.tiff", device = "tiff", width=9, height=4)

# total number of patients per population
N_per_pop <- sim_results %>% group_by(nsim, patients_per_timepoint, N) %>% summarise(n_TRD = sum(n_TRD), n_PRD = sum(n_PRD))
N_per_pop %>% 
  pivot_longer(., cols=starts_with("n_"), names_to="pop", names_prefix="n_", values_to="n") %>%
  ggplot(.) + geom_violin(aes(x=as.factor(pop), y=n, fill=pop)) +
  facet_grid(N~patients_per_timepoint, labeller=labeller(
    `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients"),
    `N` = c(`80`="N = 80", `100`="N = 100")
  )) +
  theme_bw() + ggtitle("Total number of patients per population") + ylab("") + xlab("") + theme(legend.position="none")
ggsave("N_per_pop.tiff", device = "tiff", width=9, height=4)


# total number of patients in platform
N_per_platform <- sim_results %>% group_by(nsim, patients_per_timepoint, N) %>% summarise(n_total = sum(n_TRD+n_PRD))

N_per_platform %>% 
  ggplot(.) + geom_violin(aes(x="", y=n_total), fill="grey") +
  facet_grid(N~patients_per_timepoint, labeller=labeller(
    `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients"),
    `N` = c(`80`="N = 80", `100`="N = 100")
  )) +
  theme_bw() + ggtitle("Total number of patients per platform") + ylab("") + xlab("") + theme(legend.position="none")
ggsave("N_per_platform.tiff", device = "tiff", width=9, height=4)


### sample size of control arms

# per population
sim_results %>% 
  filter(treatment_ID=="Control") %>% select(admin, n_TRD, n_PRD) %>%
  pivot_longer(., cols=starts_with("n_"), names_to="pop", names_prefix="_n", values_to="n") %>%
  ggplot(.) + geom_violin(aes(x=admin, y=n, fill=admin)) +
  facet_wrap(~pop, nrow=1) +
  theme_bw()
ggsave("sample_size_control_population.tiff", device = "tiff", width=9, height=3)

# per recruitment rate
scenario_labeller <- labeller(
  `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients")
  )
sim_results %>% 
  filter(treatment_ID=="Control") %>% 
  ggplot(.) + geom_violin(aes(x=admin, y=n_TRD, fill=admin)) +
  facet_wrap(~patients_per_timepoint, nrow=1, labeller=scenario_labeller) +
  theme_bw()
ggsave("sample_size_control_per_recruitment.tiff", device = "tiff", width=7, height=3)

##################################################################
### number of arms

# number of arms per admin
number_of_arms_per_admin <- sim_results %>% group_by(nsim, patients_per_timepoint, N, admin) %>% count()

number_of_arms_per_admin %>% group_by(patients_per_timepoint, N, admin, "n"=as.factor(n), .drop=F) %>% 
  summarise(counts=n()) %>%
  mutate(counts=counts/max(sim_results$nsim)*100) %>%
  ggplot(.) + 
  geom_bar(aes(x=as.factor(n), y=counts, fill=admin), stat="identity", position = "dodge", width=0.7) + 
  facet_grid(N~patients_per_timepoint, labeller=labeller(
    `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients"),
    `N` = c(`80`="N = 80", `100`="N = 100")
  )) + coord_flip() +
  theme_bw() + ggtitle(paste0("Number of tested compounds per platform")) + ylab("") + xlab("Number of compounds") + 
  scale_fill_viridis_d(end=0.9, name="Domain")
ggsave("treatments_per_admin.tiff", device = "tiff", width=9, height=4)


# number of arms in platform (do not count TRD and PRD separately)
number_of_arms_per_platform <- sim_results %>% group_by(nsim, scenarioID) %>% count()



##################################################################
# estimation of Cohen's d
scenario_labeller <- labeller(
  `cohens_d` = c(`0` = "Cohen's d = 0", `0.22` = "Cohen's d = 0.22", `0.35` = "Cohen's d = 0.35", `0.5` = "Cohen's d = 0.50")
  )
sim_results %>% filter(patients_per_timepoint==30) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(cohens_d = factor(round(cohens_d_TRD,2))) %>%
  ggplot(.) + geom_violin(aes(x=admin, y=cohens_d_TRD_est, fill=admin)) +
  geom_hline(data = data.frame(y = c(0, 0.22, 0.35, 0.5), cohens_d = as.factor(c(0,0.22,0.35, 0.5))), aes(yintercept=y), linetype="dotted") +
  facet_wrap(~cohens_d, nrow=1, labeller = scenario_labeller) +
  theme_bw() + ylab("Estimated Cohen's d") + ggtitle("Estimated Cohen's d")
ggsave("estimated_cohens_d.tiff", device = "tiff", width=9, height=3)


# duration
# priority: duration per arm


# number of treatment arms per admin



