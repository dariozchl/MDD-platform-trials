library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggpubr)
#set theme for plots
theme_set(theme_pubr())

# define custom color palette
my_cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

#import simulation results
sim_results <- readRDS("~/sim_results.rds")

# PLOTS
### decisions
scenario_labeller <- labeller(
  `cohens_d` = c(`0` = "d = 0", `0.22` = "d = 0.22", `0.35` = "d = 0.35", `0.5` = "d = 0.50"),
  `N` = c(`40` = "N = 40", `60` = "N = 60", `80` = "N = 80", `100` = "N = 100", `120` = "N = 1200")
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
    `N` = c(`40` = "N = 40", `60` = "N = 60", `80`="N = 80", `100`="N = 100", `120` = "N = 120")
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
    `N` = c(`40` = "N = 40", `60` = "N = 60", `80`="N = 80", `100`="N = 100", `120` = "N = 120")
  )) +
  theme_bw() + ggtitle("Total number of patients per population") + ylab("") + xlab("") + theme(legend.position="none")
ggsave("N_per_pop.tiff", device = "tiff", width=9, height=4)


# total number of patients in platform
N_per_platform <- sim_results %>% group_by(nsim, patients_per_timepoint, N) %>% summarise(n_total = sum(n_TRD+n_PRD))

N_per_platform %>% 
  ggplot(.) + geom_violin(aes(x="", y=n_total), fill="grey") +
  facet_grid(N~patients_per_timepoint, labeller=labeller(
    `patients_per_timepoint` = c(`20` = "Recruitment per month: 20 patients", `30` = "Recruitment per month: 30 patients"),
    `N` = c(`40`="N = 40", `60`="N = 60",`80`="N = 80", `100`="N = 100",`120`="N = 120")
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
    `N` = c(`40`="N = 40",`60`="N = 60",`80`="N = 80", `100`="N = 100",`120`="N = 120")
  )) + coord_flip() +
  theme_bw() + ggtitle(paste0("Number of tested compounds per platform")) + ylab("") + xlab("Number of compounds") + 
  scale_fill_viridis_d(end=0.9, name="Domain")
ggsave("treatments_per_admin.tiff", device = "tiff", width=9, height=9)


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

####################### SAMPLE SIZE ###########################################

# total number of patients in platform
n_per_platform <- sim_results %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(N) %>% summarise(n_platform_mean = sum(n_TRD) / max(sim_results$nsim)) # counting only TRD 
write.xlsx(n_per_platform, "n_per_platform.xlsx")

##
p_nTotal <- n_per_platform %>% ggplot(aes(x = N, y = n_platform_mean, group =1))+ 
  ylim(500, 2750)+
  geom_line() + 
  geom_point(size = 2) +
  theme_bw()+
  ylab("Sample size per platform") + xlab("Sample size per treatment arm N") +
  ggtitle("Size of platform trial")
ggsave("N_per_platform.tiff", device = "tiff", width=9, height=4)

####################### ARMS ###################################################

# data number of arms per admin
number_of_arms_per_admin <- sim_results %>% group_by(nsim, N, admin) %>% count() %>%
  group_by(admin, N) %>% 
  summarise(arms=sum(n)/max(sim_results$nsim))
write.xlsx(number_of_arms_per_admin, "number_of_arms_per_admin.xlsx")

# plot arms per domain
p_armsPerPlatform <- number_of_arms_per_admin %>% group_by(N) %>% summarise(n_arms =sum(arms)) %>%
  ggplot(aes(x = N, y = n_arms, group =1))+ 
  ylim(10, 20)+
  geom_line() + 
  geom_point(size = 2) +
  theme_bw()+
  ylab("Treatments per platform") + xlab("Sample size per treatment arm N")
#ggtitle("Size of platform trial")
ggsave("Arms_per_platform.tiff", device = "tiff", width=9, height=4)

#### RCT

#import results for standard rct with interim analysis and futility threshold 0.5
n_rct <- tibble(type = c("rct", "rct", "rct", "rct", "rct"),
                N = c(40, 60, 80, 100, 120),
                n_per_RCT = c((50+60.58+67.14+73.13)/4, 
                              (75.3+94.7+105.8+114.5)/4, 
                              (99.9+130.5+146.8+155.9)/4, 
                              (125.6+167.8+187.8+197)/4, 
                              (150.4+206.4+228.7+238.3)/4),
                n_per_platform = c(1734.94, 2014.72, 2158.39, 2386.63, 2517.57)
)

#adjust table to fit the table for arms in the platform trial
arms_rct <- n_rct %>% group_by(type, N) %>% summarise(n_arms = n_per_platform/n_per_RCT)

# graph of only the RCT values
#p_armsRCT <- arms_rct %>% ggplot(aes(x = N, y = n_arms, group =1))+ 
#  ylim(10, 35)+
#  geom_line() + 
#  geom_point(size = 2) +
#  theme_bw()+
#  ylab("Treatments per platform") + xlab("Sample size per treatment arm N")

# add type platform to platform table
arms_platform <- number_of_arms_per_admin %>% group_by(N) %>% summarise(n_arms =sum(arms)) %>% add_column(type = "platform")

#combine results for platform and rct and print them in one diagram
bind_rows(arms_platform, arms_rct) %>%
  ggplot(aes(x = N, y = n_arms,
             group = type, 
             color = type,
             linetype = type
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("Number of arms possible with the same ovarall sample size") + 
  ylim(0, 30) +
  labs(y="Number of arms")
ggsave("Arms_y0.tiff", device = "tiff", width=9, height=4)

p_arms+
  labs(fill="dose") 

####################### POWER ##################################################

# data decisions in platform per admin and effect size
pow <- sim_results %>% filter(patients_per_timepoint==30) %>% 
  filter(treatment_ID != "Control") %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         cohens_d = factor(round(cohens_d_TRD,2))) %>% 
  group_by(admin, cohens_d, decisions_TRD, N, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(admin, cohens_d, N) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
write.xlsx(pow, "power.xlsx")

# import data for rct power
pow_rct <- tibble(admin = c("rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct", "rct"),
                  cohens_d = c(0,0,0,0,0, 0.22,0.22,0.22,0.22,0.22, 0.35,0.35,0.35,0.35,0.35, 0.5,0.5,0.5,0.5,0.5),
                  N = c(40,60,80,100,120, 40,60,80,100,120, 40,60,80,100,120, 40,60,80,100,120),
                  percentage = c(4.9,5.1,4.9,4.9,5.2, 26.3,34.4,42.2,48.2,55.3, 49.7,64,73.8,82.7,88.4, 75.1,88.1,94.9,97.9,99.2)
) %>% mutate(cohens_d = as.factor(cohens_d))

# alter platformdata to match the format
pow_platform <- pow %>% select(-c(decisions_TRD,n))

# bind platform and rct data into one tibble and plot them together for d = 0.5
p_pow05 <- bind_rows(pow_platform, pow_rct) %>% filter(cohens_d ==0.5) %>%
  ggplot(aes(x = N, y = percentage,
             group = admin, 
             color = admin,
             linetype = admin
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.5") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 80, linetype="dotted") +
  labs(y="Power in %")

## 0.35
p_pow035 <- bind_rows(pow_platform, pow_rct) %>% filter(cohens_d ==0.35) %>%
  ggplot(aes(x = N, y = percentage,
             group = admin, 
             color = admin,
             linetype = admin
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.35") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 80, linetype="dotted") +
  labs(y="Power in %")

## 0.22
p_pow022 <- bind_rows(pow_platform, pow_rct) %>% filter(cohens_d ==0.22) %>%
  ggplot(aes(x = N, y = percentage,
             group = admin, 
             color = admin,
             linetype = admin
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.22") + theme(plot.title=element_text(hjust=0.5))+
  #geom_hline(yintercept = 80, linetype="dotted") +
  ylim(0, 100) +
  labs(y="Power in %")

## 0
p_pow0 <- bind_rows(pow_platform, pow_rct) %>% filter(cohens_d ==0) %>%
  ggplot(aes(x = N, y = percentage,
             group = admin, 
             color = admin,
             linetype = admin
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0") + theme(plot.title=element_text(hjust=0.5))+
  geom_hline(yintercept = 5, linetype="dotted") +
  ylim(0, 70) +
  labs(y="Type I error in %")

#arrange the 4 plots together
p_successRates <- ggarrange(p_pow0, p_pow022, p_pow035, p_pow05,
                            ncol = 2, nrow = 2,
                            common.legend = TRUE,
                            legend = "bottom" 
                            #legend.grob = 
)
annotate_figure(p_successRates,
                top = text_grob("Success rates for the different domains in the platform and an RCT",
                                #color = "red", 
                                #face = "bold", 
                                size = 14)
)
ggsave("Decisions.tiff", device = "tiff", width=9, height=7)


#arrange the 3 plots together
p_successRates <- ggarrange(p_pow022, p_pow035, p_pow05,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE,
                            legend = "bottom" 
                            #legend.grob = 
)
annotate_figure(p_successRates,
                top = text_grob("Power for the different domains in the platform and an RCT",
                                #color = "red", 
                                #face = "bold", 
                                size = 14)
)
ggsave("Power_3.tiff", device = "tiff", width=9, height=4)

# other version to print different diagrams together
#pow %>%
#  ggplot(aes(x = N, y = percentage, 
#             group = admin, 
#             color = admin,
#             linetype = admin
#  )) + 
#  geom_line(size =1) + geom_point(size = 2) +
#  theme_bw() +
#  #ggtitle("Power for d = 0.5") + 
#  ylab("Success rate in %") +
#  facet_wrap(.~cohens_d, 
#             #scales = "free",
#             labeller=labeller(`cohens_d` = c(`0.5` = "Power for d = 0.5", 
#                                              `0.35` = "Power for d = 0.35", 
#                                              `0.22` = "Power for d = 0.22", 
#                                              `0` = "Type I error"))) 
#ggsave("power_per_effect_size.tiff", device = "tiff", width=9, height=4) 

##############################################
# sample size per platform with boxplots
sample_size_perPlatform <- sim_results %>% select(nsim, N, admin, n_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(nsim, N) %>% summarise(n_platform = sum(n_TRD))

sample_size_perPlatform %>% filter(N == "40")

ggplot(sample_size_perPlatform,
       aes(x=n_platform,
           y=N)) +
  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Sample size per platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Size of platform trial") +
  theme_bw()+
  stat_summary(fun = median,
               geom = "line",
               aes(group = 1)
  )
ggsave("SampleSize_withBoxes.tiff", device = "tiff", width=7, height=4)

################################ arms with Boxes

tibble_arms <- sim_results %>% group_by(nsim, N, admin) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>% count() %>% # number of treatments per
  group_by(nsim, N) %>% summarise(arms = sum(n))

ggplot(tibble_arms,
       aes(x=arms,
           y=N)) +
  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Number of arms per platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Arms per platform trial") +
  theme_bw()+
  stat_summary(fun = median,
               geom = "line",
               aes(group = 1) +
                 geom_line(data = arms_rct, aes(x=n_arms, y=N, group =1), color = "blue")
  )

#adjust table to fit the table for arms in the platform trial
arms_rct <- n_rct %>% group_by(type, N) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>% summarise(n_arms = n_per_platform/n_per_RCT)

### arms with boxes and rct
ggplot(tibble_arms, aes(x=arms, y=N)) +
  geom_line(data = arms_rct, aes(x=n_arms, y=N, group =1), color = "#69b3a2", size=1) + 
  coord_flip() + 
  theme_bw() +
  xlim(0, 40) +
  xlab("Number of arms") + ylab("Sample size per treatment arm N") +
  ggtitle("Arms per platform trial compared with possible number of RCTs") +
  stat_summary(fun = median,
               geom = "line", size=1, color = "#e4605e",
               aes(group = 1)) +
  geom_boxplot(data = tibble_arms, width =0.25,
               aes(x=arms, y=N),
               fill="#e4605e", outlier.shape = NA, coef=0
  ) 
ggsave("Arms_withBoxes.tiff", device = "tiff", width=7, height=4)
ggsave("Arms_withBoxes_noWhiskers.tiff", device = "tiff", width=7, height=4)

######## DURATION

tibble_duration <- sim_results %>%  select(nsim, N, admin, duration_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) 
#minimum duartion per sample size tibble_duration$duration_TRD
test <- tibble_duration %>% filter(N =="40") #%>% #select(duration_TRD) %>% 
#summarise(min_val=min(duration_TRD))
test$duration_TRD

tibble_duration %>% filter(N == "40")
tibble_duration %>% group_by(N) %>% summarise(median_duration= mean(duration_TRD))

ggplot(tibble_duration, aes(x=duration_TRD, y=N, color=admin)) +
  coord_flip() +
  theme_bw() +
  xlim(0, 60) +
  xlab("Duration per treatment arm in months") + ylab("Sample size per treatment arm N") +
  ggtitle("Average duration of treatment arms over all possible effect sizes") +
  geom_boxplot(data = tibble_duration, width =0.25,
               aes(x=duration_TRD, y=N), outlier.shape = NA, #coef=0
  )
ggsave("Duration_withBoxes.tiff", device = "tiff", width=7, height=4)
ggsave("Duration_withBoxes_noWhiskers.tiff", device = "tiff", width=7, height=4)



########## NUMBER OF CONTROLS

#sim_results %>%  select(nsim, N, admin, n_control_comparators_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120")))
tibble_n_control_platform <- sim_results %>% filter(treatment_ID == "Control") %>% select(nsim, N, admin, n_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(nsim, N) %>% summarise(n_control_platform = sum(n_TRD))

ggplot(sample_size_perPlatform,
       aes(x=n_platform,
           y=N)) +
  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Sample size per platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Size of platform trial") +
  theme_bw()+
  stat_summary(fun = median,
               geom = "line",
               aes(group = 1)
  )

sample_size_perPlatformv2 <- sample_size_perPlatform %>% add_column(type = "platform") %>% rename(size=n_platform)
tibble_n_control_platform <- tibble_n_control_platform %>% add_column(type = "control") %>% rename(size=n_control_platform)

tibble_ctrlAndTotal <- bind_rows(sample_size_perPlatformv2, tibble_n_control_platform)

#compared with total number of individuals in platform
ggplot(tibble_ctrlAndTotal, aes(x = size, y = N,
                                #group = type, 
                                color = type)) + 
  coord_flip() +
  geom_boxplot(width=0.5, outlier.shape = NA, #coef=0
  )  +
  stat_summary(fun = median,
               geom = "line",
               aes(group=type))+
  xlab("Sample size in platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Sample size of the complete platform trial and the overall controls") +
  theme_bw()
ggsave("Control_and_total.tiff", device = "tiff", width=7, height=4)
ggsave("Control_and_total_noWhiskers.tiff", device = "tiff", width=7, height=4)

#################### Control, treatments and platform overall

tibble_n_onTreatment <- sim_results %>% filter(treatment_ID != "Control") %>% select(nsim, N, admin, n_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(nsim, N) %>% summarise(n_onTreatment = sum(n_TRD)) %>% add_column(type = "on treatment") %>% rename(size=n_onTreatment)

tibble_ctrlAndTreatments <- bind_rows(tibble_ctrlAndTotal, tibble_n_onTreatment)

#compared with total number of individuals in platform
ggplot(tibble_ctrlAndTreatments, aes(x = size, y = N,
                                     #group = type, 
                                     color = type)) + 
  coord_flip() +
  geom_boxplot(width=0.5, outlier.shape = NA, position=position_dodge(width=0), #coef=0
  )  +
  stat_summary(fun = median,
               geom = "line",
               aes(group=type))+
  xlim(0, 3250) +
  xlab("Sample size in platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Sample size of the complete platform trial and the overall treatments and controls") +
  theme_bw()
ggsave("Control_treatment_and_total.tiff", device = "tiff", width=9, height=4)
ggsave("Control_treatment_and_total_noWhiskers.tiff", device = "tiff", width=9, height=4)

################################################### WITH ADMIN ###########################
tibble_n_control_platform_admin <- sim_results %>% filter(treatment_ID == "Control") %>% select(nsim, N, admin, n_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(nsim, N, admin) %>% summarise(n_control_platform = sum(n_TRD))

tibble_n_platform_admin <- sim_results %>% select(nsim, N, admin, n_TRD) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>%
  group_by(nsim, N, admin) %>% summarise(n_admin = sum(n_TRD))


tibble_n_platform_admin <- tibble_n_platform_admin %>% add_column(type = "platform") %>% rename(size=n_admin)
tibble_n_control_platform_admin <- tibble_n_control_platform_admin %>% add_column(type = "control") %>% rename(size=n_control_platform)

tibble_ctrlAndTotal_admin <- bind_rows(tibble_n_platform_admin, tibble_n_control_platform_admin)


##

ggplot(tibble_duration, aes(x=duration_TRD, y=N, color=admin)) +
  coord_flip() +
  theme_bw() +
  xlab("Duration per treatment arm in months") + ylab("Sample size per treatment arm N") +
  ggtitle("Average duration of treatment arms over all possible effect sizes") +
  geom_boxplot(data = tibble_duration, width =0.25,
               aes(x=duration_TRD, y=N))


options(ggplot2.discrete.fill = c("grey", "white"))
ggplot(tibble_ctrlAndTotal_admin, aes(x = size, y = N,
                                      #group = type, 
                                      fill=type)) + 
  coord_flip() +
  geom_boxplot(data = tibble_ctrlAndTotal_admin, width =0.25,
               aes(x=size, y=N, color = admin), outlier.shape = NA, #coef=0
  )+
  #stat_summary(fun = median,
  #             geom = "line",
  #             aes(group=type))+
  xlab("Sample size in platform") + ylab("Sample size per treatment arm N") +
  ggtitle("Sample size of the complete platform trial and the overall controls per domain") +
  theme_bw()
ggsave("Control_and_total_admin_messy.tiff", device = "tiff", width=12, height=6)

#######################

tibble_summary_control <- tibble_ctrlAndTotal %>% group_by(type, N) %>% summarise(n_mean = sum(size) / max(tibble_ctrlAndTotal$nsim))
#tibble_summary_control %>% group_by(N) %>% summarize(perc = n_mean[tibble_summary_control$type=="control"]/n_mean[tibble_summary_control$type=="platform")

ctrl_perc <- tibble(N = c(40, 60, 80, 100, 120),
                    perc =tibble_summary_control$n_mean[tibble_summary_control$type=="control"]/tibble_summary_control$n_mean[tibble_summary_control$type=="platform"]
) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120")))

ggplot(ctrl_perc, aes(x=N, y=perc, group =1)) +
  geom_line(color="#69b3a2", size =1) + geom_point(size = 2, color ="#69b3a2") +
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype="dotted") +
  ylim(0, 1) +
  labs(y="Proportion of control")
ggsave("Proportion_of_control.tiff", device = "tiff", width=7, height=4)












########################################################
#### RCT

#import results for standard rct with interim analysis and futility threshold 0.5
n_rct <- tibble(type = c("rct", "rct", "rct", "rct", "rct"),
                N = c(40, 60, 80, 100, 120),
                n_per_RCT = c((50+60.58+67.14+73.13)/4, 
                              (75.3+94.7+105.8+114.5)/4, 
                              (99.9+130.5+146.8+155.9)/4, 
                              (125.6+167.8+187.8+197)/4, 
                              (150.4+206.4+228.7+238.3)/4),
                n_per_platform = c(1734.94, 2014.72, 2158.39, 2386.63, 2517.57)
)

#adjust table to fit the table for arms in the platform trial
arms_rct <- n_rct %>% group_by(type, N) %>% mutate(N = factor(N, levels=c("40", "60", "80", "100", "120"))) %>% summarise(n_arms = n_per_platform/n_per_RCT)

# graph of only the RCT values
#p_armsRCT <- arms_rct %>% ggplot(aes(x = N, y = n_arms, group =1))+ 
#  ylim(10, 35)+
#  geom_line() + 
#  geom_point(size = 2) +
#  theme_bw()+
#  ylab("Treatments per platform") + xlab("Sample size per treatment arm N")

# add type platform to platform table
arms_platform <- number_of_arms_per_admin %>% group_by(N) %>% summarise(n_arms =sum(arms)) %>% add_column(type = "platform")

#combine results for platform and rct and print them in one diagram
bind_rows(arms_platform, arms_rct) %>%
  ggplot(aes(x = N, y = n_arms,
             group = type, 
             color = type,
             linetype = type
  )) + 
  geom_line() + geom_point(size = 2) +
  theme_bw() +
  ggtitle("Number of arms possible with the same ovarall sample size") + 
  ylim(0, 30) +
  labs(y="Number of arms")

