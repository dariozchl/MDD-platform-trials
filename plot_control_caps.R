

####################### SAMPLE SIZES ###########################################

# analyse patients needed per platform with regard to the allocation method
n_perPlatform <- sim_results_d035  %>% select(nsim, control_cap, admin, n_TRD) %>% 
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) %>%
  group_by(nsim, cap) %>% summarise(n_platform = sum(n_TRD))

n_perPlatform_mean <- n_perPlatform %>% 
  group_by(cap) %>% 
  summarise(n_platform_mean = sum(n_platform) / max(sim_results $nsim))
write.xlsx(n_perPlatform_mean, "n_perPlatform_mean.xlsx")

##
p_nTotal <- 
  n_perPlatform_mean %>% ggplot(aes(x = cap, y = n_platform_mean, group =1))+ 
  ylim(1500, 2500)+
  #geom_line() + 
  geom_point(size = 2) +
  theme_bw()+
  ylab("Sample size per platform") + xlab("Minimal control ratio") +
  ggtitle("Size of platform trial")
#ggsave("N_per_platform.tiff", device = "tiff", width=9, height=4)

# sample size per platform with boxplots
ggplot(n_perPlatform,
       aes(x=n_platform,
           y=cap)) +
  #scale_y_discrete(labels=c("rcts" = "rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  #geom_boxplot(fill="#69b3a2") +
  geom_violin(aes(fill = cap), trim = TRUE, width=1) +
  geom_boxplot(width = 0.1, alpha=0.6 #aes(fill = Allocation), color = "white"
  ) +
  #theme_classic() +
  theme_bw()+
  theme(legend.position = "none")+
  #xlim(1400, 2500)+
  coord_flip() +
  xlab("Sample size per platform") + ylab("Minimal control ratio") +
  ggtitle("Size of platform trial") 

ggsave("SampleSize_comp.png", device = "png", width=6, height=5)

########## NUMBER OF CONTROLS

tibble_n_control_platform <- sim_results %>% 
  filter(treatment_ID == "Control") %>% 
  select(nsim, control_cap, n_TRD) %>% 
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) %>%
  group_by(nsim, cap) %>% summarise(n_control_platform = sum(n_TRD))

ggplot(tibble_n_control_platform,
       aes(x=n_control_platform,
           y=cap)) +
  scale_y_discrete(labels=c("block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Control size per platform") + ylab("Minimal control ratio") +
  ggtitle("Number of controls per platform trial") +
  theme_bw()
ggsave("ControlSize_withBoxes.png", device = "png", width=7, height=4)

n_perPlatform_v2 <- n_perPlatform %>% add_column(type = "platform") %>% rename(size=n_platform)
tibble_n_control_platform <- tibble_n_control_platform %>% add_column(type = "control") %>% rename(size=n_control_platform)

tibble_ctrlAndTotal <- bind_rows(n_perPlatform_v2, tibble_n_control_platform)

## compared with total number of individuals in platform
ggplot(tibble_ctrlAndTotal, aes(x = size, y = cap,
                                #group = type, 
                                color = type)) +
  scale_y_discrete(labels=c("block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  geom_boxplot(width=0.5, position=position_dodge(width=0)#, outlier.shape = NA, #coef=0
  )  +
  xlab("Sample size in platform") + ylab("Minimal control ratio") +
  ggtitle("Sample size of the complete platform trial and the overall controls") +
  theme_bw()

ggsave("Control_and_total.png", device = "png", width=7, height=5)

tibble_n_onTreatment <- sim_results %>% filter(treatment_ID != "Control") %>% 
  select(nsim, control_cap, admin, n_TRD) %>% 
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) %>%
  group_by(nsim, cap) %>% summarise(n_onTreatment = sum(n_TRD)) %>% 
  add_column(type = "on treatment") %>% rename(size=n_onTreatment)

tibble_ctrlAndTreatments <- bind_rows(tibble_ctrlAndTotal, tibble_n_onTreatment)

#compared with total number of individuals in platform
ggplot(tibble_ctrlAndTreatments, aes(x = size, y = cap,
                                     #group = type, 
                                     color = type#, fill = type
)) +
  #scale_y_discrete(labels=c("block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  
  coord_flip() +
  geom_boxplot(width=0.5, #outlier.shape = NA, 
               position=position_dodge(width=0), #coef=0
  )  +
  #geom_violin(trim = FALSE, width=4.75, position=position_dodge(width=0)) +
  #geom_boxplot(width = 0.1, alpha=0.6 #aes(fill = Allocation), color = "white"
  #) +
  xlim(0, 2500) +
  xlab("Sample size in platform") + ylab("Minimal control ratio") +
  ggtitle("Sample size of the complete platform trial and the overall treatments and controls") +
  theme_bw()
ggsave("Control_treatment_and_total_box.png", device = "png", width=9, height=4)

####### ON TRIAL LEVEL
tibble_trial_level <- sim_results  %>%
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) %>%
  filter(treatment_ID != "Control") %>%
  select(nsim, cap, n_TRD, n_control_comparators_TRD)


#tibble_trial_level_v2 <- tibble_trial_level %>% filter(Allocation %in% c("block", "full"))

# on active treatment
ggplot(tibble_trial_level, aes(x=n_TRD, y=cap)) +
  geom_violin(fill="#69b3a2", trim = TRUE) +
  geom_boxplot(width =0.25, alpha=0.6
  ) +
  #scale_y_discrete(labels=c("rcts"="rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  
  coord_flip() +
  theme_bw() +
  #xlim(75, 87) +
  xlab("Patients per arm") + ylab("Minimal control ratio") +
  ggtitle("Patients per experimental treatment arm") 
ggsave("n_per_treatment_comp", device = "png", width=7, height=5)

# control comparators per decision
ggplot(tibble_trial_level, aes(x=n_control_comparators_TRD, y=cap)) +
  #geom_violin(fill="#69b3a2", trim = TRUE, width=1,position=position_dodge(width=0)) +
  geom_boxplot(width =0.75, alpha=0.6
  ) +
  scale_y_discrete(labels=c("rcts"="rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  theme_bw() +
  xlim(0, 600) +
  xlab("Controls per comparison") + ylab("Minimal control ratio") +
  ggtitle("Control comparators per experimental treatment arm") 
ggsave("ControlComparators_box.png", device = "png", width=7, height=5)

####################### ARMS ###################################################

# data number of arms per admin (excluding control)
tibble_arms <- sim_results %>% 
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) %>%
  filter(treatment_ID != "Control") %>% 
  group_by(nsim, cap) %>% count() %>%
  group_by(nsim, cap) %>% summarise(arms = sum(n))

number_of_arms_mean <- tibble_arms %>% 
  group_by(cap) %>% 
  summarise(arms_mean = sum(arms) / max(sim_results$nsim))
write.xlsx(number_of_arms_mean, "number_of_arms.xlsx")

# plot arms per platform (excluding control)
ggplot(tibble_arms,
       aes(x=arms,
           y=cap)) +
  #scale_y_discrete(labels=c("block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  geom_violin(fill="#69b3a2", trim = TRUE, width=1) +
  geom_boxplot(width =0.1, alpha=0.6
  ) +
  #geom_boxplot(fill="#69b3a2") +
  #xlim(10, 30)+
  coord_flip() +
  xlab("Treatments per platform") + ylab("Minimal control ratio") +
  ggtitle("Number of arms per platform trial") +
  theme_bw()
ggsave("Arms_comp.png", device = "png", width=9, height=5)


####################### DURATION ###############################################

tibble_duration <- sim_results %>%  
  filter(treatment_ID != "Control") %>%
  select(nsim, control_cap, admin, duration_TRD) %>% 
  mutate(cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))) 
#minimum duartion tibble_duration$duration_TRD
#tibble_duration %>% filter(N =="40") #%>% #select(duration_TRD) %>% 
#summarise(min_val=min(duration_TRD))

#median duration 
tibble_median_duration <-  tibble_duration %>% group_by(cap) %>% summarise(median_duration= mean(duration_TRD))
write.xlsx(tibble_median_duration, "median_duration.xlsx")

ggplot(tibble_duration, aes(x=duration_TRD, y=cap)) +
  geom_boxplot(fill="#69b3a2") +
  #geom_violin(fill="#69b3a2", trim = TRUE, width=1) +
  #geom_boxplot(width =0.1, alpha=0.6
  #) +
  #scale_y_discrete(labels=c("block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  coord_flip() +
  theme_bw() +
  xlim(0, 200) +
  xlab("Duration per treatment arm in weeks") + ylab("Minimal control ratio") +
  ggtitle("Average duration of treatment arms over all possible effect sizes") 
ggsave("Duration_boxes.png", device = "png", width=7, height=5)



####################### DECISIONS ##############################################

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50")
)
# decisions
sim_results %>% 
  filter(treatment_ID != "Control") %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", #"stopped early", 
                                                        "failure")),
         d = factor(round(d_TRD,2)),
         cap = factor(control_cap, levels = c('0.275',
                    '0.3',
                    '0.325',
                    '0.35',
                    '0.375',
                    '0.4',
                    '0.425',
                    '0.45',
                    '0.475',
                    '0.5'))
  ) %>% 
  group_by(admin, d, decisions_TRD, cap, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(admin, d, cap) %>% mutate(percentage = 100 * n / sum(n)) %>% 
  ggplot(., aes(admin, 
                percentage, 
                color=decisions_TRD)) + 
  geom_point(size=2, alpha = 1, position=position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,100), 
                     breaks = seq(0,100,by=20), 
                     minor_breaks = seq(0,100,by=10)) + 
  xlab("Way of administration") + 
  scale_color_viridis_d(begin = 0.1, 
                        end = 0.9, 
                        name="Decision") +
  ylab("Percentage") + 
  geom_hline(data = data.frame(y = c(5, 80, 80, 80), 
                               d = as.factor(c(0,0.2,0.35,0.5))), 
             aes(yintercept=y), linetype="dotted") +
  facet_grid(cap ~ d, labeller = scenario_labeller) +
  theme_bw() + 
  ggtitle("Percentage of each decision for various true effect sizes and under different ancova methods")

ggsave("decisions_cap.png", device = "png", width=12, height=12)

####################### REJECTIONS #############################################

n_rejections_mean <- sim_results_d035 %>% 
  filter(treatment_ID != "Control") %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", #"stopped early", 
                                                        "failure")),
         d = factor(round(d_TRD,2)),
         cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))
  ) %>% 
  group_by(nsim, d, decisions_TRD, cap, .drop=FALSE) %>% summarise(n=n()) %>%
  filter(decisions_TRD != "success") %>%
  group_by(d, decisions_TRD, cap) %>% summarise(m=mean(n))
  
  scenario_labeller <- labeller(
    `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50")
  )
n_rejections %>% ggplot(.) +
    geom_boxplot(aes(x=cap,
                     y=n,
                     fill=cap)) +
    facet_wrap(~d, nrow=1, 
               labeller = scenario_labeller) +
  theme_bw() + 
  guides(fill="none")+ #no legend
  ylab("Number of rejections") + 
  xlab("Minimum control ratio") +
  ggtitle("Number of rejections per effect size and cap") #+ 
#guides(fill=guide_legend(title="Allocation"))
ggsave("rejections.png", device = "png", width=12, height=5)

  



####################### POWER ##################################################

# data decisions in platform per admin and effect size
pow <- sim_results_d035 %>% filter(treatment_ID != "Control") %>%
  select(nsim, control_cap, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  group_by(d, decisions_TRD, control_cap, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, control_cap) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
write.xlsx(pow, "power_cap.xlsx")
print(xtable(pow, type = "latex"), file = "power.tex")


pow %>%
  ggplot(aes(x = control_cap, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) + geom_line()+
  theme_bw() +
  geom_vline(data = data.frame(x = c(0.2899, 0.309, 0.333, 0.366, 0.4142, 0.5)#, 
                               #d = as.factor(c(0,0.2,0.35,0.5))
                               ), 
             aes(xintercept=x), linetype="dotted") +
  ggtitle("Power for different minimal allocation rates to placebo") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  #geom_hline(yintercept = 80, linetype="dotted") +
  labs(x="lower cap on placebo ratio") +
  labs(y="Power in %")

ggsave("Success_rates_per_cap_just035.png", device = "png", width=9, height=6)

##################################### estimation of effect ####################

effect_size <- sim_results %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2)),
         cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))
  ) %>% select(nsim, cap, d, d_TRD_est) %>% 
  group_by(d, cap) %>% summarise(est = mean(d_TRD_est))
write.xlsx(effect_size, "effect_size.xlsx")

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50")
)
sim_results %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2)),
         cap = factor(control_cap, levels = c('0.275',
                                              '0.3',
                                              '0.325',
                                              '0.35',
                                              '0.375',
                                              '0.4',
                                              '0.425',
                                              '0.45',
                                              '0.475',
                                              '0.5'))
  ) %>%
  ggplot(.) + 
  geom_violin(aes(x=cap, 
                  y=d_TRD_est, 
                  fill=cap)) +
  geom_hline(data = data.frame(y = c(0, 0.2, 0.35, 0.5), 
                               d = as.factor(c(0,0.2,0.35, 0.5))), 
             aes(yintercept=y), linetype="dotted") +
  scale_fill_discrete("Allocation",
                      #breaks=c('block_1', 'block_k', 'block_sqrt', 'block', 'full'),
                      #labels=c("balanced", "k", "sqrt(k)", "sqrt(k) with cap", "sqrt(k) with cap,\n no block")
  )+
  scale_x_discrete(breaks=c())+
  facet_wrap(~d, nrow=1, 
             labeller = scenario_labeller) +
  theme_bw() + 
  ylab("Estimated Effect Size") + 
  xlab("Minimum control ratio") +
  ggtitle("Estimation of effect size") #+ 
#guides(fill=guide_legend(title="Allocation"))
ggsave("effect_size.png", device = "png", width=12, height=5)
