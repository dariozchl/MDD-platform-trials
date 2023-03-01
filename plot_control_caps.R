
####################### DECISIONS ##############################################

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50"),
  `period_o` = c('0'="without period", '1'="with period")
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

ggsave("decisions_TRD.tiff", device = "tiff", width=12, height=12)

####################### POWER ##################################################

# data decisions in platform per admin and effect size
pow <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, control_cap, decisions_TRD, d_TRD, d_TRD_est) %>%
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
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  group_by(d, decisions_TRD, cap, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, cap) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
write.xlsx(pow, "power_cap.xlsx")
print(xtable(pow, type = "latex"), file = "power.tex")


pow %>%
  ggplot(aes(x = cap, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) + geom_line()+
  theme_bw() +
  ggtitle("Power for different minimal allocation rates to placebo") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 80, linetype="dotted") +
  labs(x="lower cap on placebo ratio") +
  labs(y="Power in %")

ggsave("Success_rates_per_cap.png", device = "png", width=9, height=6)

