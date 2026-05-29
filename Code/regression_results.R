#libraries #####
library(dplyr)
library(fixest)
library(marginaleffects)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(broom)
library(stringr)
library(emmeans)
library(RColorBrewer)
library(modelsummary)
#load data ####
load('data/usa_score_complete_2024_geminitemp.RData')

qs <- quantile(usa_survey_emp_gemtemp$incwage, probs = seq(0, 1, 0.1), na.rm = TRUE)

usa_survey_emp_gemtemp$nchlt5<- haven::zap_label(usa_survey_emp_gemtemp$nchlt5)
usa_survey_emp_gemtemp<- usa_survey_emp_gemtemp%>%
  mutate(sex_cat= relevel(factor(sex_cat),ref = 'Male'),
         marst_cat= relevel(factor(marst_cat),ref='Married'),
         race_cat= relevel(factor(race_cat),ref='White'),
         education= relevel(factor(education),ref='Less than High School'),
         age_class= relevel(factor(age_class),ref='25'),
         soc_1= paste0(substr(SOC,1,1)),
         nchlt5_class= case_when(nchlt5==0~'0',
                                 nchlt5==1~'1',
                                 nchlt5>=2~'2 or more'),
         wage_class_dec = cut(incwage, breaks = qs, include.lowest = TRUE,labels=c("1st", "2nd","3rd", "4th", "5th","6th","7th","8th","9th","10th")))
##############################################################################
#-----------------------------------MODELS------------------------------------
##############################################################################
##COMPLEMENT####
usa_survey_emp_gemtemp$prop_mean_com_stand<- (usa_survey_emp_gemtemp$complement_gen_ai_prop1-mean(usa_survey_emp_gemtemp$complement_gen_ai_prop1, na.rm=T))/sd(usa_survey_emp_gemtemp$complement_gen_ai_prop1,na.rm=T)
model_com<-feols(prop_mean_com_stand~ age_class+
                   race_cat*sex_cat+education*sex_cat+ 
                   wage_class_dec*sex_cat+marst_cat+nchlt5_class*sex_cat| puma, data = usa_survey_emp_gemtemp,
                 vcov='cluster', weights = ~ perwt) 

summary(model_com)
##REPLACE####
usa_survey_emp_gemtemp$prop_mean_rep_stand<- (usa_survey_emp_gemtemp$replace_gen_ai_prop1-mean(usa_survey_emp_gemtemp$replace_gen_ai_prop1, na.rm=T))/sd(usa_survey_emp_gemtemp$replace_gen_ai_prop1,na.rm=T)
model_rep<-feols(prop_mean_rep_stand~ age_class+
                   race_cat*sex_cat+education*sex_cat+ 
                   wage_class_dec*sex_cat+marst_cat+nchlt5_class*sex_cat | puma, data = usa_survey_emp_gemtemp,
                 vcov='cluster', weights = ~ perwt) 

summary(model_rep)

#predicted scores ####
##education ####
specs <- emmeans(model_com, 
                 ~ sex_cat
                 | education,
                 data = usa_survey_emp_gemtemp,
                 type = "response",
                 rg.limit= 60000)

cont <- contrast(specs, method = "revpairwise")
summary(cont)

df_plot_sign <- as.data.frame(summary(cont, infer = TRUE))
df_plot_sign<- df_plot_sign%>%
  mutate(
    education = factor(education, levels = c("Less than High School","High School Diploma", "Bachelor's Degree", "Master's Degree", "Doctoral Degree"))
  )

specs_rep <- emmeans(model_rep, 
                     ~ sex_cat
                     | education,
                     data = usa_survey_emp_gemtemp,
                     type = "response",
                     rg.limit=60000)

cont_rep <- contrast(specs_rep, method = "revpairwise")
summary(cont_rep) 

df_plot_sign_rep <- as.data.frame(summary(cont_rep, infer = TRUE))
df_plot_sign_rep<- df_plot_sign_rep%>%
  mutate(
    education = factor(education, levels = c("Less than High School","High School Diploma", "Bachelor's Degree", "Master's Degree", "Doctoral Degree"))
  )

df_plot_education<-rbind( df_plot_sign%>%
                            mutate(type= 'Complement'), df_plot_sign_rep%>%
                            mutate(type= 'Replace'))

plot_sign_education <-ggplot(df_plot_education, aes(x = education, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), 
                  position = position_dodge(width = 0.6),
                  size = 0.40) +
  scale_color_brewer(palette = 'Set2')+
  scale_y_continuous(breaks = seq(-0.2,0.5,.1),
                     labels = seq(-0.2,0.5,.1),
                     limits = c(-.2,.5))+
  labs(
    x = "",
    y =  "Predicted Gender Gap \nF-M"
  ) +
  ggtitle("Educational Attainment")+
  cowplot::theme_cowplot()+
  theme(axis.text.x = element_text(),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.grid = element_line(color = "grey95"),
        plot.title = element_text(hjust = 0.5,face="bold"),
        legend.position = 'none')


plot_sign_education <-ggplot(df_plot_education, aes(x = education, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), 
                  position = position_dodge(width = 0.6),
                  size = 0.40) +
  scale_color_brewer(palette = 'Set2')+
  scale_y_continuous(breaks = seq(-0.2,0.5,.1),
                     labels = seq(-0.2,0.5,.1),
                     limits = c(-.2,.5))+
  labs(
    x = "",
    y =  "Predicted Gender Gap \nF-M"
  ) +
  ggtitle("Educational Attainment")+
  cowplot::theme_cowplot()+
  theme(axis.text.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5,face="bold"),
        legend.position = 'none')


##wage ####

specs_wage <- emmeans(model_com, 
                      ~ sex_cat
                      | wage_class_dec,
                      data = usa_survey_emp_gemtemp,
                      type = "response",
                      rg.limit= 60000)

cont <- contrast(specs_wage, method = "revpairwise")
summary(cont)

df_plot_sign <- as.data.frame(summary(cont, infer = TRUE))
df_plot_sign<- df_plot_sign%>%
  mutate(
    wage_class_dec = factor(wage_class_dec, levels = c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th"))
  )

specs_rep <- emmeans(model_rep, 
                     ~ sex_cat
                     | wage_class_dec,
                     data = usa_survey_emp_gemtemp,
                     type = "response",
                     rg.limit=60000)

cont_rep <- contrast(specs_rep, method = "revpairwise")
summary(cont_rep) 

df_plot_sign_rep <- as.data.frame(summary(cont_rep, infer = TRUE))
df_plot_sign_rep<- df_plot_sign_rep%>%
  mutate(
    wage_class_dec = factor(wage_class_dec, levels = c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th"))
  )

df_plot_wage<-rbind( df_plot_sign%>%
                       mutate(type= 'Complement'), df_plot_sign_rep%>%
                       mutate(type= 'Replace'))

plot_sign_wage <- ggplot(df_plot_wage, aes(x = wage_class_dec, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), 
                  position = position_dodge(width = 0.6),
                  size = 0.40) +
  scale_y_continuous(breaks = seq(-0.1,0.3,.1),
                     labels = seq(-0.1,0.3,.1),
                     limits = c(-.1,.3))+
  scale_color_brewer(palette = 'Set2')+
  ggtitle("Wage Deciles")+
  labs(
    x = "",
    y =  "Predicted Gender Gap \nF-M"
  ) +
  cowplot::theme_cowplot()+
  theme(axis.text.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5,face="bold"),
        legend.position = 'none')

ggsave(plot_sign_wage,file="plot_gender_gap_wage.pdf",height=4,width=6)




### race ####

specs_race <- emmeans(model_com, 
                      ~ sex_cat
                      | race_cat,
                      data = usa_survey_emp_gemtemp,
                      type = "response",
                      rg.limit= 60000)

cont <- contrast(specs_race, method = "revpairwise")
summary(cont)

df_plot_race <- as.data.frame(summary(cont, infer = TRUE))
df_plot_race <- df_plot_race%>%
  mutate(
    race_cat = factor(race_cat, 
                            levels=c("White",
                                     "Asian",
                                     "Black",
                                     "other"),
                            labels = c("White",
                                       "Asian",
                                       "Black",
                                       "Other"))
  )

specs_rep <- emmeans(model_rep, 
                     ~ sex_cat
                     | race_cat,
                     data = usa_survey_emp_gemtemp,
                     type = "response",
                     rg.limit=60000)

cont_rep <- contrast(specs_rep, method = "revpairwise")
summary(cont_rep) 

df_plot_race_rep <- as.data.frame(summary(cont_rep, infer = TRUE))
df_plot_race_rep<- df_plot_race_rep%>%
  mutate(
    race_cat = factor(race_cat, 
                      levels=c("White",
                               "Asian",
                               "Black",
                               "other"),
                      labels = c("White",
                                 "Asian",
                                 "Black",
                                 "Other"))
  )

df_plot_race_combine <-rbind( df_plot_race%>%
                       mutate(type= 'Complement'), 
                     df_plot_race_rep %>%
                       mutate(type= 'Replace'))

plot_sign_race <- ggplot(df_plot_race_combine, aes(x = race_cat, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), 
                  position = position_dodge(width = 0.6),
                  size = 0.40) +
  scale_color_brewer(palette = 'Set2')+
  ggtitle("Race")+
  scale_y_continuous(breaks = seq(-0.2,0.5,.1),
                     labels = seq(-0.2,0.5,.1),
                     limits = c(-.2,.5))+
  labs(
    x = "",
    y =  "Predicted Gender Gap \nF-M"
  ) +
  cowplot::theme_cowplot()+
  theme(axis.text.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5,face="bold"),
        legend.position = 'none')

ggsave(plot_sign_wage,file="plot_gender_gap_wage.pdf",height=4,width=6)



##children ####
specs <- emmeans(model_com, 
                 ~ sex_cat
                 | nchlt5_class,
                 data = usa_survey_emp_gemtemp,
                 type = "response",
                 rg.limit= 60000)

cont <- contrast(specs, method = "revpairwise")
summary(cont)

df_plot_sign <- as.data.frame(summary(cont, infer = TRUE))
df_plot_sign<- df_plot_sign%>%
  mutate(
    nchlt5_class = factor(nchlt5_class, levels = c('0','1','2 or more'))
  )

specs_rep <- emmeans(model_rep, 
                     ~ sex_cat
                     | nchlt5_class,
                     data = usa_survey_emp_gemtemp,
                     type = "response",
                     rg.limit=50000)

cont_rep <- contrast(specs_rep, method = "revpairwise")
summary(cont_rep) 

df_plot_sign_rep <- as.data.frame(summary(cont_rep, infer = TRUE))
df_plot_sign_rep<- df_plot_sign_rep%>%
  mutate(
    nchlt5_class = factor(nchlt5_class, levels = c('0','1','2 or more'))
  )

df_plot_child<-rbind( df_plot_sign%>%
                        mutate(type= 'Complement'), df_plot_sign_rep%>%
                        mutate(type= 'Replace'))

plot_sign_child<-ggplot(df_plot_child, aes(x = nchlt5_class, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), 
                  position = position_dodge(width = 0.6),
                  size = 0.40) +
  scale_color_brewer(palette = 'Set2')+
  ylim(0,0.15)+
  labs(
    x = "Number of Children",
    y =  NULL
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title=element_blank(),
        legend.position = 'none')

plot_sign_all<-((plot_sign_education)/plot_sign_wage)+
  theme(legend.position = 'bottom',
        legend.text = element_text(face="bold"),
        legend.justification = "center")
ggsave(plot_sign_all,file='predicted_gender_gap.png',height = 7, width = 12)
#plot coefficients no interactions ####

cm_final <- c(
  "age_class30" = "30-34", "age_class35" = "35-39", "age_class40" = "40-44",
  "age_class45" = "45-49", "age_class50" = "50-54", "age_class55" = "55-59", 
  "age_class60" = "60-64",
  "race_catAsian" = "Asian", "race_catBlack" = "Black", "race_catother" = "Other",
  "educationBachelor's Degree" = "Bachelor's Degree",
  "educationDoctoral Degree" = "Doctoral Degree",
  "educationHigh School Diploma" = "High School Diploma",
  "educationMaster's Degree" = "Master's Degree",
  "sex_catFemale" = "Female",
  "wage_class_dec2nd" = "2nd", "wage_class_dec3rd" = "3rd",
  "wage_class_dec4th" = "4th", "wage_class_dec5th" = "5th",
  "wage_class_dec6th" = "6th", "wage_class_dec7th" = "7th",
  "wage_class_dec8th" = "8th", "wage_class_dec9th" = "9th",
  "wage_class_dec10th" = "10th",
  "marst_catDivorced"= "Divorced",
  "marst_catNever married/single"="Never married/single",
  "marst_catSeparated"="Separated",
  "marst_catWidowed"="Widowed",
  "nchlt5_class1" = "1",
  "nchlt5_class2 or more" = "2 or more"
)

plot_data <- modelplot(list("Complement" = model_com, 
                            "Replace" = model_rep), draw = FALSE) %>%
  filter(term %in% names(cm_final)) %>%
  mutate(
    group = case_when(
      grepl("age_class", term) ~ "Age\n(Ref. 25-29)",
      grepl("race_cat", term) ~ "Race\n(Ref. White)",
      grepl("education", term) ~ "Education\n(Ref. Less than HS)",
      grepl("sex_cat", term) ~ "Sex\n(Ref. Male)",
      grepl("wage_class_dec", term) ~ "Wage\n(Ref. 1st)",
      grepl("marst_cat", term) ~ "Marital Status\n(Ref. Married)",
      grepl("nchlt5_class", term) ~ "Number of children\n(Ref. 0)"
    ),
    clean_label = cm_final[term]
  ) %>%
  mutate(
    group = factor(group, levels = c(
      "Age\n(Ref. 25-29)","Race\n(Ref. White)",  "Education\n(Ref. Less than HS)", "Sex\n(Ref. Male)",
      "Wage\n(Ref. 1st)","Marital Status\n(Ref. Married)","Number of children\n(Ref. 0)"
    )),
    clean_label = factor(clean_label, levels = rev(c(
      "30-34", "35-39", "40-44", "45-49","50-54","55-59","60-64",
      "Asian", "Black", "Other",
      "High School Diploma", "Bachelor's Degree", "Master's Degree", "Doctoral Degree",
      "Female",
      "2nd","3rd", "4th", "5th","6th","7th","8th","9th","10th",
      "Divorced","Never married/single","Separated","Widowed",
      "1" ,"2 or more"
    )))
  ) %>%
  mutate(
    group= case_when(substr(term,1,4)=='wage' ~"Wage\n(Ref. 1st)",   TRUE~ group )
  ) 

ggplot(plot_data, aes(x = estimate, y = clean_label, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, 
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = c("Complement" = "#76EEC6", "Replace" = "#FF8C00")) +
  scale_shape_manual(values = c("Complement" = 16, "Replace" = 17)) +
  labs(x = "Estimate", y = NULL, color = NULL, shape=NULL) +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1, size = 9),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 9, color = "black"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "bottom"
  )+
  xlim(-0.2,1.2)+
  scale_x_continuous(breaks= seq(-0.2,1.2, by=0.2))
ggsave(file='coefficient plot1.png',width = 8, height = 11, dpi = 300)


plot_sign_all <- (plot_sign_education / plot_sign_wage / plot_sign_race) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 20, b = 20)
  )

plot_wage = plot_sign_wage +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 20, b = 20)
  )

ggsave(plot_wage,file="plot_wage.pdf",height=8,width=15)


plot_sign_all <- (plot_sign_education/ plot_sign_wage /plot_sign_race) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 20, b = 20)
  )

ggsave(plot_sign_all,file="plot_gender_gap.pdf",height=20,width=15)


plot_education = plot_sign_education +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 20, b = 20)
  )

ggsave(plot_education,file="plot_education.pdf",height=8,width=15)

plot_race = plot_sign_race +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 20, b = 20)
  )

ggsave(plot_race,file="plot_race.pdf",height=15,width=8)