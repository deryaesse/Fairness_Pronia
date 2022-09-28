library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(tidyr)
library(writexl)
library(readxl)
library(readr)

CHR_raters <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_raters.xls")
PRONIA_Demographics <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/PRONIA_ROD_CHR_Demographics_filtered (2).xls")
CHR_S_Nikos_Pred <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_S_Nikos_Pred.xls")
CHR_R_Nikos_Pred <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_R_Nikos_Pred.xls")
transition_models <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/transition_models (1).xls")
transition_multimodal <- read_excel("C:/Users/derya/Downloads/TransitionPredictions4Joseph.xlsx")
Disc_Data_all <- read_csv("C:/Users/derya/Downloads/PQT_PRONIA_2_TK_Disc_Data_all_14-Jan-2022.csv")
Repl_Data_all <- read_csv("C:/Users/derya/Downloads/PQT_PRONIA_2_TK_Repl_Data_all_14-Jan-2022.csv")
transitions <- transitions <- read_csv("C:/Users/derya/Downloads/transitions.csv")
transition_model_validation <- read_csv("C:/Users/derya/Downloads/transition_model_validation.csv")
transition_model_validation <- spread(transition_model_validation, model, prediction)

psn_multimodal <- pull(transition_multimodal, PSN)
psn_models <- as.character(pull(transition_model_validation, PSN))

diff <- setdiff(psn_models, psn_multimodal)
psn <- c(psn_multimodal, diff)

columns <- c("PSN", "SEX", "AGE_T0", "PROGNOSTIC_01_01_Transition_T0", "PROGNOSTIC_02_01_PoorOutcome_T0", "DEMOG_T0T1T2_31AA_EducationYears_T0")

Disc_psn <- Disc_Data_all %>% filter(PSN %in% psn) %>% select(columns)
Rep_psn <- Repl_Data_all %>% filter(PSN %in% psn) %>% select(columns)
data_all <- full_join(Disc_psn, Rep_psn, by=columns)

PRONIA_Demographics <- PRONIA_Demographics %>%
  mutate_at(vars(AGE_T0, DEMOG_T0T1T2_31AA_EducationYears_T0, 
                 DEMOG_T0T1T2_19_PupulationPoLiving_T0, DEMOG_T0T1T2_20A_DensityPoLiving_T0,
                 DEMOG_T0T1T2_20B_DensityPoBirth_T0), as.numeric) %>%
  mutate_at(vars(Studygroup, SEX, DEMOG_T0_02_Ethnicity_T0,
                 DEMOG_T0T1T2_36_OccupationCurrent_T0, DEMOG_T0T1T2_28A_MaritalCurrent_T0,
                 DEMOG_T0T1T2_29A_LivingCurrent_T0), as.factor) %>%
  select(-(...17))


#names(CHR_raters)
#pronia <- CHR_raters %>% select("PSN", "PROGNOSTIC_01_01_Transition_T0", "PROGNOSTIC_02_01_PoorOutcome_T0")
#pronia <- left_join(data_all, pronia, by="PSN")

#italy <- c("ITALIA", "Italia", "italy", "Italy", "Itlay", "Milan", "Milano", "udine", "Udine")
#germany <- c("germany", "Germany", "Germnay", "Deutschland", "Munich")
#uk <- c("England", "UK", "United Kingdom")
#finland <- c("Finland", "Suomi")

colnames(data_all)[6] <- "Education"
#colnames(pronia)[9] <- "CurrentCountry"
#colnames(pronia)[16] <- "Ethnicity"

#pronia <- pronia %>% mutate(
 # CurrentCountry = if_else(CurrentCountry %in% italy, "IT", CurrentCountry),
  #CurrentCountry = if_else(CurrentCountry %in% germany, "DE", CurrentCountry),
  #CurrentCountry = if_else(CurrentCountry %in% uk, "GB", CurrentCountry),
  #CurrentCountry = if_else(CurrentCountry %in% finland, "FI", CurrentCountry),
  #CurrentCountry = if_else(CurrentCountry=="Switzerland", "CH", CurrentCountry)
#)

#pronia$Ethnicity <- as.character(pronia$Ethnicity)
#pronia <- pronia %>% mutate(
 # Ethnicity = if_else(Ethnicity=="German", "DE", Ethnicity),
#  Ethnicity = if_else(Ethnicity=="Swiss", "CH", Ethnicity),
 # Ethnicity = if_else(Ethnicity=="Finnish", "FI", Ethnicity),
#  Ethnicity = if_else(Ethnicity=="Turkish", "TR", Ethnicity),
 # Ethnicity = if_else(Ethnicity=="Italian", "IT", Ethnicity),
#  Ethnicity = if_else(Ethnicity=="British", "UK", Ethnicity),
#)

data_all <- data_all %>% mutate(
  AGE_T0= as.numeric(AGE_T0),
  AGE_T0 = ifelse(AGE_T0>100, AGE_T0/1000, AGE_T0),
  edstat = as.factor(ifelse(Education > median(.$Education, na.rm=TRUE), "high", "low")),
  #urban = as.factor(ifelse(as.numeric(DEMOG_T0T1T2_20A_DensityPoLiving_T0)>=1500, "urban", "non-urban"))
  )

data_all <- data_all %>% mutate(
  PROGNOSTIC_01_01_Transition_T0 = as.factor(PROGNOSTIC_01_01_Transition_T0),
  PROGNOSTIC_02_01_PoorOutcome_T0 = as.factor(PROGNOSTIC_02_01_PoorOutcome_T0),
  #Studygroup = as.factor(Studygroup),
  #PoB_Country = as.factor(PoB_Country),
  SEX = ifelse(SEX=="1", "male", "female"),
  SEX = as.factor(SEX),
  Education = as.numeric(Education),
  #Ethnicity = as.factor(Ethnicity),
  #CurrentCountry = as.factor(CurrentCountry),
  #DEMOG_T0T1T2_28A_MaritalCurrent_T0 = as.factor(DEMOG_T0T1T2_28A_MaritalCurrent_T0)
)

#####################################################
#### ADDING TOGETHER TRANSITION AND DEMOGRAPHIC DATA
#####################################################

#pronia_chr <- pronia %>%  filter(Studygroup=="CHR")
#transition_multimodal$PSN <- as.numeric(transition_multimodal$PSN)
data_all$PSN <- as.character(data_all$PSN)
pronia_transition <- left_join(data_all, transition_multimodal, by="PSN")


trans_1 <- transition_multimodal %>% select(LABEL_BINARY...2, LABEL_BINARY...3, PSN)
trans_2 <- transition_model_validation %>% select(PSN, transition)

trans_2$PSN <- as.character(trans_2$PSN)
pronia_transition <- left_join(pronia_transition, trans_2, by="PSN")

sum(is.na(pronia_transition$LABEL_BINARY_CODE))

pronia_transition <- pronia_transition %>%
  mutate(LABEL_BINARY_CODE = if_else(is.na(LABEL_BINARY_CODE), transition, LABEL_BINARY_CODE)) %>%
  mutate(LABEL_BINARY_CODE = if_else(LABEL_BINARY_CODE==0, -1, LABEL_BINARY_CODE))


sum(is.na(pronia_transition$LABEL_BINARY_CODE))


sensitive_attributes <- c("SEX", "edstat"#, "urban"
                          )

####################################################
### Calculating the Accuracy of Clinician Ratings
####################################################

pronia_clirat <- pronia_transition %>% filter(!is.na(LABEL_BINARY_CODE)) %>%
  filter(!is.na(PROGNOSTIC_01_01_Transition_T0))

res_clinician <- pronia_clirat %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0))# %>%
#summarize_at(
 #   vars(TP, TN, FP, FN), funs(sum))


######################################################################################################
### Fairness Selected Models for Transition (Carrion, Salokangas, Cornblatt, Haidl, Clinician Rating)
######################################################################################################

transition_model_validation$PSN <- as.character(transition_model_validation$PSN)
pronia_transition_models <- full_join((pronia_transition %>% select(-"transition")), transition_model_validation, by="PSN")

models <- c("transition", "carrion", "cornblatt", "lencz", "walder", "hengartner", "michel",
            "malda", "metzler")

res_lencz <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & lencz==1, 1,0),
    TN=ifelse(transition==0 & lencz==0, 1,0),
    FP=ifelse(transition==0 & lencz==1, 1,0),
    FN=ifelse(transition==1 & lencz==0, 1,0))# %>%
 # group_by(SEX) %>%
  #summarize_at(
   # vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_hengartner <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & hengartner==1, 1,0),
    TN=ifelse(transition==0 & hengartner==0, 1,0),
    FP=ifelse(transition==0 & hengartner==1, 1,0),
    FN=ifelse(transition==1 & hengartner==0, 1,0)) #%>%
# group_by(edstat) %>%
#summarize_at(
 #vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_michel <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & michel==1, 1,0),
    TN=ifelse(transition==0 & michel==0, 1,0),
    FP=ifelse(transition==0 & michel==1, 1,0),
    FN=ifelse(transition==1 & michel==0, 1,0)) #%>%
 #group_by(edstat) %>%
#summarize_at(
 #vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_malda <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & malda==1, 1,0),
    TN=ifelse(transition==0 & malda==0, 1,0),
    FP=ifelse(transition==0 & malda==1, 1,0),
    FN=ifelse(transition==1 & malda==0, 1,0)) #%>%
#  group_by(edstat) %>%
#summarize_at(
 #vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_metzler <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & metzler==1, 1,0),
    TN=ifelse(transition==0 & metzler==0, 1,0),
    FP=ifelse(transition==0 & metzler==1, 1,0),
    FN=ifelse(transition==1 & metzler==0, 1,0)) #%>%
 # group_by(edstat) %>%
#summarize_at(
 #vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_carrion <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & carrion==1, 1,0),
    TN=ifelse(transition==0 & carrion==0, 1,0),
    FP=ifelse(transition==0 & carrion==1, 1,0),
    FN=ifelse(transition==1 & carrion==0, 1,0)) #%>%
#group_by(SEX) %>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(., na.rm=T)))

res_walder <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & walder==1, 1,0),
    TN=ifelse(transition==0 & walder==0, 1,0),
    FP=ifelse(transition==0 & walder==1, 1,0),
    FN=ifelse(transition==1 & walder==0, 1,0))# %>%
#group_by(SEX) %>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(.,na.rm=T)))

res_cornblatt <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & cornblatt==1, 1,0),
    TN=ifelse(transition==0 & cornblatt==0, 1,0),
    FP=ifelse(transition==0 & cornblatt==1, 1,0),
    FN=ifelse(transition==1 & cornblatt==0, 1,0))# %>%
#group_by(SEX) %>%
# summarize_at(
 #  vars(TP, TN, FP, FN), funs(sum(., na.rm=T)))



res_crrat1 <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0))#%>%
#group_by(SEX) %>%
 #summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(., na.rm=T)))

sex_t <- rbind(res_hengartner %>% mutate(Model='Model by Hengartner'),
             res_lencz %>% mutate(Model="Model by Lencz"),
             res_malda %>% mutate(Model='Model by Malda'),
             res_metzler %>% mutate(Model="Model by Metzler"),
             res_michel %>% mutate(Model="Model by Michel"),
             res_walder %>% mutate(Model="Model by Walder"),
             res_crrat1 %>% mutate(Model="Clinician Rating")
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  filter(SEX!="0") %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    #"Treatment Equality" = (FN/(FN+TP))/(FP/(FP+TN))#,
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, SEX)) %>%
  pivot_wider(names_from = SEX, values_from=value) %>%
  mutate(ratio = `female`/`male`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_bar(stat='identity', position=position_dodge()) + 
  #coord_flip()+
  theme_classic() + 
  coord_flip()+
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() + 
  scale_y_continuous(breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
                     )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch ="both") +
  theme(axis.title.y=element_blank(),
        axis.title.x = element_text(hjust=0.24, size=8),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  #labs(y = "Biased Towards Males                       Biased Towards Females")+
  ylab(expression("Bias Towards Males   " %<->% "  Bias Towards Females"))
#+
  #ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
   #subtitle = "(Protected Group: Male)")


sex_t_table <- rbind(res_hengartner %>% mutate(Model='Model by Hengartner'),
       res_lencz %>% mutate(Model="Model by Lencz"),
       res_malda %>% mutate(Model='Model by Malda'),
       res_metzler %>% mutate(Model="Model by Metzler"),
       res_michel %>% mutate(Model="Model by Michel"),
       res_walder %>% mutate(Model="Model by Walder"),
       res_crrat1 %>% mutate(Model="Clinician Rating")
) %>%
  group_by(Model, 
           SEX
           ) %>%
  #filter(!is.na(SEX)) %>%
  #filter(SEX!="0") %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC = (Sensitivity+Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))
write.csv(sex_t_table)
write_xlsx(sex_t_table, "sex_table_1.xlsx")

           

           
education_t <- rbind(res_hengartner %>% mutate(Model='Model by Hengartner'),
                     res_lencz %>% mutate(Model="Model by Lencz"),
                     res_malda %>% mutate(Model='Model by Malda'),
                     res_metzler %>% mutate(Model="Model by Metzler"),
                     res_michel %>% mutate(Model="Model by Michel"),
                     res_walder %>% mutate(Model="Model by Walder"),
                     res_crrat1 %>% mutate(Model="Clinician Rating")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, edstat)) %>%
  pivot_wider(names_from = edstat, values_from=value) %>%
  mutate(ratio = `low`/`high`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_classic() +
  coord_flip()+
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() + 
  scale_y_continuous(limits = c(-0.4,1.6),
                     breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
  )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch = "both") +
  theme(axis.title.y=element_blank(),
        axis.title.x = element_text(hjust=-0.1, size=8),
        axis.text.y=element_blank(),
        axis.ticks = element_blank()) +
  ylab(expression("Bias Towards Higher Education   " %<->% "  Bias Towards Lower Education"))# +
 # ggtitle(label = 'Fairness criteria for sensitive attribute: Education', 
  # subtitle = "(Protected Group: High)")

ed_t_table <- rbind(res_hengartner %>% mutate(Model='Model by Hengartner'),
                    res_lencz %>% mutate(Model="Model by Lencz"),
                    res_malda %>% mutate(Model='Model by Malda'),
                    res_metzler %>% mutate(Model="Model by Metzler"),
                    res_michel %>% mutate(Model="Model by Michel"),
                    res_walder %>% mutate(Model="Model by Walder"),
                    res_crrat1 %>% mutate(Model="Clinician Rating")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC = (Sensitivity+Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))
write.table(ed_t_table, file = "ed_transition_1")
write_xlsx(ed_t_table, "ed_table_1.xlsx")

### Fairness Functioning

func_psn <- pull(CHR_R_Nikos_Pred, PSN)
Disc_func <- Disc_Data_all %>% filter(PSN %in% func_psn) %>% select(columns)
Rep_func <- Repl_Data_all %>% filter(PSN %in% func_psn) %>% select(columns)
data_func <- full_join(Disc_func, Rep_func, by=columns)
colnames(data_func)[6] <- "Education"

data_func <- data_func %>% mutate(
  AGE_T0= as.numeric(AGE_T0),
  AGE_T0 = ifelse(AGE_T0>100, AGE_T0/1000, AGE_T0),
  edstat = as.factor(ifelse(Education > median(.$Education, na.rm=TRUE), "high", "low")),
  #urban = as.factor(ifelse(as.numeric(DEMOG_T0T1T2_20A_DensityPoLiving_T0)>=1500, "urban", "non-urban"))
)

data_func <- data_func %>% mutate(
  PROGNOSTIC_01_01_Transition_T0 = as.factor(PROGNOSTIC_01_01_Transition_T0),
  PROGNOSTIC_02_01_PoorOutcome_T0 = as.factor(PROGNOSTIC_02_01_PoorOutcome_T0),
  #Studygroup = as.factor(Studygroup),
  #PoB_Country = as.factor(PoB_Country),
  SEX = ifelse(SEX=="1", "male", "female"),
  SEX = as.factor(SEX),
  Education = as.numeric(Education),
  #Ethnicity = as.factor(Ethnicity),
  #CurrentCountry = as.factor(CurrentCountry),
  #DEMOG_T0T1T2_28A_MaritalCurrent_T0 = as.factor(DEMOG_T0T1T2_28A_MaritalCurrent_T0)
)


CHR_R_Nikos_Pred$PSN <- as.character(CHR_R_Nikos_Pred$PSN)
data_func$PSN <- as.character(data_func$PSN)
fairness_functioning_r <- inner_join(CHR_R_Nikos_Pred, data_func, by="PSN")
CHR_S_Nikos_Pred$PSN <- as.character(CHR_S_Nikos_Pred$PSN)
fairness_functioning_s <- inner_join(CHR_S_Nikos_Pred, data_func, by="PSN")

functioning <- inner_join(CHR_R_Nikos_Pred, pronia, by ="PSN")
functioning <- inner_join(CHR_S_Nikos_Pred, functioning, by ="PSN")

res_funcr <- fairness_functioning_r %>% 
  select("PSN", sensitive_attributes, GF_R_Pred, GF_R_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_R_True==1 & GF_R_Pred==1, 1,0),
    TN=ifelse(GF_R_True==-1 & GF_R_Pred==-1, 1,0),
    FP=ifelse(GF_R_True==-1 & GF_R_Pred==1, 1,0),
    FN=ifelse(GF_R_True==1 & GF_R_Pred==-1, 1,0)) %>%
  select(-c("GF_R_Pred", "GF_R_True"))# %>%
#group_by(SEX) %>% 
#  group_by(edstat) %>%
#summarize_at(
 # vars(TP, TN, FP, FN), funs(sum(., na.rm=T)) )

res_funcs <- fairness_functioning_s %>% 
  select("PSN", sensitive_attributes, GF_S_Pred, GF_S_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_S_True==1 & GF_S_Pred==1, 1,0),
    TN=ifelse(GF_S_True==-1 & GF_S_Pred==-1, 1,0),
    FP=ifelse(GF_S_True==-1 & GF_S_Pred==1, 1,0),
    FN=ifelse(GF_S_True==1 & GF_S_Pred==-1, 1,0)) %>%
  select(-c("GF_S_Pred", "GF_S_True")) #%>%
  #group_by(SEX) %>%
  #summarize_at(
   # vars(TP, TN, FP, FN), funs(sum(.,na.rm = T)) )

res_f_clinician_r <- fairness_functioning_r %>% 
  select("PSN", sensitive_attributes, GF_R_Pred, GF_R_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_R_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    TN=ifelse(GF_R_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0),
    FP=ifelse(GF_R_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    FN=ifelse(GF_R_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0)) %>%
  select(-c("GF_R_Pred", "GF_R_True"))# %>%
  #group_by(SEX) %>%
  #group_by(edstat) %>%
  #summarize_at(
   # vars(TP, TN, FP, FN), funs(sum(.,na.rm = T)))

res_f_clinician_s <- fairness_functioning_s %>% 
  select("PSN", sensitive_attributes, GF_S_Pred, GF_S_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_S_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    TN=ifelse(GF_S_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0),
    FP=ifelse(GF_S_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    FN=ifelse(GF_S_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0)) %>%
  select(-c("GF_S_Pred", "GF_S_True"))# %>%
  #group_by(SEX) %>%
  #group_by(edstat) %>%
 #summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(.,na.rm=T)))

sex_f_r <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML'),
             res_f_clinician_r %>% mutate(Model='Role Functioning - Clinician'),
             res_funcs %>% mutate(Model='Social Functioning - ML'),
             res_f_clinician_s %>% mutate(Model='Social Functioning - Clinician')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, SEX)) %>%
  pivot_wider(names_from = SEX, values_from=value) %>%
  mutate(ratio = `female`/`male`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_classic() + 
  coord_flip()+
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() + 
  scale_y_continuous(breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
                     )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch="both") +
  theme(axis.title.y=element_blank(),
        axis.title.x = element_text(hjust=0.01, size=8),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(expression("Bias Towards Males   " %<->% "  Bias Towards Females"))


sex_f_s <- rbind(res_funcs %>% mutate(Model='Social Functioning'),
                 res_f_clinician_s %>% mutate(Model='Clinician Prediction')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, SEX)) %>%
  pivot_wider(names_from = SEX, values_from=value) %>%
  mutate(ratio = `male`/`female`) %>%
  ggplot(aes(y=ratio, x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_minimal() + 
  geom_hline(yintercept = 0.8, lty=2) + 
  geom_hline(yintercept = 1.25, lty=2) + 
  scale_fill_grey() + 
  facet_grid(.~name, labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch="both") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
          subtitle = "(Protected Group: Male)")



## sex functioning table 

sex_f_table_r <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
                       res_f_clinician_r %>% mutate(Model='Clinician Prediction')) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100) %>%
  select(-c(TP, FP, TN, FN))
write.table(sex_f_table_r, file="sex_func_r_1")

sex_f_table_s <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML'),
                       res_f_clinician_r %>% mutate(Model='Role Functioning - Clinician'),
                       res_funcs %>% mutate(Model='Social Functioning - ML'),
                       res_f_clinician_s %>% mutate(Model='Social Functioning - Clinician')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC = (Sensitivity + Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))
write_xlsx(sex_f_table_s, "sex_func_s_1.xlsx")


######
# education fairness functional outcome
######

education_f_r <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML'),
                       res_f_clinician_r %>% mutate(Model='Role Functioning - Clinician'),
                       res_funcs %>% mutate(Model='Social Functioning - ML'),
                       res_f_clinician_s %>% mutate(Model='Social Functioning - Clinician')
)  %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, edstat)) %>%
  pivot_wider(names_from = edstat, values_from=value) %>%
  mutate(ratio = `low`/`high`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_classic() + 
  coord_flip() +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() + 
  scale_y_continuous(breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
                     )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch="both") +
  theme(axis.title.y=element_blank(),
        axis.title.x = element_text(hjust=0.7, size=8),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(expression("Bias Towards High Education   " %<->% "  Bias Towards Low Education"))


education_f_s <- rbind(res_funcs %>% mutate(Model='Social Functioning - ML Model'),
                       res_f_clinician_s %>% mutate(Model='Clinician Prediction')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
   # "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, edstat)) %>%
  pivot_wider(names_from = edstat, values_from=value) %>%
  mutate(ratio = `high`/`low`) %>%
  ggplot(aes(y=ratio, x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_minimal() + 
  geom_hline(yintercept = 0.8, lty=2) + 
  geom_hline(yintercept = 1.25, lty=2) + 
  scale_fill_grey() + 
  facet_grid(.~name, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Education', 
          subtitle = "(Protected Group: High)")

##education_f_r table

ed_f_r_t <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML'),
                  res_f_clinician_r %>% mutate(Model='Role Functioning - Clinician'),
                  res_funcs %>% mutate(Model='Social Functioning - ML'),
                  res_f_clinician_s %>% mutate(Model='Social Functioning - Clinician')
) %>%
  group_by(Model, edstat
           ) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC = (Sensitivity+Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))
write_xlsx(ed_f_r_t, "rolefunc_ed.xlsx")

##education_f_s table

ed_f_s_t <- rbind(res_funcs %>% mutate(Model='Social Functioning'),
      res_f_clinician_s %>% mutate(Model='Clinician Rating')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=T)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100) %>%
  select(-c(TP, FP, TN, FN))
write.table(ed_f_s_t, file="soc_func_ed")

### urban environment

urban_f_r <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
                 res_f_clinician_r %>% mutate(Model='Clinician Prediction')
) %>%
  group_by(Model, urban) %>%
  filter(!is.na(urban)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odss" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, urban)) %>%
  pivot_wider(names_from = urban, values_from=value) %>%
  mutate(ratio = `urban`/`non-urban`) %>%
  ggplot(aes(y=ratio, x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_minimal() + 
  geom_hline(yintercept = 0.8, lty=2) + 
  geom_hline(yintercept = 1.25, lty=2) + 
  scale_fill_grey() + 
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Urban Environment', 
          subtitle = "(Protected Group: Non-Urban)")




############################
#### multimodal transition
############################


#transition <- transition_models %>% select(c("PSN", "transition"))
#transition <- left_join(transition, pronia, by="PSN")
#transition_multimodal$PSN <- as.double(transition_multimodal$PSN)
#all_transition <- left_join(transition, transition_multimodal, by="PSN")
#all_transition <- all_transition %>% filter(!is.na(Studygroup))


res_mri <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_MRI) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_MRI==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_MRI==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_MRI==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_MRI==-1, 1,0)) %>%
  select(-"PRED_LABEL_MRI")# %>%
 #summarize_at(
  #vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_prs <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_PRS) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_PRS==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_PRS==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_PRS==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_PRS==-1, 1,0)) %>%
  select(-"PRED_LABEL_PRS")# %>%
 #summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_clin <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_Clin) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Clin==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Clin==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Clin==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Clin==-1, 1,0)) %>%
  select(-"PRED_LABEL_Clin")# %>%
 # summarize_at( vars(TP, TN, FP, FN), funs(sum(.,na.rm=TRUE)))

res_stk <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_Stk) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Stk==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Stk==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Stk==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Stk==-1, 1,0)) %>%
  select(-"PRED_LABEL_Stk")# %>%
# summarize_at(
 #   vars(TP, TN, FP, FN), funs(sum(.,na.rm = TRUE)))


res_crrat <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0,1,0)) %>%
  select(-"PROGNOSTIC_01_01_Transition_T0")# %>%
#summarize_at(
 # vars(TP, TN, FP, FN), funs(sum(.,na.rm = TRUE)))

sex_multimodal <- rbind(res_mri %>% mutate(Model='Model based on MRI data'),
               res_clin %>% mutate(Model='Model based on clinical data'),
               res_prs %>% mutate(Model='Model based on PRS'),
               res_stk %>% mutate(Model='Stacked Model'),
               res_crrat %>% mutate(Model='Clinician Rating')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=TRUE))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
    #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, SEX)) %>%
  pivot_wider(names_from = SEX, values_from=value) %>%
  mutate(ratio = `female`/`male`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_classic() +
  coord_flip()+
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() + 
  scale_y_continuous(breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
                     )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch="both") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust=0.61, size=8))+
  ylab(expression("Bias Towards Males   " %<->% "  Bias Towards Females"))# +
  #ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
   #       subtitle = "(Protected Group: Male)")


## sex 

sex_multimodal_table <- rbind(res_mri %>% mutate(Model='MRI'),
                              res_clin %>% mutate(Model='Clinical'),
                              res_prs %>% mutate(Model='Polygenetic Risk Score'),
                              res_stk %>% mutate(Model='Stacked'),
                              res_crrat %>% mutate(Model='Clinician Rating')
)%>%
  group_by(Model
           , SEX
           ) %>%
  filter(!is.na(SEX)) %>%
  filter(SEX!="0")%>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(sum(.,na.rm=TRUE)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC = (Sensitivity+Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))

write.table(sex_multimodal_table, file="multimodal_table_sex")

education__multimodal <- rbind(res_mri %>% mutate(Model='Model based on MRI data'),
                               res_clin %>% mutate(Model='Model based on clinical data'),
                               res_prs %>% mutate(Model='Model based on PRS'),
                               res_stk %>% mutate(Model='Stacked Model'),
                               res_crrat %>% mutate(Model='Clinician Rating')
)%>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=TRUE))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP)#,
    #"Treatment Equality" = (FN/FP),
  ) %>%
  select(-c(TP, FP, TN, FN)) %>%
  pivot_longer(-c(Model, edstat)) %>%
  pivot_wider(names_from = edstat, values_from=value) %>%
  mutate(ratio = `low`/`high`) %>%
  ggplot(aes(y=(ratio-1), x=Model, fill=Model)) + geom_col(stat='identity', position=position_dodge()) + 
  theme_classic() + 
  coord_flip()+
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = -0.2, lty=2) + 
  geom_hline(yintercept = 0, size=0.5, ) +
  geom_hline(yintercept = 0.25, lty=2) + 
  scale_fill_grey() +
  scale_y_continuous(limits = c(-0.33, 1.2),
                     breaks = c(-0.2, 0, 0.25), 
                     labels = c('0.8', '1', '1.25'),
                     #name = "Fairness Index"
                     )+
  facet_grid(name~., labeller=label_wrap_gen(width = 10, multi_line = TRUE), switch = "both") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_text(hjust=0, size=8),
        axis.ticks.y = element_blank())+
  ylab(expression("Bias for Higher Education   " %<->% "  Bias for Lower Education"))


##education_f_ table

ed_multimodal_table <- rbind(res_mri %>% mutate(Model='MRI'),
      res_clin %>% mutate(Model='Clinical'),
      res_prs %>% mutate(Model='Polygenetic Risk Score'),
      res_stk %>% mutate(Model='Stacked'),
      res_crrat %>% mutate(Model='Clinician Rating')
)%>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(sum(.,na.rm=TRUE)))%>%
  mutate(Accuracy = (TP+TN/(FP+TN+FN+TP))*100,
         Sensitivity = (TP/(TP+FN))*100,
         Specificity = (TN/(TN+FP))*100,
         PPV = (TP/(TP+FP))*100,
         NPV = (TN/(TN+FN))*100,
         BAC= (Sensitivity+Specificity)/2) %>%
  select(-c(TP, FP, TN, FN))

write_xlsx(ed_multimodal_table, "ed_multimodal_table.xlsx")




##################
##### Permutation Tests
##################

get_ratio <- function(x, attribute, priv){
  
  if(class(x)=='permutation'){x <- x %>% as.data.frame()}
  
  # define non_priv attribute
  non_priv <- na.omit(unique(x[,attribute]))[which(na.omit(unique(x[,attribute]))!=priv)]
  
  attribute <- sym(as.character(attribute))
  priv <- sym(as.character(priv))
  non_priv <- sym(as.character(non_priv))
  
  my_ratio <- x %>%
    group_by(!!attribute) %>%
    filter(!is.na(attribute)) %>%
    #first build the confusion matrix for subgroups
    summarize_at(
      vars(TP, TN, FP, FN), funs(mean(., na.rm=TRUE))
    )%>%
    #then calculate the fairness metrics
    mutate(
      "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
      "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
      "Predictive Equality" = FP/(FP+TN),
      "Predictive Parity" = TP/(TP+FP),
     # "Treatment Equality" = (FN/FP),
      #"MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
    ) %>%
    select(-c(TP, FP, TN, FN)) %>%
  filter(!is.na(!!attribute)) %>% 
    pivot_longer(-!!attribute) %>% 
    pivot_wider(names_from=!!attribute, values_from=value) %>%
    mutate(ratio = (!!non_priv / !!priv)) %>%
    select(-!!priv, -!!non_priv) %>% 
    pivot_wider(names_from=name, values_from=ratio)
  return(my_ratio)
}



test_fairness <- function(x, n_perms, attribute, priv){
  
  real_ratio <- get_ratio(x, attribute, priv) %>% 
    pivot_longer(everything(), names_to='criteria', values_to='real')
  
  perms <- modelr::permute(x, n_perms, attribute)
  
  res_ratio <- map_df(perms$perm, ~get_ratio(x=., attribute, priv))
  
  res_ratio <- res_ratio %>% pivot_longer(everything(), names_to='criteria', values_to='perm') %>% 
    full_join(real_ratio) %>%
    mutate(comp = abs(1 - (abs(perm))) >= abs(1-(abs(real)))) %>% 
    group_by(criteria) %>% 
    dplyr::summarize(
      p = sum(comp, na.rm=TRUE), 
      value=mean(real, na.rm=TRUE)) %>%
    mutate(
      p = format(p/n_perms, digits=3, format="f"),
      attribute = attribute,
      priv = priv
    )
  
  return(res_ratio)
}

n_perms <- 10000

res_validation <- rbind(res_hengartner %>% mutate(Model='Model by Hengartner'),
                        res_lencz %>% mutate(Model="Model by Lencz"),
                        res_malda %>% mutate(Model='Model by Malda'),
                        res_metzler %>% mutate(Model="Model by Metzler"),
                        res_michel %>% mutate(Model="Model by Michel"),
                        res_walder %>% mutate(Model="Model by Walder"),
                        res_crrat1 %>% mutate(Model="Clinician Rating"))

res_multimodal <- rbind(res_mri %>% mutate(Model='Model based on MRI data'),
      res_clin %>% mutate(Model='Model based on clinical data'),
      res_prs %>% mutate(Model='Model based on PRS'),
      res_stk %>% mutate(Model='Stacked Model'),
      res_crrat %>% mutate(Model='Clinician Rating'))

res_func <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML'),
                  res_f_clinician_r %>% mutate(Model='Role Functioning - Clinician'),
                  res_funcs %>% mutate(Model='Social Functioning - ML'),
                  res_f_clinician_s %>% mutate(Model='Social Functioning - Clinician'))

class(res_crrat1) <- "data.frame"
class(res_hengartner) <- "data.frame"
class(res_lencz) <- "data.frame"
class(res_walder) <- "data.frame"
class(res_michel) <- "data.frame"
class(res_malda) <- "data.frame"
class(res_metzler) <- "data.frame"

f_clinician_edu <- test_fairness(res_crrat1, n_perms, 'edstat', 'high')
f_clinician_sex <- test_fairness(res_crrat1, n_perms, 'SEX', 'male')

f_lencz_edu <- test_fairness(res_lencz, n_perms, 'edstat', 'high') 
f_malda_edu <- test_fairness(res_malda, n_perms, 'edstat', 'high') 
f_michel_edu <- test_fairness(res_michel, n_perms, 'edstat', 'high')
f_metzler_edu <- test_fairness(res_metzler, n_perms, 'edstat', 'high')
f_walder_edu <- test_fairness(res_walder, n_perms, 'edstat', 'high')
f_hengartner_edu <- test_fairness(res_hengartner, n_perms, 'edstat', 'high')

f_lencz_sex <- test_fairness(res_lencz, n_perms, 'SEX', 'male') 
f_malda_sex <- test_fairness(res_malda, n_perms, 'SEX', 'male') 
f_michel_sex <- test_fairness(res_michel, n_perms, 'SEX', 'male')
f_walder_sex <- test_fairness(res_walder, n_perms, 'SEX', 'male')
f_metzler_sex <- test_fairness(res_metzler, n_perms, 'SEX', 'male')
f_hengartner_sex <- test_fairness(res_hengartner, n_perms, 'SEX', 'male')



class(res_clin) <- "data.frame"
class(res_mri) <- "data.frame"
class(res_stk) <- "data.frame"
class(res_prs) <- "data.frame"

f_clinical_edu <- test_fairness(res_clin, n_perms, 'edstat', 'high')
f_mri_edu <- test_fairness(res_mri, n_perms, 'edstat', 'high')
f_prs_edu <- test_fairness(res_prs, n_perms, 'edstat', 'high')
f_stacked_edu <- test_fairness(res_stk, n_perms, 'edstat', 'high')
f_clinical_sex <- test_fairness(res_clin, n_perms, 'SEX', 'male')
f_mri_sex <- test_fairness(res_mri, n_perms, 'SEX', 'male')
f_prs_sex <- test_fairness(res_prs, n_perms, 'SEX', 'male')
f_stacked_sex <- test_fairness(res_stk, n_perms, 'SEX', 'male')

class(res_funcr) <- "data.frame"
class(res_funcs) <- "data.frame"
class(res_f_clinician_r) <- "data.frame"
class(res_f_clinician_s) <- "data.frame"

f_func_r_sex <- test_fairness(res_funcr, n_perms, 'SEX', 'male')
f_func_cli_r_sex <- test_fairness(res_f_clinician_r, n_perms, 'SEX', 'male')
f_func_s_sex <- test_fairness(res_funcs, n_perms, 'SEX', 'male')
f_func_cli_s_sex <- test_fairness(res_f_clinician_s, n_perms, 'SEX', 'male')
f_func_r_edu <- test_fairness(res_funcr, n_perms, 'edstat', 'high')
f_func_cli_r_edu <- test_fairness(res_f_clinician_r, n_perms, 'edstat', 'high')
f_func_s_edu <- test_fairness(res_funcs, n_perms, 'edstat', 'high')
f_func_cli_s_edu <- test_fairness(res_f_clinician_s, n_perms, 'edstat', 'high')



result_multimodal_sex <- rbind(
  f_clinical_sex %>% mutate(comp='Clinical'),
  f_mri_sex %>% mutate(comp='MRI'),
  f_prs_sex %>% mutate(comp='PRS'),
  f_stacked_sex %>% mutate(comp='Stacked'))

result_multimodal_edu <- rbind(
  f_clinical_edu %>% mutate(comp='Clinical'),
  f_mri_edu %>% mutate(comp='MRI'),
  f_prs_edu %>% mutate(comp='PRS'),
  f_stacked_edu %>% mutate(comp='Stacked'))


result_validation_sex <- rbind(
  f_hengartner_sex %>% mutate(comp='Hengartner'),
  f_malda_sex %>% mutate(comp='Malda'),
  f_michel_sex %>% mutate(comp='Michel'),
  f_metzler_sex %>% mutate(comp='Metzler'),
  f_walder_sex %>% mutate(comp='Walder'),
  f_lencz_sex %>% mutate(comp='Lencz')
  )

result_validation_edu <- rbind(
  f_hengartner_edu %>% mutate(comp='Hengartner'),
  f_malda_edu %>% mutate(comp='Malda'),
  f_michel_edu %>% mutate(comp='Michel'),
  f_metzler_edu %>% mutate(comp='Metzler'),
  f_walder_edu %>% mutate(comp='Walder'),
  f_lencz_edu %>% mutate(comp='Lencz')
)

result_functioning_sex <- rbind(
  f_func_cli_r_sex %>% mutate(comp='Clinician Role'),
  f_func_cli_s_sex %>% mutate(comp='Clinician Social'),
  f_func_r_sex %>% mutate(comp='Role ML'),
  f_func_s_sex %>% mutate(comp='Social ML')
)

result_functioning_edu <- rbind(
  f_func_cli_r_edu %>% mutate(comp='Clinician Role'),
  f_func_cli_s_edu %>% mutate(comp='Clinician Social'),
  f_func_r_edu %>% mutate(comp='Role ML'),
  f_func_s_edu %>% mutate(comp='Social ML')
)


result_atn <- result_atn %>% select(-c("attribute", "priv", "value")) %>% 
  pivot_wider(names_from = comp, values_from = p)
result_atn$Model <- rep("Biomarker", times=6)

result_atnd <- result_atnd %>% select(-c("attribute", "priv", "value")) %>% 
  pivot_wider(names_from = comp, values_from = p)
result_atnd$Model <- rep("Biomarker&Demographics", times=6)

result_p_all <- rbind(result_atn, result_atnd)
colnames(result_p_all) <- c("Metric", 'Gender', 'Education', 'Race', 'Ethnicity', 'Model')
result_p_all <- result_p_all %>% unite(metric, Model, Metric, remove = TRUE)
fairness <- full_join(fairmetrics, result_p_all, by="metric", suffix=c(".ratio", ".p_value"))


colorder2 <- c("metric", "Race.ratio", "Race.p_value",
               "Gender.ratio", "Gender.p_value",
               "Education.ratio", "Education.p_value", 
               "Ethnicity.ratio", "Ethnicity.p_value")


fairness <- fairness[, colorder2] 

fair_atn <- fairness[1:6,]
fair_atnd <- fairness[7:12,]


fair_atn %>% flextable()
fair_atnd %>% flextable()


write_xlsx(fair_atn,"C:/Users/derya/Desktop/Fairness/atnrf.xlsx")
write_xlsx(fair_atnd,"C:/Users/derya/Desktop/Fairness/atndrf.xlsx")
write_xlsx(fair_custom,"C:/Users/derya/Desktop/Fairness/customrf.xlsx")

