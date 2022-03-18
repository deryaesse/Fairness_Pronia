library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(tidyr)
library(writexl)
library(readxl)

CHR_raters <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_raters.xls")
PRONIA_Demographics <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/PRONIA_ROD_CHR_Demographics_filtered (2).xls")
CHR_S_Nikos_Pred <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_S_Nikos_Pred.xls")
CHR_R_Nikos_Pred <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/CHR_R_Nikos_Pred.xls")
transition_models <- read_excel("C:/Users/derya/Desktop/Algorithmic Fairness/ProniaFairness/transition_models (1).xls")
transition_multimodal <- read_excel("C:/Users/derya/Downloads/TransitionPredictions4Joseph.xlsx")

names(CHR_raters)
pronia <- CHR_raters %>% select("PSN", "PROGNOSTIC_01_01_Transition_T0", "PROGNOSTIC_02_01_PoorOutcome_T0")
pronia <- full_join(pronia, PRONIA_Demographics, by="PSN")

italy <- c("ITALIA", "Italia", "italy", "Italy", "Itlay", "Milan", "Milano", "udine", "Udine")
germany <- c("germany", "Germany", "Germnay", "Deutschland", "Munich")
uk <- c("England", "UK", "United Kingdom")
finland <- c("Finland", "Suomi")

colnames(pronia)[9] <- "Education"
colnames(pronia)[11] <- "CurrentCountry"
colnames(pronia)[18] <- "Ethnicity"

pronia <- pronia %>% mutate(
  CurrentCountry = if_else(CurrentCountry %in% italy, "IT", CurrentCountry),
  CurrentCountry = if_else(CurrentCountry %in% germany, "DE", CurrentCountry),
  CurrentCountry = if_else(CurrentCountry %in% uk, "GB", CurrentCountry),
  CurrentCountry = if_else(CurrentCountry %in% finland, "FI", CurrentCountry),
  CurrentCountry = if_else(CurrentCountry=="Switzerland", "CH", CurrentCountry)
)

pronia <- pronia %>% mutate(
  Ethnicity = if_else(Ethnicity=="German", "DE", Ethnicity),
  Ethnicity = if_else(Ethnicity=="Swiss", "CH", Ethnicity),
  Ethnicity = if_else(Ethnicity=="Finnish", "FI", Ethnicity),
  Ethnicity = if_else(Ethnicity=="Turkish", "TR", Ethnicity),
  Ethnicity = if_else(Ethnicity=="Italian", "IT", Ethnicity),
  Ethnicity = if_else(Ethnicity=="British", "UK", Ethnicity),
)

pronia <- pronia %>% mutate(
  AGE_T0= as.numeric(AGE_T0),
  AGE_T0 = ifelse(AGE_T0>100, AGE_T0/1000, AGE_T0),
  edstat = ifelse(Education > median(Education, na.rm=TRUE), "high", "low"))

pronia <- pronia %>% mutate(
  PROGNOSTIC_01_01_Transition_T0 = as.factor(PROGNOSTIC_01_01_Transition_T0),
  PROGNOSTIC_02_01_PoorOutcome_T0 = as.factor(PROGNOSTIC_02_01_PoorOutcome_T0),
  Studygroup = as.factor(Studygroup),
  PoB_Country = as.factor(PoB_Country),
  SEX = as.factor(SEX),
  Education = as.numeric(Education),
  Ethnicity = as.factor(Ethnicity),
  CurrentCountry = as.factor(CurrentCountry),
  DEMOG_T0T1T2_28A_MaritalCurrent_T0 = as.factor(DEMOG_T0T1T2_28A_MaritalCurrent_T0)
)



pronia_chr <- pronia %>%  filter(Studygroup=="CHR")

pronia_transition <- inner_join(pronia_chr, transition_models, by="PSN")

### Fairness Selected Models for Transition (Carrion, Salokangas, Cornblatt, Haidl, Clinician Rating)

sensitive_attributes <- c("SEX", "edstat")
models <- c("transition", "carrion", "salokangas", "cornblatt", "haidl", "PROGNOSTIC_01_01_Transition_T0")

res_carrion <- pronia_transition %>% 
  select("PSN", sensitive_attributes, models) %>% 
  mutate(
    TP=ifelse(transition==1 & carrion==1, 1,0),
    TN=ifelse(transition==0 & carrion==0, 1,0),
    FP=ifelse(transition==0 & carrion==1, 1,0),
    FN=ifelse(transition==1 & carrion==0, 1,0)) #%>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum) )

res_salokangas <- pronia_transition %>% 
  select("PSN", sensitive_attributes, models) %>% 
  mutate(
    TP=ifelse(transition==1 & salokangas==1, 1,0),
    TN=ifelse(transition==0 & salokangas==0, 1,0),
    FP=ifelse(transition==0 & salokangas==1, 1,0),
    FN=ifelse(transition==1 & salokangas==0, 1,0)) #%>%
#  summarize_at(
 #   vars(TP, TN, FP, FN), funs(sum))

res_haidl <- pronia_transition %>% 
  select("PSN", sensitive_attributes, models) %>% 
  mutate(
    TP=ifelse(transition==1 & haidl==1, 1,0),
    TN=ifelse(transition==0 & haidl==0, 1,0),
    FP=ifelse(transition==0 & haidl==1, 1,0),
    FN=ifelse(transition==1 & haidl==0, 1,0))# %>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum))

res_cornblatt <- pronia_transition %>% 
  select("PSN", sensitive_attributes, models) %>% 
  mutate(
    TP=ifelse(transition==1 & cornblatt==1, 1,0),
    TN=ifelse(transition==0 & cornblatt==0, 1,0),
    FP=ifelse(transition==0 & cornblatt==1, 1,0),
    FN=ifelse(transition==1 & cornblatt==0, 1,0)) #%>%
  #summarize_at(
   # vars(TP, TN, FP, FN), funs(sum))

res_clinician <- pronia_transition %>% 
  select("PSN", sensitive_attributes, models) %>% 
  mutate(
    TP=ifelse(transition==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(transition==0 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(transition==0 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(transition==1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0)) #%>%
 # summarize_at(
#    vars(TP, TN, FP, FN), funs(sum))

sex_t <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
             res_carrion %>% mutate(Model='Carrion'),
             res_salokangas %>% mutate(Model="Salokangas"),
             res_clinician %>% mutate(Model="Clinician")
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean)
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
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
  facet_grid(.~name, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
   subtitle = "(Protected Group: Male)")


sex_t_table <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
                     res_carrion %>% mutate(Model='Carrion'),
                     res_salokangas %>% mutate(Model="Salokangas"),
                     res_clinician %>% mutate(Model="Clinician")
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))




education_t <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
             res_carrion %>% mutate(Model='Carrion'),
             res_salokangas %>% mutate(Model="Salokangas"),
             res_clinician %>% mutate(Model="Clinician")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean)
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
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

ed_t_table <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
                     res_carrion %>% mutate(Model='Carrion'),
                     res_salokangas %>% mutate(Model="Salokangas"),
                     res_clinician %>% mutate(Model="Clinician")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))


### Fairness Functioning


fairness_functioning_r <- inner_join(CHR_R_Nikos_Pred, pronia_chr, by="PSN")

fairness_functioning_s <- inner_join(CHR_S_Nikos_Pred, pronia_chr, by="PSN")

res_funcr <- fairness_functioning_r %>% 
  select("PSN", sensitive_attributes, GF_R_Pred, GF_R_True) %>% 
  mutate(
    TP=ifelse(GF_R_True==1 & GF_R_Pred==1, 1,0),
    TN=ifelse(GF_R_True==-1 & GF_R_Pred==-1, 1,0),
    FP=ifelse(GF_R_True==-1 & GF_R_Pred==1, 1,0),
    FN=ifelse(GF_R_True==1 & GF_R_Pred==-1, 1,0)) %>%
  select(-c("GF_R_Pred", "GF_R_True"))#%>%
# summarize_at(
#  vars(TP, TN, FP, FN), funs(sum) )

res_funcs <- fairness_functioning_s %>% 
  select("PSN", sensitive_attributes, GF_S_Pred, GF_S_True) %>% 
  mutate(
    TP=ifelse(GF_S_True==1 & GF_S_Pred==1, 1,0),
    TN=ifelse(GF_S_True==-1 & GF_S_Pred==-1, 1,0),
    FP=ifelse(GF_S_True==-1 & GF_S_Pred==1, 1,0),
    FN=ifelse(GF_S_True==1 & GF_S_Pred==-1, 1,0)) %>%
  select(-c("GF_S_Pred", "GF_S_True")) #%>%
#  summarize_at(
#    vars(TP, TN, FP, FN), funs(sum) )


sex_f <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
             res_funcs %>% mutate(Model='Social Functioning')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean)
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
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
  facet_grid(.~name, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
          subtitle = "(Protected Group: Male)")


## sex functioning table 

sex_f_table <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
      res_funcs %>% mutate(Model='Social Functioning')) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))



education_f <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
                   res_funcs %>% mutate(Model='Social Functioning')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean)
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
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


##education_f_ table

rbind(res_funcr %>% mutate(Model='Role Functioning'),
      res_funcs %>% mutate(Model='Social Functioning')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))



#### multimodal transition

transition <- transition_models %>% select(c("PSN", "transition"))
transition <- left_join(transition, pronia, by="PSN")
transition_multimodal$PSN <- as.double(transition_multimodal$PSN)
all_transition <- left_join(transition, transition_multimodal, by="PSN")
all_transition <- all_transition %>% filter(!is.na(Studygroup))


res_mri <- all_transition %>% 
  select("PSN", sensitive_attributes, "transition", "PRED_LABEL_MRI") %>% 
  mutate(
    TP=ifelse(transition==1 & PRED_LABEL_MRI==1, 1,0),
    TN=ifelse(transition==0 & PRED_LABEL_MRI==-1, 1,0),
    FP=ifelse(transition==0 & PRED_LABEL_MRI==1, 1,0),
    FN=ifelse(transition==1 & PRED_LABEL_MRI==-1, 1,0)) %>%
  select(-"PRED_LABEL_MRI")# %>%
# summarize_at(
 # vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_prs <- all_transition %>% 
  select("PSN", sensitive_attributes, "transition", "PRED_LABEL_PRS") %>% 
  mutate(
    TP=ifelse(transition==1 & PRED_LABEL_PRS==1, 1,0),
    TN=ifelse(transition==0 & PRED_LABEL_PRS==-1, 1,0),
    FP=ifelse(transition==0 & PRED_LABEL_PRS==1, 1,0),
    FN=ifelse(transition==1 & PRED_LABEL_PRS==-1, 1,0)) %>%
  select(-"PRED_LABEL_PRS") #%>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_clin <- all_transition %>% 
  select("PSN", sensitive_attributes, "transition", "PRED_LABEL_Clin") %>% 
  mutate(
    TP=ifelse(transition==1 & PRED_LABEL_Clin==1, 1,0),
    TN=ifelse(transition==0 & PRED_LABEL_Clin==-1, 1,0),
    FP=ifelse(transition==0 & PRED_LABEL_Clin==1, 1,0),
    FN=ifelse(transition==1 & PRED_LABEL_Clin==-1, 1,0)) %>%
  select(-"PRED_LABEL_Clin") #%>%
 # summarize_at( vars(TP, TN, FP, FN), funs(sum(.,na.rm=TRUE)))

res_stk <- all_transition %>% 
  select("PSN", sensitive_attributes, "transition", "PRED_LABEL_Stk") %>% 
  mutate(
    TP=ifelse(transition==1 & PRED_LABEL_Stk==1, 1,0),
    TN=ifelse(transition==0 & PRED_LABEL_Stk==-1, 1,0),
    FP=ifelse(transition==0 & PRED_LABEL_Stk==1, 1,0),
    FN=ifelse(transition==1 & PRED_LABEL_Stk==-1, 1,0)) %>%
  select(-"PRED_LABEL_Stk") #%>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(.,na.rm = TRUE)))


sex_multimodal <- rbind(res_mri %>% mutate(Model='MRI'),
               res_clin %>% mutate(Model='Clinical'),
               res_prs %>% mutate(Model='Polygenetic Risk Score'),
               res_stk %>% mutate(Model='Stacked')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=TRUE))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
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
  facet_grid(.~name, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  ggtitle(label = 'Fairness criteria for sensitive attribute: Sex', 
          subtitle = "(Protected Group: Male)")


## sex functioning table 

sex_multimodal_table <- rbind(res_mri %>% mutate(Model='MRI'),
                              res_clin %>% mutate(Model='Clinical'),
                              res_prs %>% mutate(Model='Polygenetic Risk Score'),
                              res_stk %>% mutate(Model='Stacked')
)%>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=TRUE)))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))



education__multimodal <- rbind(res_mri %>% mutate(Model='MRI'),
                               res_clin %>% mutate(Model='Clinical'),
                               res_prs %>% mutate(Model='Polygenetic Risk Score'),
                               res_stk %>% mutate(Model='Stacked')
)%>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=TRUE))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    #"Equal Opportunity" = FN/(TP+FN),
    "Predictive Equality" = FP/(FP+TN),
    "Predictive Parity" = TP/(TP+FP),
    #"Treatment Equality" = (FN/FP),
    "MCC Parity" = abs((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
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


##education_f_ table

rbind(res_mri %>% mutate(Model='MRI'),
      res_clin %>% mutate(Model='Clinical'),
      res_prs %>% mutate(Model='Polygenetic Risk Score'),
      res_stk %>% mutate(Model='Stacked')
)%>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(.,na.rm=TRUE)))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))
