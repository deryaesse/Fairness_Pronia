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
pronia <- left_join(PRONIA_Demographics, pronia, by="PSN")

italy <- c("ITALIA", "Italia", "italy", "Italy", "Itlay", "Milan", "Milano", "udine", "Udine")
germany <- c("germany", "Germany", "Germnay", "Deutschland", "Munich")
uk <- c("England", "UK", "United Kingdom")
finland <- c("Finland", "Suomi")

colnames(pronia)[7] <- "Education"
colnames(pronia)[9] <- "CurrentCountry"
colnames(pronia)[16] <- "Ethnicity"

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
  edstat = ifelse(Education > median(Education, na.rm=TRUE), "high", "low"),
  edstat = as.factor(edstat))

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

#####################################################
#### ADDING TOGETHER TRANSITION AND DEMOGRAPHIC DATA
#####################################################

#pronia_chr <- pronia %>%  filter(Studygroup=="CHR")
transition_multimodal$PSN <- as.numeric(transition_multimodal$PSN)
pronia_transition <- left_join(pronia, transition_multimodal, by="PSN")


trans_1 <- transition_multimodal %>% select(LABEL_BINARY...2, LABEL_BINARY...3, PSN)
trans_2 <- transition_models %>% select(PSN, transition)

pronia_transition <- left_join(pronia_transition, trans_2, by="PSN")

sum(is.na(pronia_transition$LABEL_BINARY_CODE))

pronia_transition <- pronia_transition %>%
  mutate(LABEL_BINARY_CODE = if_else(is.na(LABEL_BINARY_CODE), transition, LABEL_BINARY_CODE)) %>%
  mutate(LABEL_BINARY_CODE = if_else(LABEL_BINARY_CODE==0, -1, LABEL_BINARY_CODE))


sum(is.na(pronia_transition$LABEL_BINARY_CODE))


sensitive_attributes <- c("SEX", "edstat")

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
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0)) #%>%
# summarize_at(
#    vars(TP, TN, FP, FN), funs(sum))


######################################################################################################
### Fairness Selected Models for Transition (Carrion, Salokangas, Cornblatt, Haidl, Clinician Rating)
######################################################################################################

pronia_transition_models <- full_join((pronia_transition %>% select(-"transition")), transition_models, by="PSN")

models <- c("transition", "carrion", "salokangas", "cornblatt", "haidl")

res_carrion <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & carrion==1, 1,0),
    TN=ifelse(transition==0 & carrion==0, 1,0),
    FP=ifelse(transition==0 & carrion==1, 1,0),
    FN=ifelse(transition==1 & carrion==0, 1,0)) #%>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum) )

res_salokangas <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & salokangas==1, 1,0),
    TN=ifelse(transition==0 & salokangas==0, 1,0),
    FP=ifelse(transition==0 & salokangas==1, 1,0),
    FN=ifelse(transition==1 & salokangas==0, 1,0)) #%>%
#  summarize_at(
 #   vars(TP, TN, FP, FN), funs(sum))

res_haidl <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & haidl==1, 1,0),
    TN=ifelse(transition==0 & haidl==0, 1,0),
    FP=ifelse(transition==0 & haidl==1, 1,0),
    FN=ifelse(transition==1 & haidl==0, 1,0))# %>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum))

res_cornblatt <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(transition==1 & cornblatt==1, 1,0),
    TN=ifelse(transition==0 & cornblatt==0, 1,0),
    FP=ifelse(transition==0 & cornblatt==1, 1,0),
    FN=ifelse(transition==1 & cornblatt==0, 1,0)) #%>%
  #summarize_at(
   # vars(TP, TN, FP, FN), funs(sum))

res_clin <- pronia_transition_models %>% 
  select("PSN", sensitive_attributes, models, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0)) #%>%
 # summarize_at(
#    vars(TP, TN, FP, FN), funs(sum))

sex_t <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
             res_carrion %>% mutate(Model='Carrion'),
             res_salokangas %>% mutate(Model="Salokangas"),
             res_clin %>% mutate(Model="Clinician")
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T))
  )%>%
  mutate(
    "Accuracy Equality" = (TP+TN)/(TP+FP+TN+FN),
    "Equalized Odds" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
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
                     res_clin %>% mutate(Model="Clinician")
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T)))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))




education_t <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
             res_carrion %>% mutate(Model='Carrion'),
             res_salokangas %>% mutate(Model="Salokangas"),
             res_clin %>% mutate(Model="Clinician")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
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
                     res_clin %>% mutate(Model="Clinician")
) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(mean(., na.rm=T)))%>%
  mutate(PPV = TP/(TP+FP),
         NPV = TN/(TN+FN),
         Accuracy = TP+TN/(FP+TN+FN+TP),
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP)) %>%
  select(-c(TP, FP, TN, FN))


### Fairness Functioning


fairness_functioning_r <- inner_join(CHR_R_Nikos_Pred, pronia, by="PSN")

fairness_functioning_s <- inner_join(CHR_S_Nikos_Pred, pronia, by="PSN")

res_funcr <- fairness_functioning_r %>% 
  select("PSN", sensitive_attributes, GF_R_Pred, GF_R_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_R_True==1 & GF_R_Pred==1, 1,0),
    TN=ifelse(GF_R_True==-1 & GF_R_Pred==-1, 1,0),
    FP=ifelse(GF_R_True==-1 & GF_R_Pred==1, 1,0),
    FN=ifelse(GF_R_True==1 & GF_R_Pred==-1, 1,0)) %>%
  select(-c("GF_R_Pred", "GF_R_True")) #%>%
# summarize_at(
 # vars(TP, TN, FP, FN), funs(sum) )

res_funcs <- fairness_functioning_s %>% 
  select("PSN", sensitive_attributes, GF_S_Pred, GF_S_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_S_True==1 & GF_S_Pred==1, 1,0),
    TN=ifelse(GF_S_True==-1 & GF_S_Pred==-1, 1,0),
    FP=ifelse(GF_S_True==-1 & GF_S_Pred==1, 1,0),
    FN=ifelse(GF_S_True==1 & GF_S_Pred==-1, 1,0)) %>%
  select(-c("GF_S_Pred", "GF_S_True")) %>%
  summarize_at(
    vars(TP, TN, FP, FN), funs(sum) )

res_f_clinician_r <- fairness_functioning_r %>% 
  select("PSN", sensitive_attributes, GF_R_Pred, GF_R_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_R_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    TN=ifelse(GF_R_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0),
    FP=ifelse(GF_R_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    FN=ifelse(GF_R_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0)) %>%
  select(-c("GF_R_Pred", "GF_R_True")) #%>%
#  summarize_at(
 #   vars(TP, TN, FP, FN), funs(sum))

res_f_clinician_s <- fairness_functioning_s %>% 
  select("PSN", sensitive_attributes, GF_S_Pred, GF_S_True, PROGNOSTIC_02_01_PoorOutcome_T0) %>% 
  mutate(
    TP=ifelse(GF_S_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    TN=ifelse(GF_S_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0),
    FP=ifelse(GF_S_True==-1 & PROGNOSTIC_02_01_PoorOutcome_T0==0, 1,0),
    FN=ifelse(GF_S_True==1 & PROGNOSTIC_02_01_PoorOutcome_T0==1, 1,0)) %>%
  select(-c("GF_S_Pred", "GF_S_True")) #%>%
#  summarize_at(
#    vars(TP, TN, FP, FN), funs(sum))

sex_f_r <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
             res_f_clinician_r %>% mutate(Model='Clinician Prediction')
) %>%
  group_by(Model, SEX) %>%
  filter(!is.na(SEX)) %>%
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
    "Equalized Odss" = ((TP/(TP+FN))+(TN/(TN+FP))/2),
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

sex_f_table_r <- rbind(res_funcr %>% mutate(Model='Role Functioning'),
                       res_f_clinician_r %>% mutate(Model='Clinician Prediction')) %>%
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


sex_f_table_s <- rbind(res_funcs %>% mutate(Model='Social Functioning'),
                       res_f_clinician_s %>% mutate(Model='Clinician Prediction'))%>%
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

######
# education fairness functional outcome
######

education_f_r <- rbind(res_funcr %>% mutate(Model='Role Functioning - ML Model'),
                     res_f_clinician_r %>% mutate(Model='Clinician Prediction')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
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


education_f_s <- rbind(res_funcs %>% mutate(Model='Social Functioning - ML Model'),
                       res_f_clinician_s %>% mutate(Model='Clinician Prediction')) %>%
  group_by(Model, edstat) %>%
  filter(!is.na(edstat)) %>%
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

rbind(res_funcr %>% mutate(Model='Role Functioning'),
      res_f_clinician_r %>% mutate(Model='Clinician Rating')) %>%
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


##education_f_s table

rbind(res_funcs %>% mutate(Model='Social Functioning'),
      res_f_clinician_s %>% mutate(Model='Clinician Rating')) %>%
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
  select(-"PRED_LABEL_MRI") #%>%
# summarize_at(
#  vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_prs <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_PRS) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_PRS==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_PRS==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_PRS==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_PRS==-1, 1,0)) %>%
  select(-"PRED_LABEL_PRS") #%>%
 # summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(., na.rm = TRUE)))

res_clin <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_Clin) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Clin==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Clin==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Clin==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Clin==-1, 1,0)) %>%
  select(-"PRED_LABEL_Clin") #%>%
  #summarize_at( vars(TP, TN, FP, FN), funs(sum(.,na.rm=TRUE)))

res_stk <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PRED_LABEL_Stk) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Stk==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Stk==-1, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PRED_LABEL_Stk==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PRED_LABEL_Stk==-1, 1,0)) %>%
  select(-"PRED_LABEL_Stk") #%>%
 #summarize_at(
  #  vars(TP, TN, FP, FN), funs(sum(.,na.rm = TRUE)))


res_crrat <- pronia_transition %>% 
  select("PSN", sensitive_attributes, LABEL_BINARY_CODE, PROGNOSTIC_01_01_Transition_T0) %>% 
  mutate(
    TP=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    TN=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==0, 1,0),
    FP=ifelse(LABEL_BINARY_CODE==-1 & PROGNOSTIC_01_01_Transition_T0==1, 1,0),
    FN=ifelse(LABEL_BINARY_CODE==1 & PROGNOSTIC_01_01_Transition_T0==0,1,0)) %>%
  select(-"PROGNOSTIC_01_01_Transition_T0") #%>%
#summarize_at(
 # vars(TP, TN, FP, FN), funs(sum(.,na.rm = TRUE)))

sex_multimodal <- rbind(res_mri %>% mutate(Model='MRI'),
               res_clin %>% mutate(Model='Clinical'),
               res_prs %>% mutate(Model='Polygenetic Risk Score'),
               res_stk %>% mutate(Model='Stacked'),
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

sex_multimodal_table <- rbind(res_mri %>% mutate(Model='MRI'),
                              res_clin %>% mutate(Model='Clinical'),
                              res_prs %>% mutate(Model='Polygenetic Risk Score'),
                              res_stk %>% mutate(Model='Stacked'),
                              res_crrat %>% mutate(Model='Clinician Rating')
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
                               res_stk %>% mutate(Model='Stacked'),
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
    "Predictive Parity" = TP/(TP+FP),
    "Treatment Equality" = (FN/FP),
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


##education_f_ table

rbind(res_mri %>% mutate(Model='MRI'),
      res_clin %>% mutate(Model='Clinical'),
      res_prs %>% mutate(Model='Polygenetic Risk Score'),
      res_stk %>% mutate(Model='Stacked'),
      res_crrat %>% mutate(Model='Clinician Rating')
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

