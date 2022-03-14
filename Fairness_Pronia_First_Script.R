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

### Fairness Selected Models (Carrion, Salokangas, Cornblatt, Haidl, Clinician Rating)

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

sex <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
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


education <- rbind(res_cornblatt %>% mutate(Model='Cornblatt'),
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
