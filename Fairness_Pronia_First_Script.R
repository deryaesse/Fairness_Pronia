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
germany <- c("germany", "Germany", "Germnay", "Deutschland")
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
  Ethnicity = if_else(Ethnicity=="British", "UK", Ethnicity)
)

pronia <- pronia %>% mutate(
  PROGNOSTIC_01_01_Transition_T0 = as.factor(PROGNOSTIC_01_01_Transition_T0),
  PROGNOSTIC_02_01_PoorOutcome_T0 = as.factor(PROGNOSTIC_02_01_PoorOutcome_T0),
  Studygroup = as.factor(Studygroup),
  PoB_Country = as.factor(PoB_Country),
  SEX = as.factor(SEX),
  AGE_T0 = as.numeric(AGE_T0),
  Education = as.numeric(Education),
  Ethnicity = as.factor(Ethnicity),
  CurrentCountry = as.factor(CurrentCountry),
  DEMOG_T0T1T2_28A_MaritalCurrent_T0 = as.factor(DEMOG_T0T1T2_28A_MaritalCurrent_T0)
)



pronia_chr <- pronia %>%  filter(Studygroup=="CHR")



