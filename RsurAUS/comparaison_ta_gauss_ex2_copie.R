rm(list=ls())

#install.packages("rtauargus", repos = "https://nexus.insee.fr/repository/r-public",  type="source")
#install.packages("GaussSuppression")
#install.packages("tidyverse")
#install.packages("curl")
#install.packages("readxl")

library(GaussSuppression)
library(dplyr)
library(rtauargus)
library(SSBtools)
library(tidyverse)
library(curl)
library(readxl)

loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

data("turnover_act_cj")
Dat <- as.data.frame(activity_corr_table)
a<-activity_corr_table #le problème de activity_corr_table est qaue certaines cellules ne possèdent pas de subdivision ce qui est problématiques pour la fonction FindDimList
head(activity_corr_table) # donc on va filtrer ce dataframe pour enlever les lignes problématiques 

a_filtrer <- a %>% 
  filter(!apply(., 1, function(row) any(duplicated(row))))

str(turnover_act_cj)
unique(turnover_act_cj$ACTIVITY)

tab_ex2 <- turnover_act_cj %>% 
  mutate(is_secret_prim = N_OBS > 0 & N_OBS < 3)

hier_activite <- SSBtools::FindDimLists(a_filtrer)[[1]]
# names(hier_activite) <- "ACTIVITY"
# 
# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(ACTIVITY %in% activity_corr_table$A88) %>% 
  filter(CJ != "Total") %>% 
  filter(ACTIVITY != "Total") %>%
  select(-TOT, -MAX)

hier_CJ <- SSBtools::FindDimLists(tab_ex2_pr_gauss[,2])[[1]]

masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:2,
    freqVar = "N_OBS",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE,
    hierarchies = list(ACTIVITY=hier_activite, CJ = hier_CJ)
  )

str(masq_gauss)
# Pose du secret avec TAU-ARGUS ------------------
hrc_file_activity <- write_hrc2(
  corr_table = activity_corr_table,
  file_name = "hrc/activity.hrc"
)

masq_ta <- tab_rtauargus(
  tab_ex2,
  dir_name = "tauargus_files/ex2",
  files_name = "ex2",
  explanatory_vars = c("ACTIVITY","CJ"), #equivalent de DimVar
  value = "N_OBS",
  freq = "N_OBS", #freqVAR
  secret_var = "is_secret_prim",
  hrc = c(ACTIVITY = hrc_file_activity),
  totcode = c(ACTIVITY="Total",CJ="Total"),
  suppress = "MOD(1,5,0,0,0)"
)
# V = cellule valide
# D = Secret secondaire
# B = secret primaire

# Comparaison --------------------------------
masq_gauss %>% count(suppressed)
masq_ta %>% count(Status!="V")

masq_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )

masq_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )






masq_gauss %>% 
  mutate(Status_Gauss = case_when(
    primary ~ "B",
    suppressed ~"D",
    TRUE ~ "V"
  )) %>% 
  select(ACTIVITY, CJ, N_OBS, Status_Gauss) %>% 
  full_join(
    masq_ta %>% select(ACTIVITY, CJ, N_OBS, Status),
    by = c("ACTIVITY", "CJ", "N_OBS")
  ) %>% 
  mutate(
    Status = ifelse(N_OBS == 0, "V", Status)
  ) %>% 
  mutate(diff_status = Status_Gauss != Status) %>% 
  filter(diff_status)
data <- as.data.frame(turnover_act_cj)
datF <- data %>% 
  pivot_wider(names_from = 'ACTIVITY', values_from = 'N_OBS')
print(datF, n = 100)

str(data)
