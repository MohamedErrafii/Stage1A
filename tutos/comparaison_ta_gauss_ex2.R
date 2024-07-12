library(dplyr)
library(rtauargus)
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

data("turnover_act_cj")
data(activity_corr_table)
head(activity_corr_table)

str(turnover_act_cj)
unique(turnover_act_cj$ACTIVITY)

tab_ex2 <- turnover_act_cj %>% 
  mutate(is_secret_prim = N_OBS > 0 & N_OBS < 3)


# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(ACTIVITY %in% activity_corr_table$A88) %>% 
  filter(CJ != "Total") %>% 
  filter(ACTIVITY != "Total") %>%
  select(-TOT, -MAX)

masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:2,
    freqVar = "N_OBS",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE
  )


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


