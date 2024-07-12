install.packages(
  "rtauargus", 
  repos = "https://nexus.insee.fr/repository/r-public",  
  type="source"
)
library(dplyr)
library(data.table)
library(rtauargus)

loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)


ET_T2_AE <- readRDS(
  "X:/HAB-Traitement-Confidentialite/TESTS/Gauss/ET_T2_AE.rds"
)

tab2_ET_AE <- tabulate_micro_data (
  df = ET_T2_AE,
  cat_vars = c("TREFF_ET", "DEP_ET"),
  hrc_vars = list(APE = c("APET", "NAF_ag")),
  resp_var = c("NBRE"),
  marge_label = "T")

str(tab2_ET_AE)

corr_APE_ag <- ET_T2_AE %>% select(NAF_ag, APET) %>% unique() %>% arrange(NAF_ag)
write_hrc2(corr_APE_ag, file_name = "hrc/APE_AE.hrc")


### SECRET PRIMAIRE

T2_SP_AE <- tab2_ET_AE %>%
  mutate(is_secret_prim = NBRE_tot > 0 & NBRE_tot < 3)

## nettoyage du tableau pour passage dans tau-argus
T2_SP_AE <- T2_SP_AE %>% select(-c("NBRE_max", "nb_obs"))
T2_SP_AE$NBRE_tot <- as.integer(T2_SP_AE$NBRE_tot)

### secret secondaire
masque_T2_AE <- tab_rtauargus (
  T2_SP_AE,
  dir_name = "tauargus_files/",
  files_name = "tab_2_AE",
  explanatory_vars = c("APE", "TREFF_ET", "DEP_ET"),
  hrc = list(APE = "hrc/APE_AE.hrc"),
  secret_var = "is_secret_prim",
  value = "NBRE_tot",
  freq = "NBRE_tot",
  totcode = c(APE = "T", TREFF_ET ="T", DEP_ET="T"),
  verbose = FALSE,
  split_tab = FALSE
)

tab2_ET_AE <- tabulate_micro_data (
  df = ET_T2_AE,
  cat_vars = c("TREFF_ET", "DEP_ET"),
  hrc_vars = list(APE = c("APET", "NAF_ag")),
  resp_var = c("NBRE"),
  marge_label = "T")


###############  SECRET  ###################

## Fichier hiérarchique (à faire s'il n'est pas encore créé)
# corr_APE2 <- naf2 %>% unique() %>% arrange(NAF2)
# corr_APE1 <- naf1 %>% unique() %>% arrange(NAF1)

corr_APE_ag <- ET_T2_AE %>% select(NAF_ag, APET) %>% unique() %>% arrange(NAF_ag)
write_hrc2(corr_APE_ag, file_name = "hrc/APE_AE.hrc")


### SECRET PRIMAIRE

T2_SP_AE <- tab2_ET_AE %>%
  mutate(is_secret_prim = NBRE_tot > 0 & NBRE_tot < 3)

## nettoyage du tableau pour passage dans tau-argus
T2_SP_AE <- T2_SP_AE %>% select(-c("NBRE_max", "nb_obs"))
T2_SP_AE$NBRE_tot <- as.integer(T2_SP_AE$NBRE_tot)

### secret secondaire
masque_T2_AE <- tab_rtauargus (
  T2_SP_AE,
  dir_name = "tauargus_files/",
  files_name = "tab_2_AE",
  explanatory_vars = c("APE", "TREFF_ET", "DEP_ET"),
  hrc = list(APE = "hrc/APE_AE.hrc"),
  secret_var = "is_secret_prim",
  value = "NBRE_tot",
  freq = "NBRE_tot",
  totcode = c(APE = "T", TREFF_ET ="T", DEP_ET="T"),
  verbose = FALSE,
  split_tab = FALSE
)