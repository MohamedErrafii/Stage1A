rm(list=ls())

# install.packages("rtauargus", repos = "https://nexus.insee.fr/repository/r-public",  type="source")
# install.packages("GaussSuppression")
# install.packages("tidyverse")
# install.packages("curl")
# install.packages("readxl")

library(GaussSuppression)
library(dplyr)
library(rtauargus)
library(SSBtools)
library(tidyverse)
library(curl)
library(readxl)
source("RsurAUS/fonction_comparaison_masque.R")

loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

data("turnover_nuts_cj")
Dat <- as.data.frame(nuts23_fr_corr_table)
a<-nuts23_fr_corr_table #le problème de nuts23_fr_corr_table est qaue certaines cellules ne possèdent pas de subdivision ce qui est problématiques pour la fonction FindDimList
head(nuts23_fr_corr_table) # donc on va filtrer ce dataframe pour enlever les lignes problématiques 

#On supprime les éléments qui n'ont pas de subdivision

a_filtrer <- a %>% 
  filter(!apply(., 1, function(row) any(duplicated(row))))

str(turnover_nuts_cj)
unique(turnover_nuts_cj$NUTS)

tab_ex2 <- turnover_nuts_cj %>% 
  mutate(is_secret_prim = N_OBS > 0 & N_OBS < 3)

hier_nuts <- SSBtools::FindDimLists(a_filtrer)[[1]]
# names(hier_nuts) <- "NUTS"
# 
# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(NUTS %in% nuts23_fr_corr_table$NUTS3) %>% 
  filter(CJ != "Total") %>% 
  filter(NUTS != "Total") %>%
  select(-TOT, -MAX)

hier_CJ <- SSBtools::FindDimLists(tab_ex2_pr_gauss[,2])[[1]]

masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:2,
    freqVar = "N_OBS",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE,
    hierarchies = list(NUTS=hier_nuts, CJ = hier_CJ)
  )

str(masq_gauss)
# Pose du secret avec TAU-ARGUS ------------------
hrc_file_nuts <- write_hrc2(
  corr_table = nuts23_fr_corr_table,
  file_name = "hrc/nuts.hrc"
)

masq_ta <- tab_rtauargus(
  tab_ex2 %>% select(-TOT),
  dir_name = "tauargus_files/ex2",
  files_name = "ex2",
  explanatory_vars = c("NUTS","CJ"), #equivalent de DimVar
  value = "N_OBS",
  freq = "N_OBS", #freqVAR
  secret_var = "is_secret_prim",
  hrc = c(NUTS = hrc_file_nuts),
  totcode = c(NUTS="Total",CJ="Total"),
  suppress = "MOD(1,5,0,0,0)"
)
# V = cellule valide
# D = Secret secondaire
# B = secret primaire

#Tout d'abors on va regarder ces lignes en  plus : 
#lignes en plus de gausssuppression 

lignes_en_plus <- anti_join(masq_gauss, masq_ta, by = c("NUTS", "CJ"))
lignes_en_plus

#on va, au cas où (mais normalement ce n'est pas le cas), si GaussSuppression a poser du secret 
#sur ces zéros

contient_SP <- any(lignes_en_plus[,ncol(lignes_en_plus)] == TRUE)
contient_SP

#si contient_SP est FALSE alors GaussSuppression n'a pas posé de secret sur ces zéros on 
#peut donc les supprimer 

masq_gauss <- anti_join(masq_gauss, lignes_en_plus, by = c("NUTS", "CJ") )

#on vérifie que le masque de gausssuppression et le masque de tau-argus peuvent être comparer 
#(i.e on le même nombre de lignes) 

lignes_en_plus <- anti_join(masq_gauss, masq_ta, by = c("NUTS", "CJ"))
lignes_en_plus

#Si lignes_en_plus contient 0 lignes on peut maintenant faire les statisques sur les différents masques

comparaison_deux_masque(masq_gauss,masq_ta)
masq_gauss %>% 
  mutate(Status_Gauss = case_when(
    primary ~ "B",
    suppressed ~"D",
    TRUE ~ "V"
  )) %>% 
  select(NUTS, CJ, N_OBS, Status_Gauss) %>% 
  full_join(
    masq_ta %>% select(NUTS, CJ, N_OBS, Status),
    by = c("NUTS", "CJ", "N_OBS")
  ) %>% 
  mutate(
    Status = ifelse(N_OBS == 0, "V", Status)
  ) %>% 
  mutate(diff_status = Status_Gauss != Status) %>% 
  filter(diff_status)

#ce code nous donne les cellules où le flag n'est pas le même, on va s'intéresser aux celles ci de plus près 
#pour savoir pourquoi le secret à été poser différemment



stats_sous_tableaux <- unique(nuts23_fr_corr_table$NUTS2) %>% 
  purrr::map(
    \(nuts2){
      filtre_NUTS3 <- nuts23_fr_corr_table %>% filter(NUTS2 == nuts2) %>% pull(NUTS3)
      masq_gauss_extract <- masq_gauss %>% filter(NUTS %in% c(nuts2, filtre_NUTS3))
      masq_ta_extract <- masq_ta %>% filter(NUTS %in% c(nuts2, filtre_NUTS3))
      comparaison_deux_masque(masq_gauss_extract, masq_ta_extract, 1:2)
    }
  )
names(stats_sous_tableaux) <- unique(nuts23_fr_corr_table$NUTS2)

stats_sous_tableaux$FR24
stats_sous_tableaux$FR41
stats_sous_tableaux$FR43
stats_sous_tableaux$FR52
stats_sous_tableaux$FR53
stats_sous_tableaux$FR62
stats_sous_tableaux$FR72
stats_sous_tableaux$FR81

#Pour FR24 

filtered_data <- a_filtrer[grepl("FR24", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR24", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR24,FR24)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR24 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR24

#Pour FR41 

filtered_data <- a_filtrer[grepl("FR41", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR41", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR41,FR41)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR41 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR41

#Pour FR43 

filtered_data <- a_filtrer[grepl("FR43", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR43", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR43,FR43)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR43 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR43

#Pour FR52 

filtered_data <- a_filtrer[grepl("FR52", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR52", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR52,FR52)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR52 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR52

#Pour FR53 

filtered_data <- a_filtrer[grepl("FR53", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR53", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR53,FR53)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR53 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR53

#Pour FR62 

filtered_data <- a_filtrer[grepl("FR62", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR62", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR62,FR62)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR62 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR62

#Pour FR72 

filtered_data <- a_filtrer[grepl("FR72", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR72", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR72,FR72)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR72 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR72

#Pour FR81 

filtered_data <- a_filtrer[grepl("FR81", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, NUTS %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FR81", a_filtrer$NUTS2), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, NUTS %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("NUTS","CJ","N_OBS"))
merged_data <- merged_data[,c("NUTS","CJ","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("NUTS","CJ","N_OBS")] %>% 
  pivot_wider(names_from = "NUTS", values_from = 'N_OBS')

merged_data_visuel<-merged_data_visuel %>% 
  select(-FR81,FR81)
merged_data_visuel <- merged_data_visuel %>% 
  slice(-1) %>% 
  add_row(merged_data_visuel[1,])
latex_FR81 <- merged_data_visuel <- merged_data_visuel %>% 
  knitr::kable(format = "latex", digits = 1)
latex_FR81




