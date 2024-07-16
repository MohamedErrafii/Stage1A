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

data("turnover_act_size")
Dat <- as.data.frame(activity_corr_table)
a<-activity_corr_table #le problème de activity_corr_table est qaue certaines cellules ne possèdent pas de subdivision ce qui est problématiques pour la fonction FindDimList
head(activity_corr_table) # donc on va filtrer ce dataframe pour enlever les lignes problématiques 

#On supprime les éléments qui n'ont pas de subdivision

a_filtrer <- a %>% 
  filter(!apply(., 1, function(row) any(duplicated(row))))

str(turnover_act_size)
unique(turnover_act_size$ACTIVITY)

tab_ex2 <- turnover_act_size %>% 
  mutate(is_secret_prim = N_OBS > 0 & N_OBS < 3)

hier_activite <- SSBtools::FindDimLists(a_filtrer)[[1]]
# names(hier_activite) <- "ACTIVITY"
# 
# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(ACTIVITY %in% activity_corr_table$A88) %>% 
  filter(SIZE != "Total") %>% 
  filter(ACTIVITY != "Total") %>%
  select(-TOT, -MAX)

hier_SIZE <- SSBtools::FindDimLists(tab_ex2_pr_gauss[,2])[[1]]

masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:2,
    freqVar = "N_OBS",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE,
    hierarchies = list(ACTIVITY=hier_activite, SIZE = hier_SIZE)
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
  explanatory_vars = c("ACTIVITY","SIZE"), #equivalent de DimVar
  value = "N_OBS",
  freq = "N_OBS", #freqVAR
  secret_var = "is_secret_prim",
  hrc = c(ACTIVITY = hrc_file_activity),
  totcode = c(ACTIVITY="Total",SIZE="Total"),
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
  select(ACTIVITY, SIZE, N_OBS, Status_Gauss) %>% 
  full_join(
    masq_ta %>% select(ACTIVITY, SIZE, N_OBS, Status),
    by = c("ACTIVITY", "SIZE", "N_OBS")
  ) %>% 
  mutate(
    Status = ifelse(N_OBS == 0, "V", Status)
  ) %>% 
  mutate(diff_status = Status_Gauss != Status) %>% 
  filter(diff_status)
# Tableau bcp trop grand pour poser le secret à la main
# data <- as.data.frame(turnover_act_size)
# datF <- data %>% 
#   pivot_wider(names_from = 'ACTIVITY', values_from = 'N_OBS')
# print(datF, n = 100)
# 
# str(data)





#######################################
# On va s'intéresser au sous tableaux 

#On va d'abord chercher les subdivisons dans le tableau originale à l'aide du tableaux hierarchiques

# get_all_subdivisions <- function(df, division_code) {
#   division_level <- df$levels[df$codes == division_code]
#   direct_subdivisions <- df[df$levels == paste0(division_level, "@"), ]
#   all_subdivisions <- direct_subdivisions
#   for (sub_code in direct_subdivisions$codes) {
#     all_subdivisions <- rbind(all_subdivisions, get_all_subdivisions(df, sub_code))
#   }
#   
#   return(rbind(df[df$codes == division_code, ], all_subdivisions))
# }
# 
# # Maintenant, on va utiliser cette fonction pour sélectionner ce qui nous intéresse en créant  une autre fonctions 
# 
# filter_data_by_division <- function(data, hierarchy, division) {
#   subdivisions <- get_all_subdivisions(hierarchy, division)
#   subdivision_codes <- subdivisions$codes
#   filtered_data <- data[data$Division %in% subdivision_codes, ]
#   
#   return(filtered_data)
# }
# 
# filtered_data <- filter_data_by_division(masq_gauss, hier_activite, "01")
# 
# print(filtered_data)
# 
# df<-hier_activite
# division_code = "AZ"

#ca marche pas mais on va tenter de le faire plus manuellemnt 

#tout d'abors on modifie un peu masq_gauss  pour le comparer plus facilement à tau-argus 

masq_gauss <- masq_gauss %>% 
  mutate(Status_Gauss = case_when(
    primary ~ "B",
    suppressed ~"D",
    TRUE ~ "V"))

#####################################################
#Pour AZ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("AZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("AZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","primary","suppressed","is_secret_prim","Status")]

print(merged_data)
merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

#Pour E

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("E", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("E", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

