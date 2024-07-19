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

source("RsurAUS/fonction_comparaison_masque.R")

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
  tab_ex2 %>% select(-TOT),
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


stats_sous_tableaux <- unique(activity_corr_table$A10) %>% 
  purrr::map(
    \(a10){
      filtre_a88 <- activity_corr_table %>% filter(A10 == a10) %>% pull(A88)
      filtre_a21 <- activity_corr_table %>% filter(A10 == a10) %>% pull(A21)
      masq_gauss_extract <- masq_gauss %>% filter(ACTIVITY %in% c(a10, filtre_a21,filtre_a88))
      masq_ta_extract <- masq_ta %>% filter(ACTIVITY %in% c(a10, filtre_a21,filtre_a88))
      comparaison_deux_masque(masq_gauss_extract, masq_ta_extract, 1:2)
    }
  )
names(stats_sous_tableaux) <- unique(activity_corr_table$A10)

stats_sous_tableaux$BE

stats_sous_tableaux_2 <- unique(activity_corr_table$A21) %>% 
  purrr::map(
    \(a21){
      filtre_a88 <- activity_corr_table %>% filter(A21 == a21) %>% pull(A88)
      # filtre_a21 <- activity_corr_table %>% filter(A10 == a10) %>% pull(A21)
      masq_gauss_extract <- masq_gauss %>% filter(ACTIVITY %in% c(a21, filtre_a88))
      masq_ta_extract <- masq_ta %>% filter(ACTIVITY %in% c(a21, filtre_a88))
      if(nrow(masq_gauss_extract) == 0){
        return(NULL)
      }else{
        return(comparaison_deux_masque(masq_gauss_extract, masq_ta_extract, 1:2))
      }
    }
  )
names(stats_sous_tableaux_2) <- unique(activity_corr_table$A21)
stats_sous_tableaux_2$C
map(stats_sous_tableaux_2, is.null)
#Tout d'abors on va regarder ces lignes en  plus : 
#lignes en plus de gausssuppression 

lignes_en_plus <- anti_join(masq_gauss, masq_ta, by = c("ACTIVITY", "SIZE"))
lignes_en_plus

#on va, au cas où (mais normalement ce n'est pas le cas), si GaussSuppression a poser du secret 
#sur ces zéros

contient_SP <- any(lignes_en_plus[,ncol(lignes_en_plus)] == TRUE)
contient_SP

#si contient_SP est FALSE alors GaussSuppression n'a pas posé de secret sur ces zéros on 
#peut donc les supprimer 

masq_gauss <- anti_join(masq_gauss, lignes_en_plus, by = c("ACTIVITY", "SIZE") )

#on vérifie que le masque de gausssuppression et le masque de tau-argus peuvent être comparer 
#(i.e on le même nombre de lignes) 

lignes_en_plus <- anti_join(masq_gauss, masq_ta, by = c("ACTIVITY", "SIZE"))
lignes_en_plus

#Si lignes_en_plus contient 0 lignes on peut maintenant faire les statisques sur les différents masques

comparaison_deux_masque(masq_gauss,masq_ta)
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

#Pour BE

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("BE", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("BE", a_filtrer$A10), ]
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

print(merged_data_visuel,width = 300)
merged_data_visuel <- merged_data_visuel %>% 
  select(-C,C)
merged_data_visuel <- merged_data_visuel %>% 
  select(-BE,BE)
merged_data_visuel<-merged_data_visuel[c(2:nrow(merged_data_visuel),1),]
print(merged_data_visuel, width =300)


merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

filtered_data2_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )

filtered_data2_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
lignes_en_plus <- anti_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY", "SIZE"))
lignes_en_plus
comparaison_deux_masque(filtered_data2_gauss,filtered_data2_ta)
#On remarque une différence dans la subdivision C donc on va regarder pour voir pourquoi il y a de tel différence 

#Pour C

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("C", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("C", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)
# masq_ta_C <- tab_rtauargus(
#   filtered_data2_ta %>% filter(ACTIVITY != "BE"),
#   dir_name = "tauargus_files/ex2",
#   files_name = "ex2",
#   explanatory_vars = c("ACTIVITY","SIZE"), #equivalent de DimVar
#   value = "N_OBS",
#   freq = "N_OBS", #freqVAR
#   secret_var = "is_secret_prim",
#   hrc = c(ACTIVITY = hrc_file_activity),
#   totcode = c(ACTIVITY="C",SIZE="Total"),
#   suppress = "MOD(1,5,0,0,0)"
# )
merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff
comparaison_deux_masque(filtered_data2_gauss,filtered_data2_ta)

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')
merged_data_visuel <- merged_data_visuel %>% 
  select(-C,C)
merged_data_visuel <- merged_data_visuel %>% 
  select(-BE,BE)
merged_data_visuel<-merged_data_visuel[c(2:nrow(merged_data_visuel),1),]
print(merged_data_visuel, width =300)



#cf cahier pour les commentaires sur les différence du secret secondaire 


#Pour FZ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("FZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("FZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

#le sous tableau  concernant la catégorie FZ est la même 


#Pour GI

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("GI", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("GI", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

# il y a des différences pour les subsections H et I on va regarder ça de plus près 

#Pour H 


#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("H", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("H", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


filtered_data3_gauss <- filtered_data2_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_gauss <- filtered_data3_gauss[2:3,]
filtered_data3_gauss


filtered_data3_ta <- filtered_data2_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_ta <- filtered_data3_ta[2:3,]
filtered_data3_ta




#Pour I


#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("I", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("I", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


filtered_data3_gauss <- filtered_data2_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_gauss <- filtered_data3_gauss[2:3,]
filtered_data3_gauss


filtered_data3_ta <- filtered_data2_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_ta <- filtered_data3_ta[2:3,]
filtered_data3_ta


#Pour JZ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("JZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("JZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

#Le secret est posé de la même manière sur la section JZ




#Pour KZ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("KZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("KZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff


#Le secret est posé de la même manière sur la section KZ

#Pour LZ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("LZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("LZ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff


#Le secret est posé de la même manière sur la section LZ





#Pour MN

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("MN", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("MN", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff


#On a des différences sur les sections sur les sections M et N, regardons de plus près 


# M 


#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("M", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("M", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


SSfiltered_data3_gauss <- filtered_data2_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_gauss <- filtered_data3_gauss[2:3,]
filtered_data3_gauss


filtered_data3_ta <- filtered_data2_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_ta <- filtered_data3_ta[2:3,]
filtered_data3_ta




# N 


#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("N", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2_gauss)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("N", a_filtrer$A21), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2_ta)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

merged_data_visuel <- merged_data[,c("ACTIVITY","SIZE","N_OBS")] %>% 
  pivot_wider(names_from = "ACTIVITY", values_from = 'N_OBS')

merged_data_visuel


filtered_data3_gauss <- filtered_data2_gauss %>% 
  group_by(primary,suppressed) %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_gauss <- filtered_data3_gauss[2:3,]
filtered_data3_gauss


filtered_data3_ta <- filtered_data2_ta %>% 
  group_by(is_secret_prim,is_secret = Status!="V") %>% 
  summarise(
    ncell = n(),
    valcell = sum(N_OBS)
  )
filtered_data3_ta <- filtered_data3_ta[2:3,]
filtered_data3_ta



#Pour OQ

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("OQ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("OQ", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

#Le masque de la sous table OQ  est le même 





#Pour RU

#Pour GaussSuppression
filtered_data <- a_filtrer[grepl("RU", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_gauss <- subset(masq_gauss, ACTIVITY %in% unique_cells)
print(filtered_data2)

#Pour Tau-Argus
filtered_data <- a_filtrer[grepl("RU", a_filtrer$A10), ]
flattened_data <- c(as.matrix(filtered_data))
unique_cells <- unique(flattened_data)
print(unique_cells)
filtered_data2_ta <- subset(masq_ta, ACTIVITY %in% unique_cells)
print(filtered_data2)

merged_data <- inner_join(filtered_data2_gauss, filtered_data2_ta, by = c("ACTIVITY","SIZE","N_OBS"))
merged_data <- merged_data[,c("ACTIVITY","SIZE","N_OBS","Status_Gauss","Status")]
merged_data$Same<- merged_data$Status == merged_data$Status_Gauss
print(merged_data)
merged_data_diff<-merged_data[merged_data$Same == FALSE,]
merged_data_diff

#Le masque de la sous table RU  est le même 

# Conclusion : le masque est différent uiquement pour les subdivisions BE ; GI ; MN
# Sur BE : même nombre de cellule elemine mais "+38" en terme de valeur pour GaussSuppression (donc tau-Argus fais mieux dans tout les points)
# Sur GI : "+2" en terme de cellule mais "-1567" en terme de valeur pour GaussSuppression ( donc tau-Argus est meilleur pour minimiser le nombre de case mais GaussSuppression est meilleur pour minimiser la valeur des cellules )
# Sur MN : "+3" en terme de cellule et "+401" en terme de valeur pour GaussSuppression ((donc tau-Argus fais mieux dans tout les points))

# Bilan : "+5" en terme de cellule mais "-1128" en terme de valeur pour GaussSuppression  ( donc tau-Argus est meilleur pour minimiser le nombre de case mais GaussSuppression est meilleur pour minimiser la valeur des cellules )
# cf le document qui essaye dexpliquer les didfférences sur les 3 subsectionsBE ; GI ; MN


#Comparaison des deux masques

#lignes en plus de gausssuppression 

lignes_en_plus <- anti_join(masq_gauss, masq_ta, by = c("ACTIVITY", "SIZE"))
lignes_en_plus


turnover_act_size %>% 
  group_by(ACTIVITY) %>% 
  summarise(moy = mean(TOT), freq = sum(N_OBS)) %>% 
  knitr::kable(format = "latex", digits = 1)
