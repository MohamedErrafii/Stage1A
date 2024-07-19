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
