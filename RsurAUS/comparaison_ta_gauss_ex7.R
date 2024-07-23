rm(list=ls())
library(GaussSuppression)
library(dplyr)
library(rtauargus)
library(SSBtools)
library(tidyverse)
library(curl)
library(readxl)
source("RsurAUS/fonction_comparaison_masque.R")

T2_SP_EI <- readRDS("X:/HAB-Traitement-Confidentialite/TESTS/Gauss/T2_SP_EI.rds")
corr_APE <- readRDS("X:/HAB-Traitement-Confidentialite/TESTS/Gauss/corr_APE_ag.rds")
str(T2_SP_EI)
T2_SP_EI %>% count(is_secret_prim)
length(unique(T2_SP_EI$APE))*length(unique(T2_SP_EI$TREFF_ET))*length(unique(T2_SP_EI$DEP_ET))
str(corr_APE)






loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

Dat <- as.data.frame(corr_APE)
a<-corr_APE #le problème de corr_APE est qaue certaines cellules ne possèdent pas de subdivision ce qui est problématiques pour la fonction FindDimList
head(corr_APE) # donc on va filtrer ce dataframe pour enlever les lignes problématiques 

#On supprime les éléments qui n'ont pas de subdivision

a_filtrer <- a %>% 
  filter(!apply(., 1, function(row) any(duplicated(row))))


tab_ex2 <- T2_SP_EI %>% 
  mutate(is_secret_prim = NBRE_tot > 0 & NBRE_tot < 3)


hier_ape <- SSBtools::FindDimLists(a_filtrer)[[1]]
# 
# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(APE %in% corr_APE$APET)

hier_treff_et <- SSBtools::FindDimLists(tab_ex2_pr_gauss[,2])[[1]]
hier_dep_et <- SSBtools::FindDimLists(tab_ex2_pr_gauss[,3])[[1]]

tictoc::tic()
masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:3,
    freqVar = "NBRE_tot",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE,
    hierarchies = list(APE=hier_ape, TREFF_ET = hier_treff_et,DEP_ET = hier_dep_et)
  )
tictoc::toc()

str(masq_gauss)
tab_comptage_zéro <- masq_gauss[masq_gauss$NBRE_tot == 0, ]
masq_gauss <- anti_join(masq_gauss, tab_comptage_zéro, by = c("APE", "TREFF_ET","DEP_ET") )

Stat_masq_Gauss<-masq_gauss %>%
  group_by(primary,suppressed) %>%
  summarise(
    ncell_Gauss = n(),
    valcell_Gauss = sum(NBRE_tot)
  )
Stat_masq_Gauss<-Stat_masq_Gauss %>%
  mutate(Status_Gauss = case_when(
    primary ~ "B",
    suppressed ~"D",
    TRUE ~ "V"
  )) %>%
  mutate()


Stat_masq_Gauss <- Stat_masq_Gauss[,(ncol(Stat_masq_Gauss)-2):ncol(Stat_masq_Gauss)]
sums <- colSums(Stat_masq_Gauss[,1:2])
Stat_masq_Gauss[,1]<-(Stat_masq_Gauss[,1]*100)/sums[1]
Stat_masq_Gauss[,2]<-(Stat_masq_Gauss[,2]*100)/sums[2]
Stat_masq_Gauss <- Stat_masq_Gauss %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
sums <- c(sums,"Total")
Stat_masq_Gauss <- rbind(Stat_masq_Gauss,sums)
Stat_masq_Gauss <- Stat_masq_Gauss %>%
  select(Status_Gauss, everything())
Stat_masq_Gauss
Stat_masq_Gauss %>% knitr::kable(format = "latex", digits = 1)

stats_sous_tableaux <- unique(corr_APE$NAF_ag) %>% 
  purrr::map(
    \(naf_ag){
      filtre_APET <- corr_APE %>% filter(NAF_ag == naf_ag) %>% pull(APET)
      masq_gauss_extract <- masq_gauss %>% filter(APE %in% c(naf_ag, filtre_APET))
      Stat_masq_gauss_extract<-masq_gauss_extract %>%
        group_by(primary,suppressed) %>%
        summarise(
          ncell_Gauss = n(),
          valcell_Gauss = sum(NBRE_tot)
        )
      Stat_masq_gauss_extract<-Stat_masq_gauss_extract %>%
        mutate(Status_Gauss = case_when(
          primary ~ "B",
          suppressed ~"D",
          TRUE ~ "V"
        )) %>%
        mutate()
      Stat_masq_gauss_extract <- Stat_masq_gauss_extract[,(ncol(Stat_masq_gauss_extract)-2):ncol(Stat_masq_gauss_extract)]
      sums <- colSums(Stat_masq_gauss_extract[,1:2])
      Stat_masq_gauss_extract[,1]<-(Stat_masq_gauss_extract[,1]*100)/sums[1]
      Stat_masq_gauss_extract[,2]<-(Stat_masq_gauss_extract[,2]*100)/sums[2]
      Stat_masq_gauss_extract <- Stat_masq_gauss_extract %>%
        mutate(across(where(is.numeric), ~ round(., 2)))
      sums <- c(sums,"Total")
      Stat_masq_gauss_extract <- rbind(Stat_masq_gauss_extract,sums)
      Stat_masq_gauss_extract <- Stat_masq_gauss_extract %>%
        select(Status_Gauss, everything())
      print(Stat_masq_gauss_extract)
      Stat_masq_gauss_extract %>% knitr::kable(format = "latex", digits = 1)
    }
  )
names(stats_sous_tableaux) <- unique(corr_APE$NAF_ag)



