rm(list=ls())

# install.packages("rtauargus", repos = "https://nexus.insee.fr/repository/r-public",  type="source")
# install.packages("GaussSuppression")
# install.packages("tidyverse")
# install.packages("curl")
# install.packages("readxl")
# install.packages("tictoc")

library(GaussSuppression)
library(dplyr)
library(rtauargus)
library(SSBtools)
library(tidyverse)
library(curl)
library(readxl)
library(tictoc)

source("RsurAUS/fonction_comparaison_masque.R")

loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus_4.2.5.TEST/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

data("datatest1")
str(datatest1)

tab_ex2 <- datatest1 %>% 
  mutate(is_secret_prim = nb_obs > 0 & nb_obs < 3)

# 
# Pose du secret avec GAUSS ----------------------------
tab_ex2_pr_gauss <-  tab_ex2 %>% 
  filter(treff != "Total") %>% 
  filter(cj != "Total") %>% 
  filter(type_distrib != "Total") %>% 
  filter(A10 != "Total") %>%
  select(-pizzas_tot, -pizzas_max, -is_secret_freq, -is_secret_dom, -nb_obs_rnd, -pizzas_tot_abs)

tictoc::tic()
masq_gauss <- tab_ex2_pr_gauss %>% 
  GaussSuppressionFromData(
    dimVar = 1:2,
    freqVar = "nb_obs",
    maxN = 2,
    protectZeros = FALSE, 
    secondaryZeros = TRUE
  )
tictoc::toc()

str(masq_gauss)

#masq tau argus 

totcode <- c(treff ="Total",cj ="Total", A10 = "Total", type_distrib ="Total")
explanatory_vars<- names(totcode)
datatest1_with_prim <- datatest1 %>%
  mutate(
    is_secret_freq = (nb_obs > 0 & nb_obs < 3),
    is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot),
    pizzas_tot= round(abs(pizzas_tot),2)
  ) %>%
  mutate(
    is_secret_prim =  is_secret_freq ,
    nb_obs = ceiling(nb_obs)
  )
tictoc::tic()
res_wo_split <- tab_rtauargus(
  tabular = datatest1_with_prim,
  files_name = "datatest1",
  dir_name = "example_1/wo_split",
  explanatory_vars = explanatory_vars,
  totcode = totcode,
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = TRUE,
  split_tab = FALSE
)
tictoc::toc()






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
  select(NUTS, CJ, nb_obs, Status_Gauss) %>% 
  full_join(
    masq_ta %>% select(NUTS, CJ, nb_obs, Status),
    by = c("NUTS", "CJ", "nb_obs")
  ) %>% 
  mutate(
    Status = ifelse(nb_obs == 0, "V", Status)
  ) %>% 
  mutate(diff_status = Status_Gauss != Status) %>% 
  filter(diff_status)

#ce code nous donne les cellules où le flag n'est pas le même, on va s'intéresser aux celles ci de plus près 
#pour savoir pourquoi le secret à été poser différemment
