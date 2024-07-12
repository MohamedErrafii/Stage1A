library(GaussSuppression)
library(SSBtools)
library(dplyr)
library(rtauargus)
library(sdcHierarchies)

ET_T2_AE <- readRDS(
  "X:/HAB-Traitement-Confidentialite/TESTS/Gauss/ET_T2_AE.rds"
)

# tab2_ET_AE <- tabulate_micro_data (
#   df = ET_T2_AE,
#   cat_vars = c("TREFF_ET", "DEP_ET"),
#   hrc_vars = list(APE = c("APET", "NAF_ag")),
#   resp_var = c("NBRE"),
#   marge_label = "T")

str(tab2_ET_AE)


tab2_ET_AE <- ET_T2_AE %>% 
  group_by(TREFF_ET, DEP_ET, APET, NAF_ag) %>% 
  summarise(nb_obs = n(), .groups = "drop") %>% 
  mutate(is_secret_prim = nb_obs > 0 & nb_obs < 3)


hier_activite <- SSBtools::FindDimLists(tab2_ET_AE[, 3:4])

masq <- GaussSuppressionFromData(
  tab2_ET_AE %>% select(-NAF_ag),
  # dimVar = c("TREFF_ET", "DEP_ET", "APET"),
  formula = ~TREFF_ET + DEP_ET + APET,
  freqVar = "nb_obs",
  hierarchies =  hier_activite,
  maxN = 2,
  protectZeros = FALSE,
  secondaryZeros = TRUE,
  output = "publish_inner_x"
)

str(masq)

