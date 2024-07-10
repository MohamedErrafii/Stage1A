install.packages("remotes")
remotes::install_github(
  "InseeFrLab/rtauargus",
  build_vignettes = FALSE,
  upgrade = "never"
)

library(rtauargus)

data("indiv_dt") 
tab0 <- indiv_dt %>% count(SIZE, CJ, TYPE) %>% 
  mutate(is_primary = n > 0 & n < 3)
count(tab0, is_primary)

tab1 <- indiv_dt %>% count(A10, A21, A88) %>% 
  mutate(is_primary = n > 0 & n < 3)
count(tab1, is_primary)

tab2 <- indiv_dt %>% count(A10, A21, A88, SIZE) %>% 
  mutate(is_primary = n > 0 & n < 3)
count(tab2, is_primary)

tab3 <- indiv_dt %>% count(A10, A21, A88, SIZE, CJ) %>% 
  mutate(is_primary = n > 0 & n < 3)
count(tab3, is_primary)

tab4 <- indiv_dt %>% count(A10, A21, A88, SIZE, CJ, TYPE) %>% 
  mutate(is_primary = n > 0 & n < 3)
count(tab4, is_primary)

# POur générer la hiérarchie entre les variables A10, A21, A88
SSBtools::FindDimLists(tab1[,1:3])


# En complément
str(datatest1)
str(datatest2)
