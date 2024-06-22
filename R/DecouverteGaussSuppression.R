rm(list=ls())
#install.packages("GaussSuppression")
library(GaussSuppression)
z1 <- SSBtoolsData("z1")
z2 <- SSBtoolsData("z2")
z3 <- SSBtoolsData("z3")
# Ordinary suppressions
a <- GaussSuppressionFromData(z1, 1:2, 3, maxN = 5)
b <- GaussSuppressionFromData(z2, 1:4, 5, maxN = 1)
Z1 <- head(z1,16)
A <- GaussSuppressionFromData(Z1, 1:2, 3, maxN = 5)
library(tidyverse); library(curl); library(readxl)
Z1 <- Z1 %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
Z1
A <- A %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
A

a <- a %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
a
z1 <- z1 %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
z1


b <- b %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
b
z2 <- z2 %>% 
  pivot_wider(names_from = 'region', values_from = 'ant')
z2
print(z2, n=28)
print(b, n=28)
b

c <- GaussSuppression(z3)
c
