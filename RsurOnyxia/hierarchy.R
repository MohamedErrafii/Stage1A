#hierarchy comment lui signaler la hierarchcy  # regarder model matrix et hierarchy2modelmatrix pour commencer 

install.packages("GaussSuppression")
library("GaussSuppression")
library("SSBtools")
library(tidyverse)
library(curl)
library(readxl)
rm(list =ls())


#On reprends le tableau utilisé dans le diapo (diapo 109 de la présentation)

#df <- data.frame(values = c(400, 46,21, 2, 23, 191, 32, 54, 67, 38, 80,27, 41, 12, 83,44, 39), 
#                var1 = c("Pays", "Nord", "N1", "N2", "N3","Ouest","O1", "O2", "O3", "O4", "Est","E1", "E2", "E3", "Sud","S1", "S2"), stringsAsFactors = FALSE)
#df<- data.frame(values = c(46, 21, 2, 23 ), 
#               var1 = c( "Nord",  "N1", "N2", "N3"), stringsAsFactors = FALSE)


df <- data.frame(values = c(21, 2, 23, 32, 54, 67, 38,27, 41, 12,44, 39), 
                 var1 = c("N1", "N2", "N3","O1", "O2", "O3", "O4","E1", "E2", "E3", "S1", "S2"), stringsAsFactors = FALSE)


#geoFormula <- c("Nord = N1+ N2 + N3", "Ouest = O1 + O2 + O3 + O4", "Est = E1 + E2 +E3", "Sud = S1 + S2", "Pays = Nord + Ouest + Est + Sud")
#geoFormula <- c("Nord = N1+ N2 + N3")

#georHier <- Formula2Hierarchy(geoFormula)

georHier <- data.frame(levels = c("@","@@","@@@","@@@","@@@","@@","@@@","@@@","@@@","@@@","@@","@@@","@@@","@@@","@@","@@@","@@@"),
                       codes = c("Pays", "Nord", "N1", "N2", "N3","Ouest","O1", "O2", "O3", "O4", "Est","E1", "E2", "E3", "Sud","S1", "S2"))

#h1 <- AutoHierarchies(list(geo = geoFormula), data = df)


df2 <- GaussSuppressionFromData(df, freqVar = "values", hierarchies = list(var1 = georHier), formula = values ~ var1)

#le code marche et on cherche maintenant une fonction qui va chercher automatiquement les hierarchies au lieu de les écrires la main











































































# First, create different types of input
z <- SSBtoolsData("sprt_emp_withEU")
z
yearFormula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
yearFormula
yearHier <- Formula2Hierarchy(yearFormula)
yearHier
geoDimList <- FindDimLists(z[, c("eu","geo")], total = "Europe")[[1]]
geoDimList
geoDimList2 <- FindDimLists(z[, c("geo", "eu")])[[1]]
geoDimList2
geoHrc <- DimList2Hrc(geoDimList)
geoHrc
ageHier <- SSBtoolsData("sprt_emp_ageHier")
ageHier

h1 <- AutoHierarchies(list(age = ageHier, geo = geoDimList, year = yearFormula))
h1

h2 <- AutoHierarchies(list(age = "Y15-64", geo = geoHrc, year = yearHier), data = z, 
                      total = "Europe")
h2

h3 <- AutoHierarchies(list(age = "Total", geo = geoDimList2, year = "Total"), data = z)
h3
h4 <- FindHierarchies(z[, c(1, 2, 3, 5)])
h4
h5 <- AutoHierarchies(list(age = "Total", geo = "", year = "colFactor"), data = z)
h5
identical(h1, h2)
identical(h3, h4)

# Print the resulting hierarchies
h1 # = h2
h3 # = h4
h5

a<-FindHierarchies(z[, c("geo", "eu", "age")])
a

# ===================================================================== 
#   Examples illustrating the combineHierarchies parameter
# =====================================================================

# First, create data
d <- SSBtoolsData("d2ws")[1:3]
d$isCounty1 <- "NO"
d$isCounty1[d$county == "county-1"] <- "YES"
d

# sdcTable coding showing two tree-shaped hierarchies
dimList <- FindDimLists(d)
dimList

# Two tree-shaped hierarchies can still be seen 
# Hierarchies with three and two levels
hA <- AutoHierarchies(dimList, combineHierarchies = FALSE)
hA

# A single hierarchy with only one level 
# Contains the information needed to create a dummy matrix
hB <- AutoHierarchies(dimList)
hB

# Dummy matrices from the hierarchies
DummyHierarchies(hA)
DummyHierarchies(hB)


# ===================================================================== 
#   Special examples with character vector(s) as unnamed list elements
# =====================================================================

# Same output as FindHierarchies above
AutoHierarchies(list(c("geo", "eu", "age")), data = z)

# Now combined with a named list element 
AutoHierarchies(list(year = yearHier, c("geo", "eu", "age")), data = z)

# Total codes by unnamed list element as named character vector 
AutoHierarchies(list(year = yearHier, c(Europe = "geo", "eu", All = "age")), data = z)

# Two types of year input. Total codes by using the parameter `total`. 
AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
                total = c("allYears", "unused", "Tot"))

# Avoid combineHierarchies to see effect of each year input separately 
# (even earlier return possible with `combineHierarchies = NA`)
AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
                total = c("allYears", "unused", "Tot"), combineHierarchies = FALSE)


###################################
###################################
###################################
















