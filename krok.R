install.packages("readr")
install.packages("naniar")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("ISLR")


#Biblioteki
library(readr)
library(naniar)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 
library(hot.deck)

samochody_01
vis_miss(samochody_01, warn_large_data = FALSE)

library(mice)

help("mice")
# Imputacja braków danych za pomocą funkcji mice
imputed_data <- mice(samochody_01, method = "pmm", m = 5, maxit = 10, seed = 123)

# Pobranie pierwszego uzupełnionego zbioru danych
samochody_01_imputed <- complete(imputed_data, 1)

# Nadpisanie kolumny year w oryginalnym zbiorze
samochody_01$year <- samochody_01_imputed$year 

# Sprawdzenie czy braki zostały uzupełnione
vis_miss(samochody_01, warn_large_data = FALSE)
# porównanie starego " pozmianach1i nowego zbioru, gdzie poprawiona została kolumna year 
vis_miss(pozmianach1, warn_large_data = FALSE)

# Zastąpienie NA w kolumnie fuel_type
samochody_01$fuel_type <- ifelse(
  is.na(samochody_01$fuel_type) & samochody_01$engine_capacity == "Elektryczny",
  "Elektryczny",
  samochody_01$fuel_type
)
# efekty zastąpienia NA w elektrycznych autach
vis_miss(samochody_01, warn_large_data = FALSE)
# eliminowanie NA w hybrydach
samochody_01$fuel_type <- ifelse(
  is.na(samochody_01$fuel_type) & samochody_01$engine_capacity == "Hybryda",
  "Hybryda",
  samochody_01$fuel_type
)
vis_miss(samochody_01, warn_large_data = FALSE)


samochody_01$engine_capacity <- trimws(tolower(samochody_01$engine_capacity))

# Zastąpienie braków w fuel_type
samochody_01$fuel_type <- ifelse(
  is.na(samochody_01$fuel_type) & samochody_01$engine_capacity == "elektryczny",
  "Elektryczne",
  samochody_01$fuel_type
)
table(is.na(samochody_01$fuel_type))
# wykorzystanie polyreg
imputed_data <- mice(samochody_01, method = "polyreg", m = 5, maxit = 10, seed = 123)
samochody_01_imputed <- complete(imputed_data, 1)
samochody_01$fuel_type <- samochody_01_imputed$fuel_type
vis_miss(samochody_01, warn_large_data = FALSE)

#zastosowanie innej default method  logistic regression imputation (binary data, factor with 2 levels) polyreg
help(mice)
##
# Wyodrębnienie kolumn fuel_type i brand
samochody_fuel_brand <- samochody_01[, c("fuel_type", "brand"), drop = FALSE]

# Imputacja tylko dla kolumn fuel_type i brand
imputed_data <- mice(samochody_fuel_brand, method = c("polyreg", "polyreg"), m = 5, maxit = 10, seed = 123)

# Uzyskanie pierwszej imputacji
fuel_brand_imputed <- complete(imputed_data, 1)

# Zaktualizowanie oryginalnych kolumn fuel_type i brand w samochody_01
samochody_01$fuel_type <- fuel_brand_imputed$fuel_type
samochody_01$brand <- fuel_brand_imputed$brand

vis_miss(samochody_01, warn_large_data = FALSE)


