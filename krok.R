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
#####

# Usunięcie wierszy z NA w kolumnie 'fuel_type'
samochody_01_cleaned <- samochody_01[!is.na(samochody_01$fuel_type), ]

# Wyświetlenie oczyszczonej ramki danych
print(samochody_01_cleaned)
vis_miss(samochody_01_cleaned, warn_large_data = FALSE)

#decyzja o usunięciu obserwacji z NA 
samochody_dowiz <- samochody_01[complete.cases(samochody_01), ]

# Wyświetlenie oczyszczonej ramki danych
print(samochody_dowiz)
vis_miss(samochody_dowiz, warn_large_data = FALSE)
