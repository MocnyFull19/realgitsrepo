#Instalacja pakietów
install.packages("readr")
install.packages("naniar")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("ISLR")
install.packages("hot.deck")
install.packages("mice")

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
library(readr)
library(visdat)
library(hot.deck)
library(mice)

#Wgrywanie danych - samochody_new
samochody_new <- read_csv("samochody_new.csv")

# Podgląd danych
head(samochody_new)
str(samochody_new)

#Brakujace obserwacje
n_miss(samochody_new) #liczba brakujacych danych
n_complete(samochody_new) #liczba kompletynch obserwacji
prop_miss(samochody_new) #proporcja
# Sprawdzanie brakujących wartości
samochody_new %>% 
  miss_case_table()

# Wizualizacja braków danych

#Wizualizacja brakujacych obserwacji
vis_miss(samochody_new, warn_large_data = FALSE)
#wizualizacja brakujacych danych
gg_miss_fct(samochody_new, fct = gearbox) 
#brakujace dane ze wzgledu na typ skrzyni biegow
gg_miss_upset(samochody_new, 
              nsets = 10)

#Zastapienie innych wartosci specjalnych na NA
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

sapply(samochody_new, is.special)
#Dodanie warunkow do danych
unique(samochody_new$gearbox)
RULE <- editset(c("year >= 1900","gearbox %in% c('manual','automatic')"
                  , "price_in_pln > 0"))
violated <- violatedEdits(RULE, samochody_new)

# Funkcja do zamiany danych w kolumnach year i engine_capacity
fix_year_and_engine_capacity <- function(data) {
  invalid_year <- grepl("cm3$", data$year)
  data$engine_capacity[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA
  invalid_engine_capacity <- grepl("^[1-2][0-9]{3}$", data$engine_capacity) & 
    as.numeric(data$engine_capacity) > 1900
  data$year[invalid_engine_capacity] <- data$engine_capacity[invalid_engine_capacity]
  data$engine_capacity[invalid_engine_capacity] <- NA
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_engine_capacity(samochody_new)

# Funkcja do zamiany danych w kolumnach year i fuel_type
fix_year_and_fuel_type <- function(data) {
  fuel_types <- c("Elektryczny", "Benzyna", "Diesel", "Hybryda", "Benzyna+LPG")
  invalid_year <- data$year %in% fuel_types
  data$fuel_type[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA
  invalid_fuel_type <- grepl("^[1-2][0-9]{3}$", data$fuel_type) & 
    as.numeric(data$fuel_type) > 1900
  data$year[invalid_fuel_type] <- data$fuel_type[invalid_fuel_type]
  data$fuel_type[invalid_fuel_type] <- NA
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_fuel_type(samochody_new)

# Funkcja do zamiany danych w kolumnach fuel_type i mileage
fix_fuel_type_and_mileage <- function(data) {
  invalid_fuel_type <- grepl("km$", data$fuel_type)
  data$mileage[invalid_fuel_type] <- data$fuel_type[invalid_fuel_type]
  data$fuel_type[invalid_fuel_type] <- NA
  fuel_types <- c("Elektryczny", "Benzyna", "Diesel", "Hybryda", "Benzyna+LPG")
  invalid_mileage <- data$mileage %in% fuel_types
  data$fuel_type[invalid_mileage] <- data$mileage[invalid_mileage]
  data$mileage[invalid_mileage] <- NA
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_fuel_type_and_mileage(samochody_new)

# Funkcja do zamiany danych w kolumnach year i mileage
fix_year_and_mileage <- function(data) {
  invalid_year <- grepl("km$", data$year)
  data$mileage[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA
  invalid_mileage <- grepl("^[1-2][0-9]{3}$", data$mileage) & 
    as.numeric(data$mileage) > 1900
  data$year[invalid_mileage] <- data$mileage[invalid_mileage]
  data$mileage[invalid_mileage] <- NA
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_mileage(samochody_new)

# Wizualizacja brakujących danych
vis_miss(samochody_new, warn_large_data = FALSE)

#Imputacja braków danych za pomocą funkcji mice 
imputed_data <- mice(samochody_new, method = "pmm", m = 5, maxit = 10, seed = 123)

# Pobranie pierwszego uzupełnionego zbioru danych
samochody_new_imputed <- complete(imputed_data, 1)

# Nadpisanie kolumny year w oryginalnym zbiorze
samochody_new$year <- samochody_new_imputed$year 

# Sprawdzenie czy braki zostały uzupełnione
vis_miss(samochody_new, warn_large_data = FALSE)

# Zastąpienie NA w kolumnie fuel_type
samochody_new$fuel_type <- ifelse(
  is.na(samochody_new$fuel_type) & samochody_new$engine_capacity == "Elektryczny",
  "Elektryczny",
  samochody_new$fuel_type
)
# efekty zastąpienia NA w elektrycznych autach
vis_miss(samochody_new, warn_large_data = FALSE)
# eliminowanie NA w hybrydach
samochody_new$fuel_type <- ifelse(
  is.na(samochody_new$fuel_type) & samochody_new$engine_capacity == "Hybryda",
  "Hybryda",
  samochody_new$fuel_type
)
vis_miss(samochody_new, warn_large_data = FALSE)


samochody_new$engine_capacity <- trimws(tolower(samochody_new$engine_capacity))

# Zastąpienie braków w fuel_type
samochody_new$fuel_type <- ifelse(
  is.na(samochody_new$fuel_type) & samochody_new$engine_capacity == "elektryczny",
  "Elektryczne",
  samochody_new$fuel_type
)
table(is.na(samochody_new$fuel_type))
##
# Wyodrębnienie kolumn fuel_type i brand
samochody_fuel_brand <- samochody_new[, c("fuel_type", "brand"), drop = FALSE]

# Imputacja tylko dla kolumn fuel_type i brand
imputed_data <- mice(samochody_fuel_brand, method = c("polyreg", "polyreg"), m = 5, maxit = 10, seed = 123)

# Uzyskanie pierwszej imputacji
fuel_brand_imputed <- complete(imputed_data, 1)

# Zaktualizowanie oryginalnych kolumn fuel_type i brand w samochody_new
samochody_new$fuel_type <- fuel_brand_imputed$fuel_type
samochody_new$brand <- fuel_brand_imputed$brand

vis_miss(samochody_new_, warn_large_data = FALSE)

# Usunięcie wierszy z NA w kolumnie 'fuel_type'
samochody_dowiz <- samochody_new[!is.na(samochody_new$fuel_type), ]

#decyzja o usunięciu obserwacji z NA 
samochody_dowiz <- samochody_new[complete.cases(samochody_new), ]

vis_miss(samochody_dowiz, warn_large_data = FALSE)

# Zakonczenie data cleaning oraz wrangling. Tabela z czystymi danymi jest -> samochody_dowiz

