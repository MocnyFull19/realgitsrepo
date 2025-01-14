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

#Wgrywanie danych - samochody_new
samochody_new <- read_csv("samochody_new.csv")
view(samochody_new)

#Brakujace obserwacje
n_miss(samochody_new) #liczba brakujacych danych
n_complete(samochody_new) #liczba kompletynch obserwacji
prop_miss(samochody_new) #proporcja

#Wizualizacja brakujacych obserwacji
vis_miss(samochody_new, warn_large_data = FALSE)
#wizualizacja brakujacych danych
gg_miss_fct(samochody_new, fct = gearbox) 
#brakujace dane ze wzgledu na typ skrzyni biegow
gg_miss_upset(samochody_new, 
              nsets = 10)
#braki danych w roznych konfiguracjach

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
plot(violated)


###COPILOT START

# Funkcja do zamiany danych w kolumnach year i engine_capacity
fix_year_and_engine_capacity <- function(data) {
  # Sprawdź, które wartości w kolumnie year kończą się na "cm3"
  invalid_year <- grepl("cm3$", data$year)
  # Przenieś te wartości do kolumny engine_capacity
  data$engine_capacity[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA  # Ustaw wartość NA w kolumnie year
  
  # Sprawdź, które wartości w kolumnie engine_capacity są czterocyfrowymi liczbami większymi od 1900
  invalid_engine_capacity <- grepl("^[1-2][0-9]{3}$", data$engine_capacity) & as.numeric(data$engine_capacity) > 1900
  # Przenieś te wartości do kolumny year
  data$year[invalid_engine_capacity] <- data$engine_capacity[invalid_engine_capacity]
  data$engine_capacity[invalid_engine_capacity] <- NA  # Ustaw wartość NA w kolumnie engine_capacity
  
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_engine_capacity(samochody_new)

# Funkcja do zamiany danych w kolumnach year i fuel_type
fix_year_and_fuel_type <- function(data) {
  # Określenie wartości paliw
  fuel_types <- c("Elektryczny", "Benzyna", "Diesel", "Hybryda", "Benzyna+LPG")
  
  # Sprawdź, które wartości w kolumnie year odpowiadają typom paliw
  invalid_year <- data$year %in% fuel_types
  # Przenieś te wartości do kolumny fuel_type
  data$fuel_type[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA  # Ustaw wartość NA w kolumnie year
  
  # Sprawdź, które wartości w kolumnie fuel_type są czterocyfrowymi liczbami większymi od 1900
  invalid_fuel_type <- grepl("^[1-2][0-9]{3}$", data$fuel_type) & as.numeric(data$fuel_type) > 1900
  # Przenieś te wartości do kolumny year
  data$year[invalid_fuel_type] <- data$fuel_type[invalid_fuel_type]
  data$fuel_type[invalid_fuel_type] <- NA  # Ustaw wartość NA w kolumnie fuel_type
  
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_fuel_type(samochody_new)

# Funkcja do zamiany danych w kolumnach fuel_type i mileage
fix_fuel_type_and_mileage <- function(data) {
  # Sprawdź, które wartości w kolumnie fuel_type kończą się na "km"
  invalid_fuel_type <- grepl("km$", data$fuel_type)
  # Przenieś te wartości do kolumny mileage
  data$mileage[invalid_fuel_type] <- data$fuel_type[invalid_fuel_type]
  data$fuel_type[invalid_fuel_type] <- NA  # Ustaw wartość NA w kolumnie fuel_type
  
  # Określenie typów paliw
  fuel_types <- c("Elektryczny", "Benzyna", "Diesel", "Hybryda", "Benzyna+LPG")
  
  # Sprawdź, które wartości w kolumnie mileage odpowiadają typom paliw
  invalid_mileage <- data$mileage %in% fuel_types
  # Przenieś te wartości do kolumny fuel_type
  data$fuel_type[invalid_mileage] <- data$mileage[invalid_mileage]
  data$mileage[invalid_mileage] <- NA  # Ustaw wartość NA w kolumnie mileage
  
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_fuel_type_and_mileage(samochody_new)

unique(samochody_new$fuel_type)

# Funkcja do zamiany danych w kolumnach year i mileage
fix_year_and_mileage <- function(data) {
  # Sprawdź, które wartości w kolumnie year kończą się na "km"
  invalid_year <- grepl("km$", data$year)
  # Przenieś te wartości do kolumny mileage
  data$mileage[invalid_year] <- data$year[invalid_year]
  data$year[invalid_year] <- NA  # Ustaw wartość NA w kolumnie year
  
  # Sprawdź, które wartości w kolumnie mileage odpowiadają typom paliw
  invalid_mileage <- grepl("^[1-2][0-9]{3}$", data$mileage) & as.numeric(data$mileage) > 1900
  # Przenieś te wartości do kolumny year
  data$year[invalid_mileage] <- data$mileage[invalid_mileage]
  data$mileage[invalid_mileage] <- NA  # Ustaw wartość NA w kolumnie mileage
  
  return(data)
}

# Zastosuj funkcję do swojego zbioru danych
samochody_new <- fix_year_and_mileage(samochody_new)


###COPILOT KONIEC



write.csv(samochody_new, "pozmianach1.csv", row.names = FALSE)

#Wczytanie danych po zmianach
samochody_01 <- read_csv("pozmianach1.csv")
samochody_01

#Wizualizacja brakujacych obserwacji
vis_miss(samochody_01, warn_large_data = FALSE)


