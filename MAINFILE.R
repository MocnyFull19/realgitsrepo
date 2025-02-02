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
install.packages("stringr")
install.packages("summarytools")
install.packages("GGally")
install.packages("dplyr")
install.packages("psych")
install.packages("summarytools")
install.packages("flextable")
install.packages("writexl")
install.packages("kableExtra")

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
library(stringr)
library(writexl)
library(flextable)
library(summarytools)
library(ggplot2)
library(GGally)
library(dplyr)
library(psych)
library(summarytools)
library(kableExtra)

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

vis_miss(samochody_new, warn_large_data = FALSE)

# Usunięcie wierszy z NA w kolumnie 'fuel_type'
samochody_dowiz <- samochody_new[!is.na(samochody_new$fuel_type), ]

#decyzja o usunięciu obserwacji z NA 
samochody_dowiz <- samochody_new[complete.cases(samochody_new), ]

vis_miss(samochody_dowiz, warn_large_data = FALSE)

# Zakonczenie data cleaning oraz wrangling. Tabela z czystymi danymi jest -> samochody_dowiz

str(samochody_dowiz)

# przekształcenie
dane <- samochody_dowiz %>%
  mutate(
    mileage = mileage %>%
      str_remove_all("[^0-9]") %>%  
      as.numeric(),                 
    engine_capacity = engine_capacity %>%
      str_remove_all("[^0-9]") %>%  
      as.numeric(),
    year = year %>%
      str_remove_all("[^0-9]") %>%  
      as.numeric()
  )


str(dane)

# Liczenie liczby wystąpień każdej marki
top_5_brands <- dane %>%
  count(brand) %>%
  top_n(5, n) %>%
  pull(brand)

# Filtrujemy dane, aby uwzględnić tylko te 5 marek
samochody_top_5_brands <- dane %>%
  filter(brand %in% top_5_brands)

# Wykres kołowy- typ paliwa
fuel_count <- dane %>%
  count(fuel_type)

ggplot(fuel_count, aes(x = "", y = n, fill = fuel_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Procentowy udział rodzaju paliwa", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Wykres liczby samochodów w zależności od roku produkcji
dane %>%
  ggplot(aes(x = year)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Liczba samochodów w zależności od roku produkcji",
    x = "Rok produkcji",
    y = "Liczba samochodów"
  )

# Wykres liczby samochodów(rodzaju paliwa) w zależności od wojewodztwa
dane %>%
  group_by(voivodeship) %>%
  filter(n() > 30) %>%
  ungroup() %>%
  ggplot(aes(x = voivodeship, fill = fuel_type)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(
    title = "Rodzaj paliwa w województwach z więcej niż 10 samochodami",
    x = "Województwo",
    y = "Liczba samochodów",
    fill = "Rodzaj paliwa"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#Wykres liniowy (line plot) - Trend cen samochodów w zależności od roku produkcji
dane %>%
  group_by(year) %>%
  summarise(mean_price = mean(price_in_pln, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_price)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Trend cen samochodów w zależności od roku produkcji",
    x = "Rok produkcji",
    y = "Średnia cena (PLN)"
  )

# Wykres średniej ceny w zależności od rodzaju paliwa
dane %>%
  group_by(fuel_type) %>%
  summarise(mean_price = mean(price_in_pln, na.rm = TRUE)) %>%
  ggplot(aes(x = fuel_type, y = mean_price, fill = fuel_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Średnia cena samochodów w zależności od rodzaju paliwa",
    x = "Rodzaj paliwa",
    y = "Średnia cena (PLN)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#Top 10 najczęstszych marek samochodów
dane %>%
  count(brand) %>%
  arrange(desc(n)) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(brand, -n), y = n, fill = brand)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Top 10 najczęstszych marek samochodów",
    x = "Marka",
    y = "Liczba wystąpień"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Wykres cena vs przebieg w zaleznosci od roku produkcji
dane <- dane %>%
  mutate(year_group = case_when(
    year >= 1995 & year <= 2000 ~ "1995-2000",
    year >= 2001 & year <= 2005 ~ "2001-2005",
    year >= 2006 & year <= 2010 ~ "2006-2010",
    year >= 2011 & year <= 2015 ~ "2011-2015",
    year >= 2016 & year <= 2020 ~ "2016-2020",
    year >= 2021 & year <= 2023 ~ "2021-2023",
    TRUE ~ "Other"
  ))

dane_avg <- dane %>%
  group_by(year_group) %>%
  summarise(
    avg_mileage = mean(mileage, na.rm = TRUE),
    avg_price_in_pln = mean(price_in_pln, na.rm = TRUE)
  )

ggplot(dane_avg, aes(x = avg_mileage, y = avg_price_in_pln, color = year_group)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Cena vs. Przebieg w zależności od przedziału rocznego", x = "Średni przebieg (km)", y = "Średnia cena (PLN)") +
  theme_minimal() +
  scale_color_manual(values = rainbow(length(unique(dane_avg$year_group)))) +
  theme(legend.title = element_blank())

# Wykres średniej ceny i przebiegu w zależności od marki samochodu i grupy lat produkcji
samochody_top_5_brands$year_group <- cut(samochody_top_5_brands$year,
                                         breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2023),
                                         labels = c("1995-2000", "2001-2005", "2006-2010", "2011-2015", "2016-2020", "2021-2023"),
                                         right = TRUE)

samochody_avg_top_5_year <- samochody_top_5_brands %>%
  group_by(brand, year_group) %>%
  summarise(
    avg_mileage = mean(mileage, na.rm = TRUE),
    avg_price_in_pln = mean(price_in_pln, na.rm = TRUE)
  )

ggplot(samochody_avg_top_5_year, aes(x = avg_mileage, y = avg_price_in_pln, color = brand, shape = year_group)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Średnia cena vs. Przebieg dla 5 najczęściej występujących marek (z podziałem na lata produkcji)", 
       x = "Średni przebieg (km)", 
       y = "Średnia cena (PLN)", 
       color = "Marka samochodu",
       shape = "Grupa lat produkcji") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Poczatek statystyki opisowej - obowiazujaca tabela - dane

# Podsumowanie danych
summary_stats <- dfSummary(dane)
print(summary_stats)

#HTML
print(summary_stats, method = "browser", file = "report.html")


ggcorr(
  data = dane %>% select(where(is.numeric)),
  method = c("pairwise.complete.obs" , "pearson"),
  label = TRUE
) +
  theme(
    axis.text.x = element_text(hjust = 1), 
    axis.text.y = element_text(hjust = 1)
  )

var(dane$price_in_pln) #variance
coeff_var<-sd(dane$price_in_pln)/mean(dane$price_in_pln) #coefficient of variability %
coeff_var
IQR(dane$price_in_pln)# difference between quartiles =Q3-Q1 
sx<-IQR(dane$price_in_pln)/2  #interquartile deviation
coeff_varx<-sx/median(dane$price_in_pln) #IQR coefficient of variability %
coeff_varx
min(dane$price_in_pln)
max(dane$price_in_pln)
quantile(dane$price_in_pln,probs=c(0,0.1,0.25,0.5,0.75,0.95,1),na.rm=TRUE)

# Grupowanie danych po 'brand' i obliczanie statystyk dla każdej grupy

analiza_df <- dane %>%
  group_by(brand) %>%
  summarise(
    Min = min(price_in_pln, na.rm = TRUE),
    Max = max(price_in_pln, na.rm = TRUE),
    `Kwartyl dolny` = quantile(price_in_pln, 0.25, na.rm = TRUE),
    Mediana = round(median(price_in_pln, na.rm = TRUE), 2),
    `Kwartyl górny` = quantile(price_in_pln, 0.75, na.rm = TRUE),
    Średnia = round(mean(price_in_pln, na.rm = TRUE), 2),
    `Odch. std.` = round(sd(price_in_pln, na.rm = TRUE), 2),
    IQR = round(IQR(price_in_pln, na.rm = TRUE), 2),
    `Odchylenie ćwiartkowe` = round(IQR(price_in_pln, na.rm = TRUE) / 2, 2),
    `Skośność` = round(skew(price_in_pln, na.rm = TRUE), 2),
    `Kurtoza` = round(kurtosi(price_in_pln, na.rm = TRUE), 2),
    .groups = "drop"
  )

ogolne_podsumowanie <- dane %>%
  summarise(
    brand = "Ogólnie",
    Min = min(price_in_pln, na.rm = TRUE),
    Max = max(price_in_pln, na.rm = TRUE),
    `Kwartyl dolny` = quantile(price_in_pln, 0.25, na.rm = TRUE),
    Mediana = round(median(price_in_pln, na.rm = TRUE), 2),
    `Kwartyl górny` = quantile(price_in_pln, 0.75, na.rm = TRUE),
    Średnia = round(mean(price_in_pln, na.rm = TRUE), 2),
    `Odch. std.` = round(sd(price_in_pln, na.rm = TRUE), 2),
    IQR = round(IQR(price_in_pln, na.rm = TRUE), 2),
    `Odchylenie ćwiartkowe` = round(IQR(price_in_pln, na.rm = TRUE) / 2, 2),
    `Skośność` = round(skew(price_in_pln, na.rm = TRUE), 2),
    `Kurtoza` = round(kurtosi(price_in_pln, na.rm = TRUE), 2)
  )

pelna_tabela <- bind_rows(ogolne_podsumowanie, analiza_df)

kable(pelna_tabela, 
      col.names = c("Marka", "Min", "Max", "Kwartyl dolny", "Mediana", 
                    "Kwartyl górny", "Średnia", "Odch. std.", "IQR", 
                    "Odchylenie ćwiartkowe", "Skośność", "Kurtoza"),
      caption = "Podsumowanie cen samochodów według marki (w tym ogólnie)")

write_xlsx(pelna_tabela, "Analiza_opisowa_wyniki.xlsx")

