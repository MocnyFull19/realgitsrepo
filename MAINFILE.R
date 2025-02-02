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
install.packages("ggstatsplot")

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
library(ggstatsplot)


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

# Wnioskowanie statystyczne - obowiazujace tabela - dane

ggbetweenstats(
  data = dane,
  x = fuel_type,
  y = price_in_pln,
  type = "parametric", 
  title = "Porównanie średnich cen samochodów w zależności od rodzaju paliwa",
  xlab = "Rodzaj paliwa",
  ylab = "Cena (PLN)",
  messages = FALSE
)
###Wyniki testów statystycznych sugerują, że różnice w średnich cenach między poszczególnymi kategoriami paliw są statystycznie istotne. Oznacza to, że z dużą pewnością możemy stwierdzić, że samochody elektryczne są średnio droższe niż samochody benzynowe, hybrydowe itp.
#Test  Welcha wykazał bardzo istotne statystycznie różnice między średnimi cen samochodów w zależności od rodzaju paliwa (p < 0.001).
#Współczynnik efektu jest bardzo wysoki (0.96), co oznacza, że rodzaj paliwa wyjaśnia prawie całą zmienność cen samochodów.
#Przedział ufności (0.96 – 1.00) wskazuje na bardzo silny efekt.
#Bardzo duża liczba obserwacji (n = 83,625) sprawia, że test jest bardzo wiarygodny.
#Ostatecznie można stwierdzić, że rodzaj paliwa silnie wpływa na cenę samochodu, i wynik ten jest bardzo pewny ze względu na ekstremalnie niski poziom p-wartości.

##korelacja przebieg i cena 
# Wizualizacja danych
plot(dane$mileage, dane$price_in_pln,
     xlab = "Przebieg (km)",
     ylab = "Cena (PLN)",
     main = "Zależność między ceną a przebiegiem samochodu")
#istnieje negatywna korelacja między przebiegiem a ceną. Oznacza to, że im większy przebieg samochodu, tym niższa jest jego cena. 
# Obliczenie korelacji Spearmana
cor.test(dane$mileage, dane$price_in_pln, method = "spearman")
#Wartość współczynnika korelacji Spearmana wynosi -0.59.Wartość -0.59 wskazuje na relatywnie silną zależność, ale nie jest to korelacja bardzo silna
#p-value < 2.2e-16: P-wartość jest bardzo mała (mniejsza niż 0.05), co oznacza, że wynik jest statystycznie istotny. Możemy odrzucić hipotezę zerową (że nie ma korelacji) na poziomie istotności 0.05.

#Test Kruskala-Wallisa
kruskal.test(price_in_pln ~ mileage, data = dane)
#Kruskal-Wallis chi-squared = 47625: Jest to statystyka testu Kruskala-Wallisa. Jest to miara różnic między grupami. Wartość 47625 wskazuje, że różnice między grupami (w tym przypadku cenami samochodów w zależności od przebiegu) są dość duże.
#df = 14096: Liczba stopni swobody. Stopnie swobody w testach statystycznych Kruskala-Wallisa są obliczane jako liczba grup minus 1. Jest to informacja o liczbie porównań, które zostały uwzględnione w teście.
#p-value < 2.2e-16: P-wartość jest mniejsza niż 2.2e-16, co oznacza, że jest to wartość ekstremalnie mała. Jest to wynik statystycznie bardzo istotny.

#3 wykres 
png("marka_a_cena.png", width = 1600, height = 1200, res = 300)

samochody_top_5_brands <- na.omit(samochody_top_5_brands)  # Usuń brakujące dane

ggbetweenstats(
  data = samochody_top_5_brands,
  x = brand,
  y = price_in_pln,
  title = "zależność ceny od marki samochodu",
  xlab = "Marka",
  ylab = "Cena (PLN)",
  type = "np",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.param = "median",
  centrality.para.method = "nonparametric",
  ggtheme = theme_bw(),
  outlier.tagging = FALSE, # Usuwa wartości odstające
  messages = FALSE
) +
  scale_y_log10(limits = c(1000, 1e6)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.position = "bottom"
  )

dev.off()


#Wykres przedstawia porównanie średnich cen samochodów różnych marek.

#Główne wnioski:
# Różnice w cenach między markami: Wykres wyraźnie pokazuje, że średnie ceny samochodów różnią się w zależności od marki.
#Najdroższe marki: BMW i Mercedes-Benz mają najwyższe średnie ceny.
#Najtańsze marki: Opel, Peugeot i Volkswagen mają niższe średnie ceny w porównaniu do BMW i Mercedes-Benz.
#Ordinal = 0.32, CI95% [0.31, 1.00]: Te wartości związane są z miarą efektu, która mówi nam o wielkości różnic między grupami. W tym przypadku, wartość ordinal wynosi 0.32, co sugeruje umiarkowaną różnicę między grupami. Przedział ufności 95% [0.31, 1.00] oznacza, że jesteśmy pewni w 95%, że prawdziwa wartość efektu znajduje się w tym przedziale.
#Test Kruskala-Wallisa: Test ten wykazał, że różnice w cenach samochodów różnych marek są statystycznie istotne (p < 0.001). Oznacza to, że z dużym prawdopodobieństwem możemy stwierdzić, że ceny samochodów różnią się w zależności od marki.


#4
png("histogram.png", width = 1600, height = 1200, res = 300)
gghistostats(
  data = dane,
  x = price_in_pln,
  title = "Histogram cen samochodów z testem normalności",
  xlab = "Cena (PLN)",
  test.value = 0,
  type = "parametric"
) +
  scale_x_continuous(
    limits = c(0, 1000000),
    breaks = seq(0, 1000000, by = 200000),  # Etykiety co 200k
    labels = scales::comma
  )
dev.off()
#Ceny samochodów są zróżnicowane: Większość samochodów ma relatywnie niską cenę, ale istnieje też grupa samochodów bardzo drogich, co powoduje, że rozkład jest prawostronnie asymetryczny.
#Rozkład cen nie jest normalny: Dane nie spełniają założeń testu t-Studenta, co oznacza, że nie możemy bezpośrednio stosować testów statystycznych opartych na rozkładzie normalnym
#Histogram jest silnie asymetryczny, z długim ogonem po prawej stronie. Oznacza to, że większość samochodów ma ceny w niższych przedziałach, a tylko nieliczne osiągają bardzo wysokie ceny.
# przedział ufności dla średniej ceny samochodu z populacji wynosi znajduje się między 75 931.13 PLN a 77 270.43 PLN. Oznacza to, że mamy 95% pewności, że prawdziwa średnia cena samochodu w populacji ogólnej mieści się w tym przedziale.
5. 
ggscatterstats(
  data = dane,
  x = year,
  y = price_in_pln,
  title = "Zależność ceny od roku produkcji samochodu",
  xlab = "Rok produkcji",
  ylab = "Cena (PLN)",
  type = "np",  # Zastosowanie testu Kruskala-Wallisa (non-parametric)
  marginal = TRUE,  # Dodaje histogramy po bokach
  bf.message = FALSE  # Ukrywa Bayes Factor
)
#Współczynnik korelacji Spearmana (ρ = 0.77): Wskazuje na silną, dodatnią korelację między rokiem produkcji a ceną samochodu. Oznacza to, że im nowszy samochód, tym większa szansa, że będzie droższy.
#Przedział ufności 95% dla ρ: Potwierdza, że korelacja jest istotna statystycznie.
#Wartość p (p = 0.00): Potwierdza, że korelacja jest istotna statystycznie (p < 0.05).
#Interpretacja przedstawionego wykresu powinna skupić się na kilku kluczowych aspektach, które pozwolą na lepsze zrozumienie zależności między ceną samochodu a jego rokiem produkcji. Oto one:

#1. Wykres punktowy (Scatter plot):

#  Trend: Na wykresie widzimy wyraźny trend wzrostowy - im nowszy jest samochód (im wyższy rok produkcji), tym wyższa jest jego cena. To zjawisko jest zgodne z oczekiwaniami - nowsze modele są zazwyczaj droższe od starszych.
#Rozproszenie punktów: Możemy zaobserwować, że punkty na wykresie są dość rozproszone, co oznacza, że cena samochodu nie zależy tylko od roku produkcji. Wpływ na nią mają również inne czynniki, takie jak marka, model, stan techniczny, wyposażenie itp.
#Punkty odstające: Na wykresie widać kilka punktów, które znacznie odbiegają od głównego trendu. Mogą to być np. luksusowe modele lub samochody kolekcjonerskie, których cena jest znacznie wyższa niż przeciętna dla danego roku produkcji.
#2. Histogramy brzegowe:

# Histogram na osi X (Rok produkcji): Pokazuje rozkład liczby samochodów w zależności od roku produkcji. Widzimy, że najwięcej samochodów w tym zbiorze danych pochodzi z lat 2010-2020.
#3Histogram na osi Y (Cena): Pokazuje rozkład cen samochodów. Widzimy, że większość samochodów ma cenę poniżej 1 miliona PLN, a tylko nieliczne osiągają ceny powyżej 2 milionów PLN.
#3. Informacje statystyczne:

# Współczynnik korelacji Spearmana (ρ = 0.77): Wskazuje na silną, dodatnią korelację między rokiem produkcji a ceną samochodu. Oznacza to, że im nowszy samochód, tym większa szansa, że będzie droższy.
#Przedział ufności 95% dla ρ: Potwierdza, że korelacja jest istotna statystycznie.
#Wartość p (p = 0.00): Potwierdza, że korelacja jest istotna statystycznie (p < 0.05).
#Podsumowanie:

# Z wykresu i danych statystycznych wynika, że istnieje silna, dodatnia korelacja między rokiem produkcji a ceną samochodu. Nowsze samochody są zazwyczaj droższe, ale na cenę wpływają również inne czynniki, co widać po rozproszeniu punktów na wykresie. Histogramy brzegowe pozwalają na lepsze zrozumienie rozkładu danych dla roku produkcji i ceny.

#Dodatkowe informacje:
#Warto zauważyć, że przedstawiona analiza dotyczy konkretnego zbioru danych. Wyniki mogą się różnić dla innych zbiorów danych.


x
