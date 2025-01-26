library(dplyr)
library(ggplot2)
library(stringr)

# przekształcenie
dane <- samochody_dowiz %>%
  mutate(
    mileage = mileage %>%
      str_remove_all("[^0-9]") %>%  
      as.numeric(),                 
    engine_capacity = engine_capacity %>%
      str_remove_all("[^0-9]") %>%  
      as.numeric()                  
  )

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
  scale_color_manual(values = rainbow(length(unique(samochody_avg$year_group)))) +
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



