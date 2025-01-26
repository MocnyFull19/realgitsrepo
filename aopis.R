# Instalacja i załadowanie pakietu
install.packages("summarytools")
install.packages("GGally")
install.packages("dplyr")
install.packages("psych")
install.packages("summarytools")
install.packages("flextable")
install.packages("writexl")
library(writexl)
library(flextable)
library(summarytools)
library(ggplot2)
library(GGally)
library(dplyr)
library(psych)
library(summarytools)

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
library(dplyr)

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

write_xlsx(pelna_tabela, "podsumowanie_cen_samochodow_z_ogolem.xlsx")
