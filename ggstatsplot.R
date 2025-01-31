library(ggstatsplot)
library(ggplot2)
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
#Test ANOVA Welcha wykazał bardzo istotne statystycznie różnice między średnimi cen samochodów w zależności od rodzaju paliwa (p < 0.001).
#Współczynnik efektu jest bardzo wysoki (0.96), co oznacza, że rodzaj paliwa wyjaśnia prawie całą zmienność cen samochodów.
#Przedział ufności (0.96 – 1.00) wskazuje na bardzo silny efekt.
#Bardzo duża liczba obserwacji (n = 83,625) sprawia, że test jest bardzo wiarygodny.
#Ostatecznie można stwierdzić, że rodzaj paliwa silnie wpływa na cenę samochodu, i wynik ten jest bardzo pewny ze względu na ekstremalnie niski poziom p-wartości.

#korelacja przebieg i cena 
ggscatterstats(
data = dane,
x = mileage,
y = price_in_pln,
title = "Zależność między ceną a przebiegiem samochodu",
xlab = "Przebieg (km)",
ylab = "Cena (PLN)",
type ="pearson", 
  )

#Wykres przedstawia wyraźną, ujemną zależność między ceną samochodu a jego przebiegiem. Oznacza to, że im większy przebieg samochodu, tym niższa jest jego cena. Jest to intuicyjny wynik, ponieważ samochody z większym przebiegiem są zazwyczaj starsze i mają większy stopień zużycia, co obniża ich wartość rynkową.
#Współczynnik korelacji Pearsona: Mierzy siłę i kierunek liniowej zależności między dwiema zmiennymi. 
#W tym przypadku, wartość -0.42 oznacza umiarkowaną, ujemną korelację. To potwierdza, że wraz ze wzrostem przebiegu, cena samochodu ma tendencję do spadku

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
  marginal = TRUE,  # Dodaje histogramy po bokach
  bf.message = FALSE  # Ukrywa Bayes Factor
)

#Im nowszy samochód tym średnia cena również rośnie.
#Silna korelacja: Współczynnik korelacji Pearsona jest dodatni i bliski 1, co wskazuje na silną, dodatnią liniową zależność między ceną a rokiem produkcji.
#Istotność statystyczna: Bardzo niskie p-value (prawie równe 0) dla testu t-Studenta oznacza, że zaobserwowana zależność nie jest przypadkowa i istnieje wysokie prawdopodobieństwo, że w populacji ogólnej również występuje taka zależność
#Chociaż istnieje wyraźna tendencja wzrostowa, dane są dość rozproszone wokół linii trendu. Oznacza to, że na cenę samochodu wpływają również inne czynniki oprócz roku produkcji, takie jak marka, model, wyposażenie, stan techniczny itp.
#Współczynnik korelacji Pearsona: Mierzy siłę i kierunek liniowej zależności między dwiema zmiennymi. W tym przypadku, wartość 0.46 oznacza umiarkowaną, dodatnią korelację. To potwierdza, że wraz ze wzrostem roku produkcji, cena samochodu ma tendencję do wzrostu.