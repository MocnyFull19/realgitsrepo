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
#Test  Welcha wykazał bardzo istotne statystycznie różnice między średnimi cen samochodów w zależności od rodzaju paliwa (p < 0.001).
#Współczynnik efektu jest bardzo wysoki (0.96), co oznacza, że rodzaj paliwa wyjaśnia prawie całą zmienność cen samochodów.
#Przedział ufności (0.96 – 1.00) wskazuje na bardzo silny efekt.
#Bardzo duża liczba obserwacji (n = 83,625) sprawia, że test jest bardzo wiarygodny.
#Ostatecznie można stwierdzić, że rodzaj paliwa silnie wpływa na cenę samochodu, i wynik ten jest bardzo pewny ze względu na ekstremalnie niski poziom p-wartości.

2##korelacja przebieg i cena 
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