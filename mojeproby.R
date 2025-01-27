install.packages("ggstatsplot")
library(ggstatsplot)
library(dplyr)
library(rlang)
library(ggplot2)
ggbetweenstats(
  data = samochody_top_5_brands,
  x = brand,
  y = price_in_pln,
  title = "proba",
  xlab = "marka",
  ylab = "cena",
  type = "np",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.param = "median",
  centrality.para.method = "nonparametric",
  ggtheme = theme_gray(),
  messages = FALSE)
  )

#2 
png("wykres_bez_bledu1.png", width = 1600, height = 1200, res = 300)

samochody_top_5_brands <- na.omit(samochody_top_5_brands)  # Usuń brakujące dane

ggbetweenstats(
  data = samochody_top_5_brands,
  x = brand,
  y = price_in_pln,
  title = "Czytelny wykres bez błędów",
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
)

dev.off()
#2
png("wykres_bez_bledu1.png", width = 1600, height = 1200, res = 300)

samochody_top_5_brands <- na.omit(samochody_top_5_brands)  # Usuń brakujące dane

ggbetweenstats(
  data = samochody_top_5_brands,
  x = brand,
  y = price_in_pln,
  title = "Czytelny wykres bez błędów",
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
