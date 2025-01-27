#Hejka
#CZESC
# eciceicneincei
View(samochody_new)
install.packages(c("tidyverse", "naniar"))
library(tidyverse)
library(naniar)
n_miss(samochody_new)
prop_miss(samochody_new)
miss_var_summary(samochody_new)%>% 
  group_by(samochody_new$brand) %>% 
  miss_var_summary()
samochody_new %>% 
  miss_case_table()
vis_miss(samochody_new)
gg_miss_fct(samochody_new, fct = brand)
gg_miss_fct(samochody_new, fct = gearbox)
gg_miss_upset(samochody_new, 
              nsets = 10)
ggplot(data = samochody_new, aes(x = brand, y = model)) +
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal() 

#data wwrangling
samochody_new %>%
  count(brand)

samochody_new %>%
  count(year)
colnames(samochody_new)[4] <- "fuel_type"
view(samochody_new)
colnames(samochody_new)[4] <- "mileage"
view(samochody_new)
