# Prognoza unor indicatori micro- sau macro-economici
install.packages("tidyverse")
library(tidyverse)
install.packages("forecast")
library(forecast)
install.packages("ggplot2")
library(ggplot2)

date_MI <- MegaImage_Rstudio
View(date_MI)
# 1. Modelul de regresie liniara
# Crearea unui model simplu pentru Venituri_Totale bazat pe Cantitate si Pret_Unitar
model_lm <- lm(Val_totala ~ Cantitate + Pret_Unitar, data = date_MI)
summary(model_lm)

# Creare data frame pentru predicții
data_new <- data.frame(
  Cantitate = date_MI$Cantitate,
  Pret_Unitar = date_MI$Pret_Unitar
)

# Aplicarea predicțiilor
date_MI <- date_MI %>%
  mutate(Predictii_LM = predict(model_lm, newdata = data_new))


ggplot(date_MI, aes(x = Cantitate, y = Val_totala)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = Predictii_LM), color = "red", size = 1) +
  labs(
    title = "Regresie liniară: Venituri în funcție de Cantitate și Preț",
    x = "Cantitate",
    y = "Venituri Totale"
  ) +
  theme_minimal()

# 2. Modelul ARIMA
# Crearea unei serii temporale
venituri_ts <- ts(date_MI$Val_totala, frequency = 12, start = c(2024, 1))

# Aplicarea modelului ARIMA
model_arima <- auto.arima(venituri_ts)
summary(model_arima)

# Prognoza pentru urmatoarele 12 luni
forecast_arima <- forecast(model_arima, h = 12)

autoplot(forecast_arima) +
  labs(title = "Prognoza Venituri Totale folosind ARIMA", x = "Ani", y = "Venituri Totale")

# 3. Analiza de scenarii
# Crearea scenariilor optimist și pesimist
date_MI <- date_MI %>%
  mutate(
    Optimist = Val_totala * 1.10,
    Pesimist = Val_totala * 0.90,
    Luna = factor(Luna, levels = c("ianuarie", "februarie", "martie", "aprilie", "mai", "iunie",
                                   "iulie", "august", "septembrie", "octombrie", "noiembrie", "decembrie"))
  )

combined_data <- date_MI %>%
  select(Luna, Val_totala, Optimist, Pesimist) %>%
  pivot_longer(cols = c(Val_totala, Optimist, Pesimist),
               names_to = "Scenariu", values_to = "Valoare")

# Generarea graficului
ggplot(combined_data, aes(x = Luna, y = Valoare, color = Scenariu, group = Scenariu)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Analiza de scenarii pentru Venituri Totale",
    x = "Luna",
    y = "Valoare",
    color = "Scenariu"
  ) +
  theme_minimal()

