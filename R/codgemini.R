library(tidyverse)
library(lubridate)

# Creiamo un dataset di esempio
data <- tibble(
  id = c("A", "B", "C", "D", "E"),
  data_entrata = as.Date(c("2020-01-01", "2020-03-01", "2019-01-01", "2020-05-01", "2020-08-01")),
  data_uscita = as.Date(c("2021-12-31", "2021-06-30", "2021-09-01", "2021-12-31", "2022-06-01")),
  diagnosi_tumore = c(FALSE, FALSE, TRUE, FALSE, TRUE)
)

# Calcola il tempo a rischio per ciascun cane in anni
data <- data %>%
  mutate(
    tempo_a_rischio = as.numeric(difftime(data_uscita, data_entrata, units = "days")) / 365.25
  )

# Calcola il tempo totale a rischio e i nuovi casi
tempo_persona_totale <- sum(data$tempo_a_rischio)
nuovi_casi <- sum(data$diagnosi_tumore)

# Calcola la densità di incidenza
densita_incidenza <- nuovi_casi / tempo_persona_totale

print(paste("Tempo totale a rischio (anni-persona):", round(tempo_persona_totale, 2)))
print(paste("Numero di nuovi casi di tumore:", nuovi_casi))
print(paste("Densità di incidenza (casi per anno-persona):", round(densita_incidenza, 3)))


library(ggplot2)

# Aggiungi colonne per anno e durata (età del cane nel periodo a rischio)
data <- data %>%
  mutate(
    anno_entrata = year(data_entrata),
    anno_uscita = year(data_uscita),
    eta_inizio = as.numeric(difftime(data_entrata, min(data_entrata), units = "days")) / 365.25,
    eta_fine = as.numeric(difftime(data_uscita, min(data_entrata), units = "days")) / 365.25
  )

ggplot(data) +
  # Linee che rappresentano il tempo a rischio per ciascun cane
  geom_segment(aes(
    x = data_entrata, xend = data_uscita,
    y = eta_inizio, yend = eta_fine,
    color = diagnosi_tumore
  )) +
  # Aggiungi punti per i casi di tumore diagnosticati
  geom_point(data = data %>% filter(diagnosi_tumore),
             aes(x = data_uscita, y = eta_fine), color = "red", size = 3) +
  labs(
    title = "Diagramma di Lexis per la popolazione canina",
    x = "Data di calendario",
    y = "Età del cane (in anni)",
    color = "Diagnosi di tumore"
  ) +
  theme_minimal()
