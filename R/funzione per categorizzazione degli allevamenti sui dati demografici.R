# Carica le librerie necessarie
library(dplyr)
library(lubridate)


prep_herd <- function(dati, data_fine = as.Date("2024-05-27")) {
  
  
  dati %>%
    mutate(
      data_fine_osservazione = pmin(coalesce(data_uscita, data_fine), data_fine),
      morto = motivo_uscita == "M" & !is.na(data_uscita) & data_uscita <= data_fine,
      eta_morte = as.numeric(data_fine_osservazione - data_nascita) / 365.25,
      evento = ifelse(morto, 1, 0)
    )
  
}


prep_herd(herdA)-> dfherd


anni_osservazione <- as.numeric(difftime(max(dfherd$data_uscita, na.rm = TRUE),
                                         min(dfherd$data_ingresso, na.rm = TRUE),
                                         units = "days")) / 365.25

n_capi <- nrow(dfherd)

n_decessi <- sum(dfherd$evento == 1, na.rm = TRUE)

tasso_mortalità <- (n_decessi / (n_capi * anni_osservazione)) * 100

tasso_uscita <- (sum(!is.na(dfherd$data_uscita)) / (n_capi * anni_osservazione)) * 100

età_media_morte <- mean(dfherd$eta_uscita[dfherd$evento == 1], na.rm = TRUE)

# Funzione per analizzare un singolo allevamento
analizza_allevamento <- function(df_allevamento) {
  # Calcola il numero di anni di osservazione
  anni_osservazione <- as.numeric(difftime(max(df_allevamento$data_uscita, na.rm = TRUE),
                                           min(df_allevamento$data_ingresso, na.rm = TRUE),
                                           units = "days")) / 365.25
  # Calcola il numero di capi
  n_capi <- nrow(df_allevamento)
  
  # Calcola il numero di decessi
  n_decessi <- sum(df_allevamento$evento == 1, na.rm = TRUE)
  
  # Calcola il tasso di mortalità per 100 capi per anno
  tasso_mortalità <- (n_decessi / (n_capi * anni_osservazione)) * 100
  
  # Calcola il tasso di ingresso per 100 capi per anno
  tasso_ingresso <- (n_capi / anni_osservazione) * 100
  
  # Calcola il tasso di uscita per 100 capi per anno
  tasso_uscita <- (sum(!is.na(df_allevamento$data_uscita)) / (n_capi * anni_osservazione)) * 100
  
  # Calcola l'età media alla morte
  età_media_morte <- mean(df_allevamento$eta_uscita[df_allevamento$evento == 1], na.rm = TRUE)
  
  # Calcola la durata media della permanenza
  durata_media <- mean(df_allevamento$lex.dur, na.rm = TRUE)
  
  # Categorizzazione basata su soglie definite
  categoria <- case_when(
    tasso_mortalità < 5 & durata_media > 1 ~ "Basso Rischio",
    tasso_mortalità >= 5 & tasso_mortalità < 10 ~ "Medio Rischio",
    tasso_mortalità >= 10 ~ "Alto Rischio",
    TRUE ~ "Non Classificato"
  )
  
  # Restituisce un data frame con i risultati
  return(data.frame(
    tasso_mortalità = tasso_mortalità,
    tasso_ingresso = tasso_ingresso,
    tasso_uscita = tasso_uscita,
    età_media_morte = età_media_morte,
    durata_media = durata_media,
    categoria = categoria
  ))
}

analizza_allevamento(dfherd)


# Funzione per analizzare tutti gli allevamenti
analizza_tutti_allevamenti <- function(df_completo) {
  df_completo %>%
    group_by(codice_allevamento) %>%
    group_modify(~ analizza_allevamento(.x)) %>%
    ungroup()
}
