pkg() 
library(LexisPlotR)
library(Epi)

dt <- read_excel(here("dati","Registro_carico_scarico_Scalvini_2014-2023_27.05.2024.xlsx"), 
                                         sheet = "Registro carico-scarico 2014-23")





















# anno periodo 
# questo codice serve per definire l'età degli individui in un determinato periodo di tempo 
# cioè "anno di osservazione"... ad esempio dal dataset ho per ogni bovina la data di nascita, 
# la data di uscita e la data di estrazione dei dati, quindi posso sapere l'età di ogni singola 
# bovina alla data di estrazione, ma non ho le età per i diversi periodi passati
# ad esempio la bovina 	 DE0351016122 è nata nel 2007 quindi alla data di estrazione dei dati
# dall'allevamento ha 17 anni e posso agilmente calcolare l'età nel 2024 facendo la differenza
# tra l'anno di estrazione e la data di nascita ( lo stesso posso fare con la data di uscita)
# mi serve sapere  l'età dei soggetti nei diversi periodi quindi è sufficiente aggiungere al dataset un numero di colonne pari 
# al numero di periodi d'interesse

dt %>% 
  clean_names() %>% 
  select(codice_capo, 
         data_nascita, 
         data_ingresso,
         data_uscita_stalla,
         motivo_ingresso, 
         motivo_uscita, 
         data_estrazione_dati) %>% 
  mutate(
    data_nascita = as.Date(data_nascita),
    data_ingresso = as.Date(data_ingresso),
    data_uscita = as.Date(data_uscita_stalla),
    eta_ingresso = as.numeric(difftime(data_ingresso, data_nascita, units = "days")) / 365.25, # Età in anni
    eta_uscita = as.numeric(difftime(data_uscita, data_nascita, units = "days")) / 365.25, # Età in anni
    year_ingresso = as.numeric(format(data_ingresso, "%Y")), # Anno di ingresso
    year_uscita = as.numeric(format(data_uscita, "%Y")) # Anno di uscita
  ) -> dati
  



calcola_statistiche_annuali_con_mortalita <- function(bovini) {
  library(dplyr)
  library(lubridate)
  
  anni <- 2017:2024
  
  risultati <- lapply(anni, function(anno) {
    inizio_anno <- as.Date(paste0(anno, "-01-01"))
    fine_anno <- as.Date(paste0(anno, "-12-31"))
    
    # Presenti nel corso dell’anno
    presenti <- bovini %>%
      filter(data_ingresso <= fine_anno & (is.na(data_uscita) | data_uscita >= inizio_anno)) %>%
      nrow()
    
    # Usciti durante l’anno
    usciti <- bovini %>%
      filter(!is.na(data_uscita) & year(data_uscita) == anno) %>%
      nrow()
    
    # Morti durante l’anno
    morti <- bovini %>%
      filter(!is.na(data_uscita) & year(data_uscita) == anno & motivo_uscita == "M") %>%
      nrow()
    
    # Tasso di mortalità (%)
    tasso_mortalita <- ifelse(presenti > 0, (morti / presenti) * 100, NA)
    
    data.frame(
      anno = anno,
      presenti = presenti,
      usciti = usciti,
      morti = morti,
      tasso_mortalita = round(tasso_mortalita, 2)
    )
  })
  
  bind_rows(risultati)
}

  



calcola_statistiche_complete <- function(bovini) {
  library(dplyr)
  library(lubridate)
  
  anni <- 2017:2024
  
  risultati <- lapply(anni, function(anno) {
    inizio_anno <- as.Date(paste0(anno, "-01-01"))
    fine_anno <- as.Date(paste0(anno, "-12-31"))
    
    # Calcolo giorni di permanenza durante l’anno
    bovini_anno <- bovini %>%
      mutate(
        start = pmax(data_ingresso, inizio_anno, na.rm = TRUE),
        end = pmin(coalesce(data_uscita, fine_anno), fine_anno, na.rm = TRUE),
        giorni_presenza = as.numeric(end - start + 1)
      ) %>%
      filter(giorni_presenza > 0)
    
    giorni_totali <- sum(bovini_anno$giorni_presenza)
    presenti_equivalenti <- round(giorni_totali / 365, 2)
    
    usciti <- bovini %>%
      filter(!is.na(data_uscita) & year(data_uscita) == anno) %>%
      nrow()
    
    morti <- bovini %>%
      filter(!is.na(data_uscita) & year(data_uscita) == anno & motivo_uscita == "M") %>%
      nrow()
    
    tasso_mortalita <- ifelse(presenti_equivalenti > 0, (morti / presenti_equivalenti) * 100, NA)
    
    data.frame(
      anno = anno,
      presenti_equivalenti = presenti_equivalenti,
      usciti = usciti,
      morti = morti,
      tasso_mortalita = round(tasso_mortalita, 2)
    )
  })
  
  bind_rows(risultati)
}

dati %>% 
  select(codice_capo, data_ingresso, data_uscita, motivo_uscita, data_nascita)-> dt


calcola_statistiche_complete(dt)


library(ggplot2)
library(dplyr)
library(lubridate)

bovini_plot <-dt %>%
  mutate(
    data_uscita_plot = coalesce(data_uscita, as.Date("2024-12-31")),
    stato_fine = case_when(
      is.na(data_uscita) ~ "presente",
      motivo_uscita == "M" ~ "morto",
      TRUE ~ "uscito"
    ),
    eta_ingresso = as.numeric(difftime(data_ingresso, data_nascita, units = "days")) / 365.25,
    eta_uscita   = as.numeric(difftime(data_uscita_plot, data_nascita, units = "days")) / 365.25,
    anno_uscita  = year(data_uscita)
  )

# Lexis con esclusione dei punti per chi è uscito nel 2024 (anche se realmente)
ggplot(bovini_plot) +
  geom_segment(aes(x = data_ingresso, xend = data_uscita_plot,
                   y = eta_ingresso, yend = eta_uscita),
               color = "gray40") +
  geom_point(data = filter(bovini_plot, !is.na(data_uscita) & anno_uscita < 2024),
             aes(x = data_uscita_plot, y = eta_uscita, color = stato_fine),
             size = 2) +
  scale_color_manual(values = c("morto" = "red", "uscito" = "blue")) +
  labs(
    title = "Diagramma di Lexis - Bovini (senza uscite 2024)",
    x = "Anno di calendario",
    y = "Età (anni)",
    color = "Evento finale"
  ) +
  theme_minimal()








# 
# 
# 
# library(Epi)
# 
# dati$durata <- as.numeric(dati$data_uscita - dati$data_ingresso)
# 
# 
# numero_morti <- sum(dati$motivo_uscita == "M", na.rm = TRUE)
# tempo_animale <- sum(dati$durata, na.rm = TRUE)
# tempo_animale_anni <- tempo_animale / 365.25
# tasso_mortalità <- numero_morti / tempo_animale_anni
# 
# 
# dati$evento <- ifelse(dati$motivo_uscita == "M", "Morto", "Censurato")
# 
# dati$eta_ingresso <- as.numeric(dati$data_ingresso - dati$data_nascita)/365.25
# 
# 
# data_censura <- as.Date("2024-05-27")
# 
# dati$data_uscita[is.na(dati$data_uscita)] <- data_censura
# dati$evento <- ifelse(!is.na(dati$motivo_uscita) & dati$motivo_uscita == "M", "Morto", "Censurato")
# dati$eta_ingresso <- as.numeric(dati$data_ingresso - dati$data_nascita) / 365.25
# 
# 
# baseline <- as.Date("2000-01-01")
# dati$tcal_entry <- as.numeric(dati$data_ingresso - baseline) / 365.25
# dati$tcal_exit <- as.numeric(dati$data_uscita - baseline) / 365.25
# dati$eta_ingresso <- as.numeric(dati$data_ingresso - dati$data_nascita) / 365.25
# 
# library(Epi)
# lexis_obj2 <- Lexis(entry = list(per = dati$tcal_entry, age = dati$eta_ingresso),
#                     exit = list(per = dati$tcal_exit),
#                     exit.status = dati$evento,
#                     data = dati)
# 
# 
# 
# 
# 
# inizio <- as.Date(paste0(format(min(dati$data_ingresso, na.rm = TRUE), "%Y"), "-01-01"))
# fine    <- as.Date(paste0(format(max(dati$data_uscita, na.rm = TRUE), "%Y"), "-12-31"))
# 
# inizio_num <- as.numeric(inizio - baseline) / 365.25
# fine_num   <- as.numeric(fine - baseline) / 365.25
# 
# breaks_num <- seq(inizio_num, fine_num, by = 1)
# 
# lexis_split <- splitLexis(lexis_obj2, breaks = breaks_num, time.scale = "per")
# 
# lexis_split$anno <- floor(lexis_split$per)
# 
# sintesi_annuale <- lexis_split %>%
#   group_by(anno) %>%
#   summarise(
#     person_time = sum(lex.dur, na.rm = TRUE),         # tempo a rischio in anni
#     morti       = sum(evento == "Morto", na.rm = TRUE)  # eventi di morte
#   ) %>%
#   mutate(
#     tasso_mortalita = morti / person_time             # tasso di mortalità (eventi per anno-persona)
#   )
# 
# 
# 
# 
# table(lexis_obj$evento)
# table(lexis_split$anno, lexis_split$evento)
# 
# 
# 
# 
# 
# 
# 
# dati <- dati %>%
#   
#   
#   mutate(
#     # Convertire tutte le date nel formato Date
#     data_nascita = as.Date(data_nascita),
#     data_ingresso = as.Date(data_ingresso),
#     data_uscita = as.Date(data_uscita),
#     
#     # Se la data di uscita è NA, assegniamo la data odierna per evitare problemi
#     data_uscita = ifelse(is.na(data_uscita), as.character(Sys.Date()), as.character(data_uscita)),
#     data_uscita = as.Date(data_uscita),
#     
#     # Calcolare età all'ingresso e all'uscita in anni
#     eta_ingresso = as.numeric(difftime(data_ingresso, data_nascita, units = "days")) / 365.25,
#     eta_uscita = as.numeric(difftime(data_uscita, data_nascita, units = "days")) / 365.25,
#     
#     # Estrarre gli anni (ora data_uscita è sicuramente valida)
#     per_ingresso = as.numeric(format(data_ingresso, "%Y")),
#     per_uscita = as.numeric(format(data_uscita, "%Y")),
#     
#     # Assegnare 1 se il motivo di uscita è "M" (morte), altrimenti 0
#     exit.status = ifelse(is.na(motivo_uscita), 0, ifelse(motivo_uscita == "M", 1, 0))
#   ) %>% glimpse()
# 
# dati <- dati %>%
#   mutate(
#     durata_year = year_uscita - year_ingresso,
#     durata_eta = eta_uscita - eta_ingresso,
#     correzione = durata_per - durata_eta,
#     
#     # Se le due durate non coincidono, correggiamo eta_uscita
#     eta_uscita = ifelse(correzione != 0, eta_ingresso + durata_year, eta_uscita)
#   )
# 
# 
# dati_lexis <- Lexis(
#   entry = list(per = dati$per_ingresso, age = dati$eta_ingresso),
#   exit = list(per = dati$per_uscita, age = dati$eta_uscita),
#   exit.status = dati$exit.status,
#   data = dati
# )
# 
# 
# 
# 
# # Impostiamo i margini per il grafico
# par(mar = c(5, 5, 2, 1))
# 
# # Creiamo il grafico
# plot(
#   dati_lexis$per, 
#   dati_lexis$age, 
#   col = "white",  # Non vogliamo tracciare alcun punto all'inizio
#   main = "Diagramma di Lexis - Bovini",
#   xlab = "Anno di calendario",
#   ylab = "Età (anni)",
#   xlim = c(min(dati$per_ingresso), 2024),  # Limitiamo fino al 2024
#   ylim = c(min(dati$eta_ingresso), max(dati$eta_uscita)), # Limitiamo le età
#   type = "n"  # Non tracciare nessun dato inizialmente
# )
# 
# # Tracciamo le linee di vita dei bovini
# # Usando un ciclo per tracciare le linee di vita dei singoli bovini
# for (i in 1:nrow(dati_lexis)) {
#   # Colore rosso per morti, blu per sopravvissuti
#   lines(c(dati_lexis$per[i], dati_lexis$per_uscita[i]), 
#         c(dati_lexis$age[i], dati_lexis$eta_uscita[i]),
#         col = ifelse(dati_lexis$exit.status[i] == 1, "red", "blue"), 
#         lwd = 1)
# }
# 
# # Aggiungiamo la griglia per gli anni (linee verticali) e le età (linee orizzontali)
# grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted", lwd = 0.5)
# 
# # Aggiungiamo una legenda per indicare i morti e i sopravvissuti
# legend("topleft", legend = c("Morti", "Sopravvissuti"), col = c("red", "blue"), pch = 1)
# 
# 
# 
# # mutate(coorte = year(data_nascita), 
#   #        età_uscita = year(data_uscita_stalla)-coorte, 
#   #        estrazione = year("2024-05-27"), 
#   #        p2017 = year("2017-01-01"), 
#   #        p2018 = year("2018-01-01"), 
#   #        p2019 = year("2019-01-01"), 
#   #        p2020 = year("2020-01-01"), 
#   #        p2021 = year("2021-01-01"), 
#   #        p2022 = year("2022-01-01"), 
#   #        p2023 = year("2023-01-01"), 
#   #        year17 = p2017-coorte,
#   #        year18 = p2018-coorte, 
#   #        year19 = p2019-coorte,
#   #        year20 = p2020-coorte, 
#   #        year21 = p2021-coorte,
#   #        year22 = p2022-coorte,
#   #        year23 = p2023-coorte, 
#   #        year24 = estrazione - coorte) %>% 
#   # select(-c(9:16)) %>%  
#   # pivot_longer(cols = 9:16, names_to = "period", values_to = "ageperiod") %>%   View()
#   # filter(ageperiod >= 0) %>%  
#   # group_by(period, ageperiod) %>% 
#   # count() %>%  View()
#  
#   
#   
#   
# dt %>% 
#   clean_names() %>% 
#   select(codice_capo, 
#          data_nascita, 
#          data_ingresso,
#          data_uscita_stalla,
#          motivo_ingresso, 
#          motivo_uscita) %>%  
#   mutate(
#     birth_y = year(data_nascita), 
#     into_y = year(data_ingresso), 
#     exit_y = year(data_uscita_stalla)
#   ) %>%  
#   
#   mutate(age_into = as.numeric(difftime(data_ingresso, data_nascita, units = "days"))/365.25, 
#          age_exit = as.numeric(difftime(data_uscita_stalla, data_nascita, units = "days"))/365.25, 
#          time_in = as.numeric(difftime(data_uscita_stalla, data_ingresso, units = "days"))/365.25) %>% 
#   
#   mutate(
#     gruppo_eta = round(age_exit, 0),
#     gruppo_periodo = round(exit_y, 0),
#     gruppo_coorte = round(birth_y, 0)
#   ) %>% 
#   
#   filter(exit_y >= 2019 ) %>%  
#   
#   group_by(gruppo_periodo) %>% 
#   count()
#  
#   
# 
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
# 
# 
# lx <- lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 1,lwd = 0.003)
# 
# lexis_age(lg = lx, age = 5)
# 
# lexis_year(lg = lx, year = 2020)
# 
# lexis_cohort(lg = lx, cohort = 2019)
# lexis_lifeline(lg = lx, birth = "2015-04-19", entry = "2017-10-26", exit = "2022-07-15", lwd = 0.3, colour = "blue")
# 
# 
# dt %>% 
#   mutate(birth_Y = year(DATA_NASCITA)) %>% 
#   filter(birth_Y == 2020) -> dt20
# 
# dt20 %>%  View()
# 
# lx %>% 
# lexis_cohort(cohort = 2020) %>% 
# lexis_lifeline( birth = dt20$DATA_NASCITA, 
#                 exit =  dt20$`DATA USCITA STALLA`, 
#                 entry = dt20$DATA_INGRESSO, lwd = 0.1, lineends = TRUE) 
# 
# 
# dt %>% 
#   #filter(!is.na(`DATA USCITA STALLA`)) %>% 
#   mutate(coorte = year(DATA_NASCITA))-> dtx 
# 
# dtx %>% 
#   sample_n(459) -> herdA
# 
# dtx %>% 
#   sample_n(200) -> herdB
# 
# dtx %>% 
#   sample_n(50)-> herdC
# 
# 
# 
# dtx2  <- dtx %>%  sample(dt, size = 459, replace  )
#   
# lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
#   lexis_lifeline( birth = herdA$DATA_NASCITA, 
#                   exit =  herdA$`DATA USCITA STALLA`, 
#                   entry = herdA$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> A
# lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
# lexis_lifeline( birth = herdB$DATA_NASCITA, 
#                 exit =  herdB$`DATA USCITA STALLA`, 
#                 entry = herdB$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> B
# lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
#   lexis_lifeline( birth = herdC$DATA_NASCITA, 
#                   exit =  herdC$`DATA USCITA STALLA`, 
#                   entry = herdC$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> C
# 
# library(patchwork)
# A+B+C
#  
# 
# 
# 
# 
# 
#   # lexis_year(year=2021, fill = "lightgray") %>% 
#   # lexis_cohort(cohort = 2018, fill = "lightgreen")
# 
#  
#  
# dtx %>% 
#   mutate(out = ifelse(is.na(`MOTIVO USCITA`), "uscita", "dentro")) %>% 
#   group_by(coorte, out) %>% 
#   count() %>%  
#   pivot_wider(names_from = out, values_from = n) %>% 
#   mutate(rate_out = 100*uscita/(uscita+dentro)) %>% 
#  ggplot()+
#   aes(x = coorte, 
#       y = rate_out )+
#   geom_point()+
#   geom_line()
#  
# 
# 
# polygons <- data.frame(group = c(1, 1, 1),
#                        x = c("2022-01-01", "2023-01-01", "2023-01-01"),  
#                        y = c(1, 1, 2))
# 
# lexis_polygon(lx, x = polygons$x, y = polygons$y, group = polygons$group)
