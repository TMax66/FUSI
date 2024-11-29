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
         motivo_uscita) %>% 
  mutate(coorte = year(data_nascita), 
         età_uscita = year(data_uscita_stalla)-coorte, 
         estrazione = year("2024-05-27"), 
         p2017 = year("2017-01-01"), 
         p2018 = year("2018-01-01"), 
         p2019 = year("2019-01-01"), 
         p2020 = year("2020-01-01"), 
         p2021 = year("2021-01-01"), 
         p2022 = year("2022-01-01"), 
         p2023 = year("2023-01-01"), 
         year17 = p2017-coorte,
         year18 = p2018-coorte, 
         year19 = p2019-coorte,
         year20 = p2020-coorte, 
         year21 = p2021-coorte,
         year22 = p2022-coorte,
         year23 = p2023-coorte, 
         year24 = estrazione - coorte) %>% 
  select(-c(9:16)) %>%  
  pivot_longer(cols = 9:16, names_to = "period", values_to = "ageperiod") %>%   View()
  filter(ageperiod >= 0) %>%  
  group_by(period, ageperiod) %>% 
  count() %>%  View()
 
  


lx <- lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 1,lwd = 0.003)

lexis_age(lg = lx, age = 5)

lexis_year(lg = lx, year = 2020)

lexis_cohort(lg = lx, cohort = 2019)
lexis_lifeline(lg = lx, birth = "2015-04-19", entry = "2017-10-26", exit = "2022-07-15", lwd = 0.3, colour = "blue")


dt %>% 
  mutate(birth_Y = year(DATA_NASCITA)) %>% 
  filter(birth_Y == 2020) -> dt20

dt20 %>%  View()

lx %>% 
lexis_cohort(cohort = 2020) %>% 
lexis_lifeline( birth = dt20$DATA_NASCITA, 
                exit =  dt20$`DATA USCITA STALLA`, 
                entry = dt20$DATA_INGRESSO, lwd = 0.1, lineends = TRUE) 


dt %>% 
  #filter(!is.na(`DATA USCITA STALLA`)) %>% 
  mutate(coorte = year(DATA_NASCITA))-> dtx 

dtx %>% 
  sample_n(459) -> herdA

dtx %>% 
  sample_n(200) -> herdB

dtx %>% 
  sample_n(50)-> herdC



dtx2  <- dtx %>%  sample(dt, size = 459, replace  )
  
lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
  lexis_lifeline( birth = herdA$DATA_NASCITA, 
                  exit =  herdA$`DATA USCITA STALLA`, 
                  entry = herdA$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> A
lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
lexis_lifeline( birth = herdB$DATA_NASCITA, 
                exit =  herdB$`DATA USCITA STALLA`, 
                entry = herdB$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> B
lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
  lexis_lifeline( birth = herdC$DATA_NASCITA, 
                  exit =  herdC$`DATA USCITA STALLA`, 
                  entry = herdC$DATA_INGRESSO, lwd = 0.1, lineends = TRUE)  -> C

library(patchwork)
A+B+C
 





  # lexis_year(year=2021, fill = "lightgray") %>% 
  # lexis_cohort(cohort = 2018, fill = "lightgreen")

 
 
dtx %>% 
  mutate(out = ifelse(is.na(`MOTIVO USCITA`), "uscita", "dentro")) %>% 
  group_by(coorte, out) %>% 
  count() %>%  
  pivot_wider(names_from = out, values_from = n) %>% 
  mutate(rate_out = 100*uscita/(uscita+dentro)) %>% 
 ggplot()+
  aes(x = coorte, 
      y = rate_out )+
  geom_point()+
  geom_line()
 


polygons <- data.frame(group = c(1, 1, 1),
                       x = c("2022-01-01", "2023-01-01", "2023-01-01"),  
                       y = c(1, 1, 2))

lexis_polygon(lx, x = polygons$x, y = polygons$y, group = polygons$group)
