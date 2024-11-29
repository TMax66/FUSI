# codice da chatgpt
# 
#
pkg()

dt <- read_excel(here("dati","Registro_carico_scarico_Scalvini_2014-2023_27.05.2024.xlsx"), 
                 sheet = "Registro carico-scarico 2014-23")

dt %>% 
  clean_names() %>% 
  select(codice_capo, 
         data_nascita, 
         data_ingresso,
         data_uscita_stalla,
         motivo_ingresso, 
         motivo_uscita)-> dt



dt %>% 
  mutate(estrazione = as.Date("2024-05-27"), 
         age = ifelse(is.na(data_uscita_stalla), 
                      as.numeric(difftime(estrazione, data_nascita, units = "days")) / 365.25, 
                      as.numeric(difftime(data_uscita_stalla, data_nascita, units = "days")) / 365.25), 
         
         
         stay_in =  ifelse(is.na(data_uscita_stalla), 
                           as.numeric(difftime(estrazione, data_ingresso, units = "days")) / 365.25, 
                           as.numeric(difftime(dt$data_uscita_stalla, dt$data_ingresso, units = "days")) / 365.25))  %>% 


  mutate(status = ifelse(motivo_uscita %in% c("M", "N", "D"), "M", "V"),
         status = as.factor(status),
    
    birth_year = year(data_nascita), 
    exit_year = year(data_uscita_stalla)) -> dt_lexis
  

lexis_grid(year_start = 2017, year_end = 2024, age_start = 0, age_end = 10, delta = 0.5,lwd = 0.003) %>% 
  lexis_lifeline( birth = dt_lexis$data_nascita, 
                  exit =  dt_lexis$data_uscita_stalla, 
                  entry = dt_lexis$data_ingresso, lwd = 0.1, lineends = TRUE)



