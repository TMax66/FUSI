year.start <- as.Date(paste(2010, "-01-01", sep = ""))
year.end <- as.Date(paste(2020, "-01-01", sep = ""))
year.seq <- seq(year.start, year.end, by = "year")
age.seq <- 1:10


dia <- data.frame()

for (i in 1:length(year.seq[-length(year.seq)])) {
  for (j in 1:length(age.seq[-length(age.seq)])) {
    a <- year.seq[i]
    b <- year.seq[i + 1]
    c <- age.seq[j]
    d <- age.seq[j + 1]
    dia <- rbind(dia, c(a, b, c, d))
  }
}

colnames(dia) <- c("a", "b", "c", "d")
dia$a <- as.Date(dia$a, origin = "1970-01-01")
dia$b <- as.Date(dia$b, origin = "1970-01-01")


gg <- ggplot() + geom_segment(aes(x = year.seq, xend = year.seq, 
                                  y = 1, yend = 10), lwd = 0.3)+
  
  geom_segment(aes(x = year.start, 
                   xend = year.end, y = age.seq, yend = age.seq), lwd = 0.3)+
  geom_segment(aes(x = dia$a, xend = dia$b, y = dia$c, 
                   yend = dia$d), lwd = 0.3)

################################################ USO Epi############################################

library(Epi)

dt <- read_excel(here("dati","Registro_carico_scarico_Scalvini_2014-2023_27.05.2024.xlsx"), 
                 sheet = "Registro carico-scarico 2014-23")
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
  pivot_longer(cols = 9:16, names_to = "period", values_to = "ageperiod") -> dt_lexis




dmL <- Lexis(entry = list(per = period,
                          age = ageperiod,
                             ),
               + exit = list(per = dox),
               + exit.status = factor(!is.na(dodth),
                                      + labels = c("DM", "Dead")),
               + data = DMlate)
