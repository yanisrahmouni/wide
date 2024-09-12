#harmonisation df 2004-2020 pour stat sur diff femmes/ hommes de donations

library(tidyverse)
library(here)
library(stats)

##2004
menage <- read.csv2(here("00_data/2004/Csv/menage.csv"))%>%
  select(IDENT, 
         contains("PATRI"), #patri brut I guess? 
         AGEPR,          
         pond, 
         SEXEPR, 
         TYPMEN, 
         HODNB, DONHOD, DONVERS, 
         MATRIPR, 
         RMINTER) %>% 
  mutate(across(c(contains("IDENT")), ~as.character(.x)), 
         TYPMEN = case_when(TYPMEN %in% c(2:5) ~ 1, 
                             T ~ 0), 
         HODNB =  ifelse(HODNB == 0, 0, 1))

transmission <- read.csv2(here("00_data/2004/Csv/transmission.csv"))%>%
  select(IDENT, 
         MTDONV, TRANSNA, IDENTTRANS) %>%
  mutate(across(contains("IDENT"), ~as.character(.x))) %>%
  filter(TRANSNA ==3) %>%
  group_by(IDENT) %>%
  summarise(n_donation = n())%>%
  ungroup() 

merged1 <- left_join(menage, transmission)%>%
  mutate(across(-c(PATRI), ~as.character(.x)), 
         pond = as.numeric(pond), 
         year = 2004) %>%
  rename(ENFHORS = HODNB)%>%
  replace_na(list(n_donation = "0")) %>%
    mutate(donation_dicho = if_else(n_donation ==0, 0, 1))

#2009
menage <- read.csv2(here("00_data/2009/Csv/menage.csv"))%>%
  select(IDENTMEN, 
         AGEPR,          
         SEXEPR, 
         COUPLEPR, 
         SEXEPR, 
         POND, 
         MATRIPR, 
         ENFHORS, DONHOD, PATRI_BRUT, DONVERS, 
         RMINTER) %>% 
  mutate(across(c(contains("IDENT")), ~as.character(.x)), 
         COUPLEPR = case_when(COUPLEPR %in% c(2:3) ~ 0, 
                            T ~ 1)) 

transmission <- read.csv2(here("00_data/2009/Csv/transmission.csv"))%>%
  select(IDENTMEN, TRANSNA, IDENTTRANS, QUIAUT, QUIENF, MTDONVC, QUIENFHOD) %>%
  mutate(across(contains("IDENT"), ~as.character(.x))) %>%
  filter(TRANSNA ==3)%>%
  group_by(IDENTMEN)%>%
  summarise(donation_enfant_hod = if_else(any(QUIENFHOD == 1), 1, 0), 
            donation_autre = if_else(any(QUIAUT ==1), 1, 0), 
            n_donation = n(), 
            mean_montant = mean(MTDONVC))

merged <- left_join(menage, transmission)%>%
  mutate(across(-c(PATRI_BRUT), ~as.character(.x)), 
         POND = as.numeric(POND), 
         year = 2009) %>%
  replace_na(list(n_donation = "0"))

######### on check qu'on a les mêmes résultats que Masson sur dons et création d'entreprise####
check <- merged%>%
  filter(ENFHORS == 1) %>%
  group_by(IDENTMEN) %>%
  select(PATRI_BRUT, IDENTMEN, POND, donation) %>%
  unique() %>%
  ungroup() %>%
  mutate(quantile100 = cut(PATRI_BRUT, unique(quantile(PATRI_BRUT, w = POND, seq(0, 1, 0.01))), labels = FALSE), 
         quanti_masson = case_when(quantile100 <= 25 ~ "0-25", 
                                   quantile100 > 25 & quantile100 <= 50 ~ "25-50", 
                                   quantile100 > 50 & quantile100 <= 70 ~ "50-70", 
                                   quantile100 > 70 & quantile100 <= 90 ~ "70-90", 
                                   quantile100 > 90 & quantile100 <= 99 ~ "90-99", 
                                   quantile100 > 99 & quantile100 <= 100 ~ "99-100")) 

check_masson <- check %>%
  group_by(quanti_masson) %>%
  mutate(total = sum(POND)) %>%
  group_by(quanti_masson, donation)%>%
  summarise(n_donation = sum(POND)/ total)%>% unique()%>%
  filter(donation == 1)
    
plot_masson <- check_masson%>%
ggplot(aes(x = quanti_masson)) +
  geom_histogram(aes(y = 100*n_donation), stat = "identity") + 
  theme_bw() + 
  xlab("") + ylab("") + 
  ggtitle("Pourcentage de donateurs - ménages avec enfants hors domicile (EP 2009)")

check_2009 <- merged%>%
  group_by(IDENTMEN) %>%
  select(PATRI_BRUT, IDENTMEN, POND, donation) %>%
  unique() %>%
  ungroup() %>%
  mutate(quantile100 = cut(PATRI_BRUT, unique(quantile(PATRI_BRUT, w = POND, seq(0, 1, 0.01))), labels = FALSE), 
         quanti_masson = case_when(quantile100 <= 25 ~ "0-25", 
                                   quantile100 > 25 & quantile100 <= 50 ~ "25-50", 
                                   quantile100 > 50 & quantile100 <= 70 ~ "50-70", 
                                   quantile100 > 70 & quantile100 <= 90 ~ "70-90", 
                                   quantile100 > 90 & quantile100 <= 99 ~ "90-99", 
                                   quantile100 > 99 & quantile100 <= 100 ~ "99-100")) 


check_masson <- check_2009 %>%
  group_by(quanti_masson) %>%
  mutate(total = sum(POND)) %>%
  group_by(quanti_masson, donation)%>%
  summarise(n_donation = sum(POND)/ total)%>% unique()%>%
  filter(donation == 1)

plot_masson_2 <- check_masson%>%
  ggplot(aes(x = quanti_masson)) +
  geom_histogram(aes(y = 100*n_donation), stat = "identity") + 
  theme_bw() + 
  xlab("") + ylab("") + 
  ggtitle("Pourcentage de donateurs - tous les ménages (EP 2009)")

check_masson_plot <- arrangeGrob(plot_masson, plot_masson_2)
ggsave(here("04_plots/check_masson.png"), check_masson_plot)


#masson trouve 5%, on trouve 27%. il a probablement inclu tous les menages, pas seulement ceux 
#qui ont un enfant hors domicile 


######

merged_table <-merged%>%
  rename(IDENT = IDENTMEN, 
         PATRI= PATRI_BRUT, 
         pond = POND, 
         TYPMEN = COUPLEPR)%>%
  mutate(donation_dicho = if_else(n_donation ==0, 0, 1))%>%
  bind_rows(merged1) 

rm(merged)

#2014

menage <- read.csv2(here("00_data/2014/Csv/menage.csv"))%>%
  select(IDENT, 
         PATRI_BRUT, 
         AGEPR,          
         SEXEPR, 
         COUPLEPR, 
         SEXEPR, 
         POND, 
         MATRIPR, 
         RMINTER, 
         ENFHORS, DONHOD, DONVERS) %>% 
  mutate(across(c(contains("IDENT")), ~as.character(.x)), 
         COUPLEPR = case_when(COUPLEPR %in% c(2:3) ~ 0, 
                              T ~ 1)) 

transmission <- read.csv2(here("00_data/2014/Csv/transmission.csv"))%>%
  select(IDENT, 
         MTDONV, TRANSNA, IDENTTRANS, QUIENF, QUIAUT, MTDONVC, QUIENFHOD) %>%
  mutate(across(contains("IDENT"), ~as.character(.x))) %>%
  filter(TRANSNA ==3)%>%
  group_by(IDENT)%>%
  summarise(donation_enfant_hod = if_else(any(QUIENFHOD == 1), 1, 0), 
            donation_autre = if_else(any(QUIAUT ==1), 1, 0), 
            n_donation = n(), 
            mean_montant = mean(MTDONVC))

merged_table_2 <- left_join(menage, transmission)%>%
  mutate(across(-c(PATRI_BRUT), ~as.character(.x)), 
         POND = as.numeric(POND), 
         year = 2014) %>%
  replace_na(list(n_donation = "0"))%>%
  rename(PATRI= PATRI_BRUT, 
         pond = POND, 
         TYPMEN = COUPLEPR)%>%
  mutate(donation_dicho = if_else(n_donation ==0, 0, 1))%>%
  bind_rows(merged_table)

rm(merged_table)

#2017

menage <- read.csv2(here("00_data/2017/Csv/menage.csv"))%>%
  select(IDENT, 
         PATRI_BRUT, 
         AGEPR,          
         SEXEPR, 
         COUPLEPR, 
         SEXEPR, 
         POND_TRANS, 
         MATRIPR, 
         ZREVDEC, 
         ENFHORS, DONHOD, DONVERS) %>% 
  mutate(across(c(contains("IDENT")), ~as.character(.x)), 
         COUPLEPR = case_when(COUPLEPR %in% c(2:3) ~ 0, 
                              T ~ 1)) 

transmission <- read.csv2(here("00_data/2017/Csv/transmission.csv"))%>%
  select(IDENT, 
         MTDONV, TRANSNA, IDENTTRANS, QUIENF, QUIAUT, MTDONVC, contains("DONTYP"), QUIENFHOD) %>%
  mutate(across(contains("IDENT"), ~as.character(.x))) %>%
  filter(TRANSNA ==3)%>%
  group_by(IDENT)%>%
  summarise(donation_enfant_hod = if_else(any(QUIENFHOD == 1), 1, 0), 
            donation_autre = if_else(any(QUIAUT ==1), 1, 0), 
            n_donation = n(), 
            mean_montant = mean(MTDONVC), 
            bien_prof = if_else(any(DONTYP3 == 1), 1, 0), 
            terrain = if_else(any(DONTYP4 == 1), 1, 0), 
            logement = if_else((any(DONTYP1 == 1) | any(DONTYP2 == 1)), 1, 0), 
            argent = if_else(any(DONTYP6 == 1), 1, 0), 
            meuble = if_else(any(DONTYP7 == 1), 1, 0), 
            finance = if_else(any((DONTYP5 == 1) | any(DONTYP8 ==1)), 1, 0))


merged_table_3 <- left_join(menage, transmission)%>%
  mutate(across(-c(PATRI_BRUT), ~as.character(.x)), 
         POND_TRANS = as.numeric(POND_TRANS), 
         year = 2017) %>%
  replace_na(list(n_donation = "0"))%>%
  rename(PATRI= PATRI_BRUT, 
         pond = POND_TRANS, 
         TYPMEN = COUPLEPR)%>%
  mutate(donation_dicho = if_else(n_donation ==0, 0, 1))%>%
  bind_rows(merged_table_2)

rm(merged_table_2)

#2020

menage <- read.csv2(here("00_data/2020/Csv/menage.csv"))%>%
  select(IDENT, 
         PATRI_BRUT, 
         AGEPR,          
         SEXEPR, 
         COUPLEPR, 
         SEXEPR, 
         POND_TRANS, 
         MATRIPR, 
         ENFHORS, DONHOD, DONVERS, 
         ZREVDEC) %>% 
  mutate(across(c(contains("IDENT")), ~as.character(.x)), 
         COUPLEPR = case_when(COUPLEPR %in% c(2:3) ~ 0, 
                              T ~ 1)) 

transmission <- read.csv2(here("00_data/2020/Csv/transmission.csv"))%>%
  select(IDENT, 
         MTDONV, TRANSNA, IDENTTRANS, QUIENF, QUIAUT, MTDONVC, contains("DONTYP"), 
         QUIENFHOD) %>%
  mutate(across(contains("IDENT"), ~as.character(.x))) %>%
  filter(TRANSNA ==3)%>%
  group_by(IDENT)%>%
  summarise(donation_enfant_hod = if_else(any(QUIENFHOD == 1), 1, 0), 
            donation_autre = if_else(any(QUIAUT ==1), 1, 0), 
            n_donation = n(), 
            mean_montant = mean(MTDONVC), 
            bien_prof = if_else(any(DONTYP3 == 1), 1, 0), 
            terrain = if_else(any(DONTYP4 == 1), 1, 0), 
            logement = if_else((any(DONTYP1 == 1) | any(DONTYP2 == 1)), 1, 0), 
            argent = if_else(any(DONTYP6 == 1), 1, 0), 
            meuble = if_else(any(DONTYP7 == 1), 1, 0), 
            finance = if_else(any((DONTYP5 == 1) | any(DONTYP8 ==1)), 1, 0))

merged_table_4 <- left_join(menage, transmission)%>%
  mutate(across(-c(PATRI_BRUT), ~as.character(.x)), 
         POND_TRANS = as.numeric(POND_TRANS), 
         year = 2020) %>%
  replace_na(list(n_donation = "0"))%>%
  rename(PATRI= PATRI_BRUT, 
         pond = POND_TRANS, 
         TYPMEN = COUPLEPR)%>%
  mutate(donation_dicho = if_else(n_donation ==0, 0, 1))%>%
  bind_rows(merged_table_3)

rm(merged_table_3)

####cleaning#####
transmission_all <- merged_table_4 %>%
  mutate(
Sexe = ifelse(SEXEPR == 1, "Homme", "Femme"),
TYPMEN  = ifelse(TYPMEN == 1, "yes", "no"), 
ENFHORS  = ifelse(ENFHORS == 1, "yes", "no")) %>%
  group_by(IDENT) %>%
  mutate(wealth_gift = case_when(any(DONHOD == 1) ~ 1, 
                                 T ~ 0), 
         pond = as.numeric(pond))  %>%
  ungroup() %>%
  select(IDENT, Sexe, pond, ENFHORS, contains("donation"), year, 
         TYPMEN, wealth_gift, DONHOD, DONVERS, AGEPR, MATRIPR, 
         ZREVDEC, RMINTER, PATRI, mean_montant, bien_prof:finance) %>%
  unique() %>%
  mutate(year = as.factor(year), 
MATRIPR = as.factor(MATRIPR), 
AGEPR = factor(
  case_when(AGEPR <30 ~ "<30",
            AGEPR %in% c(30:45) ~ "30-45", 
            AGEPR %in% c(46:60) ~ "46-60", 
            AGEPR %in% c(61:80) ~ "61-80", 
            T ~ '>80'),
  levels = c( "<30", "30-45", "46-60", "61-80", ">80")), 
         donation_dicho = as.integer(donation_dicho), 
         Age =  factor(AGEPR, levels = c( "<30", "30-45", "46-60", "61-80", ">80")), 
         PATRI = as.numeric(PATRI), 
matrim = factor(
  case_when(MATRIPR ==1 ~ "Célibataire",
            MATRIPR == 2 ~ "Marié(e) ou remarié(e)", 
           MATRIPR ==3 ~ "Veuf(ve)", 
            T ~ 'Divorcé(e)')))%>%
  filter(!is.na(PATRI))%>%
  group_by(year)%>%
  mutate(quantile100 = as.factor(cut(PATRI, unique(quantile(PATRI, w = pond, seq(0, 1, 0.1))), labels = FALSE)))%>%
  ungroup()%>%
  mutate(donation_enfant_hod = as.numeric(donation_enfant_hod), 
          mean_montant = as.numeric(mean_montant), 
          n_donation = as.numeric(n_donation), 
          aide_hod = if_else(DONHOD ==1, 1, 0))%>%
  replace_na(list(aide_hod = 0, 
                  donation_enfant_hod = 0))
  
# check que les variables DONVERS et TRANSNA sont cohérentes
transmission_check <- transmission_all %>%
  filter(year == 2004, donation_dicho == 1)

transmission_check2 <- transmission_all %>%
  filter(year == 2004, wealth_gift == 1)

truc <- anti_join(transmission_check,transmission_check2,  by = "IDENT")

transmission_all %>% 
  group_by(TYPMEN, MATRIPR) %>% count()

#c'est ok on enregistre

write.csv2(transmission_all, here("03_cleant_data/2004_2020_donateurs.csv"))


  