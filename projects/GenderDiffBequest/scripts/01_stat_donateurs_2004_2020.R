#stat donateurs sur 2004-2020 
library(tidyverse)
library(here)
library(spatstat)
library(flextable)
library(survey)
library(gtsummary)
library(kableExtra)

transmission_all <- read.csv2(here("03_cleant_data/2004_2020_donateurs.csv"))%>%
  mutate(Age =  factor(Age, levels = c( "<30", "30-45", "46-60", "61-80", ">80")), 
         quantile100 = as.factor(quantile100), 
         donation_enfant = as.numeric(donation_enfant))
  
###donations#####
###evolution 2004-2020#####
evol <- transmission_all %>%
  filter(ENFHORS == "yes") %>%
  select(pond, donation_dicho, IDENT, year) %>%
  unique() %>%
  group_by(year)%>%
  mutate(total = sum(pond)) %>%
  group_by(year, donation_dicho)%>%
  summarise(n_donation = sum(pond)/ total)%>% unique()%>%
  filter(donation_dicho == 1)

evol_enfant <- transmission_all %>%
  filter(ENFHORS == "yes") %>%
  replace_na(list(donation_enfant = 0))%>%
  select(pond, donation_enfant, IDENT, year) %>%
  unique() %>%
  group_by(year)%>%
  mutate(total = sum(pond)) %>%
  group_by(year, donation_enfant)%>%
  summarise(n_donation_enfant = sum(pond)/ total)%>% unique()%>%
  filter(donation_enfant == 1)%>%
  inner_join(evol)
  
evol_gift <- transmission_all %>%
  filter(ENFHORS == "yes") %>%
  select(pond, wealth_gift, IDENT, year) %>%
  unique() %>%
  group_by(year)%>%
  mutate(total = sum(pond)) %>%
  group_by(year, wealth_gift)%>%
  summarise(n_gift = sum(pond)/ total)%>% unique()%>%
  filter(wealth_gift == 1)%>%
  inner_join(evol_enfant)%>%
  select(year, n_gift, n_donation, n_donation_enfant)

evol_gift %>%
  pivot_longer(cols = c(n_gift, n_donation, n_donation_enfant)) %>%
  ggplot(aes(x = as.factor(year), colour = name, y = 100*value, group = name)) + 
  geom_point() + 
  geom_line()+ 
  theme_bw() + 
  ylab("% of households") + xlab("") + 
  scale_colour_discrete(name = "Type of financial transfer", labels = c("Donations", "Donations to children", "Wealth gift"))
ggsave(here("04_plots/evol_donations_gifts__2004_2020.png"))

stat_enfhors <- inner_join(evol_enfant, evol_gift)
write.csv2(stat_enfhors, here("01_stat_desc/2004_2020_stat_enfhors.csv"))

#stat desc 

  tbl1 <- survey::svydesign(~1, data = transmission_all %>%filter(ENFHORS == "yes"), weights = ~pond) %>%
    tbl_svysummary(by = donation_dicho, percent = "row", include = c(Age, matrim, quantile100))%>%
    as_kable_extra(format = "latex", booktabs = T)
  
tbl2 <-
  survey::svydesign(~1, data = transmission_all %>%filter(ENFHORS == "yes"), weights = ~pond) %>%
  tbl_svysummary(by = wealth_gift, percent = "row", include = c(Age, matrim, quantile100))%>%
  as_kable_extra(format = "latex", booktabs = T)

# 
# #par age
# global <- transmission_all %>%
#   filter(ENFHORS == "yes") %>%
#   group_by(year) %>% 
#   summarise(prop_donateurs = weighted.mean(donation, w = pond, na.rm = T))
# 
# donation_age <- transmission_all %>%
#   filter(ENFHORS == "yes") %>%
#   group_by(AGEPR) %>% 
#   summarise(prop_donateurs = weighted.mean(donation, w = pond, na.rm = T), 
#             n = sum(pond))

#parmi les femmes célibataires de plus de 60 ans, 18% ont fait une donation

#par decile patrimoine
transmission_decile <- transmission_all%>%
  filter(ENFHORS == "yes", !is.na(PATRI)) %>%
  group_by(IDENT) %>%
  select(PATRI, IDENT, pond, donation, year) %>%
  unique() %>%
  group_by(year) %>%
  mutate(quantile100 = cut(PATRI, unique(quantile(PATRI, w = pond, seq(0, 1, 0.1))), labels = FALSE), 
         quanti_masson = case_when(quantile100 <= 25 ~ "0-25", 
                                   quantile100 > 25 & quantile100 <= 50 ~ "25-50", 
                                   quantile100 > 50 & quantile100 <= 70 ~ "50-70", 
                                   quantile100 > 70 & quantile100 <= 90 ~ "70-90", 
                                   quantile100 > 90 & quantile100 <= 99 ~ "90-99", 
                                   quantile100 > 99 & quantile100 <= 100 ~ "99-100")) 

decile <- transmission_decile %>%
  filter(year != 2004)%>%
  group_by(quantile100, year) %>%
  mutate(total = sum(pond)) %>%
  group_by(quantile100, donation, year)%>%
  summarise(n_donation = sum(pond)/ total)%>% unique()%>%
  filter(donation == 1)

decile%>%
  ggplot(aes(x = quantile100)) +
  geom_histogram(aes(y = 100*n_donation), stat = "identity") + 
  theme_bw() + 
  xlab("") + ylab("") + facet_wrap(~year, ncol = 2)+ 
ggtitle("Pourcentage de donateurs - ménages avec enfants hors-domicile")


#patri brut
patri <- transmission_all%>%
  filter(year == 2017, 
         ENFHORS == "yes", 
         TYPMEN == "no")%>%
  select(PATRI, IDENT, pond, donation, year, Sexe) %>%
  unique() %>%
  mutate(quantile100 = cut(PATRI, unique(quantile(PATRI, 
                                                  w = pond, seq(0, 1, 0.1))), 
                          labels = FALSE))

weighted.quantile(patri$PATRI, w = patri$pond, probs = seq(0, 1, 0.1))


revenu <- transmission_all%>%
  filter(year == 2017, 
         ENFHORS == "yes", 
         TYPMEN == "no", 
         !is.na(ZREVDEC))%>%
  select(ZREVDEC, IDENT, pond, donation, year, Sexe) %>%
  unique() %>%
  mutate(quantile100 = cut(ZREVDEC, unique(quantile(ZREVDEC, 
                                                  w = pond, seq(0, 1, 0.1))), 
                           labels = FALSE))



  summarise(prop_donateurs = weighted.mean(donation, w = pond, na.rm = T), 
            n = sum(pond))

donation_patri %>%
  ggplot(aes(x = quantile100)) +
  geom_histogram(aes(y = 100*prop_donateurs), stat = "identity") + 
  theme_bw() + facet_wrap(~Sexe) + 
  xlab("Décile de patrimoine (brut)") + ylab("") + 
  ggtitle("Pourcentage de donateurs - ménages célibataires avec enfants hors domicile")

#plus les femmes ont du patrimoine, plus elles donnent! 
donation_revenu <- revenu %>%
  group_by(Sexe, quantile100) %>% 
  summarise(prop_donateurs = weighted.mean(donation, w = pond, na.rm = T), 
            n = sum(pond))

donation_revenu %>%
    ggplot(aes(x = quantile100)) +
  geom_histogram(aes(y = 100*prop_donateurs), stat = "identity") + 
  theme_bw() + facet_wrap(~Sexe) + 
  xlab("Décile de revenu (déclaré)") + ylab("") + 
  ggtitle("Pourcentage de donateurs - ménages célibataires avec enfants hors domicile")


