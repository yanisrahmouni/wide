#stat donateurs sur 2004-2020 
library(tidyverse)
library(here)
library(spatstat)
library(gtsummary)
library(survey)

transmission_all <- read.csv2(here("03_cleant_data/2004_2020_donateurs.csv"))%>%
  mutate(Age =  factor(Age, levels = c( "<30", "30-45", "46-60", "61-80", ">80")), 
         quantile100 = as.factor(quantile100), 
         donation_enfant_hod = as.numeric(donation_enfant_hod), 
         mean_montant = as.numeric(mean_montant), 
         n_donation = as.numeric(n_donation))%>%
  filter(year > 2004)

###difference femmes/ hommes: on restreint aux menages celibataires######
big_df_femmes_hommes <- transmission_all %>%
  filter(TYPMEN == "no", ENFHORS == "yes") %>%
  group_by(Sexe, year) %>%
  mutate(n_sex = sum(pond), 
         pond = pond) %>%
  group_by(Sexe, donation_dicho, year) %>%
  summarise(n_donation_sex = sum(pond)/ n_sex)%>%unique()%>%
  filter(donation_dicho == 1) %>%
  ungroup()

diff_donation_enfant <- transmission_all %>%
  filter(TYPMEN == "no", ENFHORS == "yes") %>%
  group_by(Sexe, year) %>%
  mutate(n_sex = sum(pond), 
         pond = pond) %>%
  group_by(Sexe, donation_dicho, year) %>%
  summarise(donation = sum(pond)/n_sex, 
            donation_dicho = donation_dicho)%>%unique()%>%
  filter(donation_dicho==1)

diff_donation_enfant2<- transmission_all %>%
  filter(TYPMEN == "no", donation_dicho ==1, !is.na(donation_enfant)) %>%
  group_by(Sexe, year) %>%
  mutate(n_sex = sum(pond), 
         pond = pond) %>%
  group_by(Sexe, donation_enfant, year) %>%
  summarise(n_donation_enfant = sum(pond)/n_sex, 
            donation_enfant = donation_enfant)%>%unique()%>%
  filter(donation_enfant==1)

diff_donation_enfant3<- transmission_all %>%
  filter(TYPMEN == "no", donation_dicho ==1, !is.na(donation_enfant)) %>%
  group_by(Sexe) %>%
  mutate(n_sex = sum(pond), 
         pond = pond) %>%
  group_by(Sexe, donation_autre, year) %>%
  summarise(n_donation_autre = sum(pond)/n_sex, 
            donation_autre = donation_autre)%>%unique()%>%
  filter(donation_autre==1)

final_check <- inner_join(diff_donation_enfant, inner_join(diff_donation_enfant2, diff_donation_enfant3))
#######

#autre option 

tbl1_sex_donation <- survey::svydesign(~1, data = transmission_all %>%    
                                         filter(TYPMEN == "no", ENFHORS == "yes"), weights = ~pond) %>%
  tbl_svysummary(by = Sexe, percent = "column", 
                 include = c(donation_dicho, donation_enfant_hod, aide_hod))

tbl1_sex_children <- survey::svydesign(~1, data = transmission_all %>%    
                                         filter(TYPMEN == "no", ENFHORS == "yes", 
                                                donation_dicho ==1)
                                       , weights = ~pond) %>%
  tbl_svysummary(by = Sexe, percent = "column", 
                 include = c(donation_enfant_hod, donation_autre, n_donation, mean_montant, bien_prof:finance), 
                 type = c(n_donation = "continuous"), 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 missing = "no")


  tbl_stack(list(tbl1_sex_donation, tbl1_sex_children))%>%
  as_kable_extra(format = "latex", booktabs = T)


# 
# final_check %>%
#   pivot_longer(cols = c(donation, n_donation_autre, n_donation_enfant)) %>%
#   ggplot(aes(x = as.factor(year), colour = name, y = 100*value, group = name)) + 
#   geom_point() + 
#   geom_line()+ 
#   theme_bw() + 
#   ylab("% of households") + xlab("") + facet_grid(~Sexe)
#   scale_colour_discrete(name = "Type of financial transfer", labels = c("Donations", "Donations to children", "Wealth gift"))
# ggsave(here("04_plots/evol_donations_gifts__2004_2020.png"))

#difference de montant

big_df_femmes_hommes_montant <- transmission_all %>%
  filter(TYPMEN == "no", ENFHORS == "yes", donation_dicho == 1, mean_montant != 0,
         mean_montant != NaN) %>%
  group_by(Sexe, year) %>%
  mutate(mean_montant = as.numeric(mean_montant)) %>%
  summarise(mean_montant = weighted.mean(mean_montant, w = pond),
            n_montant= n())%>%
  ungroup()

evol_gift <- transmission_all %>%
  filter(ENFHORS == "yes", ENFHORS == "yes") %>%
  select(pond, wealth_gift, IDENT, year, Sexe) %>%
  unique() %>%
  group_by(year, Sexe)%>%
  mutate(n_sex = sum(pond),
         pond = pond) %>%
  group_by(Sexe, wealth_gift, year) %>%
  summarise(n_gift_sex = sum(pond)/ n_sex,
            n_sex = n())%>%unique()%>%
  filter(wealth_gift == 1) %>%
  ungroup()

stat_single <- full_join(big_df_femmes_hommes, big_df_femmes_hommes_montant)%>%
  select(-donation_dicho)%>%
  inner_join(evol_gift)

##plots 
stat_single %>% 
  filter(year > 2004) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, color = Sexe, fill = Sexe)) + 
  geom_col(aes(y = 100*n_donation_sex), position = "dodge") + 
  geom_text(aes(y = 100*n_donation_sex, label = scales::percent(n_donation_sex)), 
            vjust = -0.5, position = position_dodge(.7), 
            size = 3)+ 
  geom_line(aes(y = mean_montant/1000, group = Sexe)) + 
  geom_point(aes(y = mean_montant/1000)) + 
  theme_bw() + 
  ylab("Montant moyen des donations (en milliers d'euros)") + 
  xlab("") +   
  labs(caption = "Champ: ménages célibataires avec enfants hors-domicile")
ggsave(here("04_plots/evol_donation_celib.png"))


big_df_femmes_hommes_gift %>%
  pivot_longer(cols = c(n_gift_sex, n_donation_sex)) %>%
  ggplot(aes(x = year, colour = name, y = 100*value, group = name)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  theme_bw() + facet_grid(~Sexe)+ 
  ylab("% de ménages") + xlab("") + 
  labs(caption = "Champ: ménages célibataires avec enfants hors-domicile")+ 
  scale_colour_discrete(name = "Type de transfert", labels = c("Donations", "Aides"))
ggsave(here("04_plots/evol_don_gift_2004_2020.png"))