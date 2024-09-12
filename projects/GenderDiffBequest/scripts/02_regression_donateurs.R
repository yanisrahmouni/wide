#regressions
library(tidyverse)
library(here)
library(ggeffects)
library(gtsummary)

###loading data####
#faire des modèles linéaires 
transmission_all <- read.csv2(here("03_cleant_data/2004_2020_donateurs.csv"))%>%
  mutate(MATRIPR = as.factor(MATRIPR), 
         year = as.factor(year), 
         donation = as.integer(donation))

test_risk_2014 <- read.csv2(here("00_data/2014/Csv/MENAGE.csv"))%>%
  select(IDENT, 
         RISKFI) %>%
  mutate(year = "2014", 
         RISKFI = as.factor(RISKFI))%>%
  unique() %>%
  filter(!is.na(RISKFI))%>%
  inner_join(transmission_all)%>%
  filter(TYPMEN == "no", ENFHORS == "yes")%>%
  unique()%>%
  filter(!is.na(RISKFI))

###models####
#ajouter cat age 60-70, 70-80, >80
M1 <- transmission_all%>%
  filter(TYPMEN == "no", ENFHORS == "yes")%>%
  glm(formula = donation ~ Sexe + year, family = binomial(link = "logit"), 
      data = .)%>%
  tbl_regression(exponentiate = T) %>%
  add_global_p() %>%
  bold_p() 

M2 <- transmission_all%>%
  filter(TYPMEN == "no", ENFHORS == "yes")%>%
  glm(formula = donation ~ Sexe + AGEPR + year, family = binomial(link = "logit"), 
      data = .)%>%
  tbl_regression(exponentiate = T) %>%
  add_global_p() %>%
  bold_p() 

#quand on ajoute l'âge, les hommes donnent plus

M3 <- transmission_all%>%
  filter(TYPMEN == "no", ENFHORS == "yes")%>%
  glm(formula = donation ~ Sexe + AGEPR  + PATRI + 
        Sexe*PATRI, family = binomial(link = "logit"), 
      data = .)%>%
  tbl_regression(exponentiate = T) %>%
  add_global_p() %>%
  bold_p() 

stat_desc_risk <- test_risk_2014 %>%
  group_by(Sexe)%>%
  mutate(total = n())%>%
  group_by(Sexe, RISKFI) %>%
  summarise(percent = scales::percent(n()/ total)) %>% unique() %>%
  pivot_wider(names_from = Sexe, values_from = percent)

stat_desc_risk2 <- test_risk_2014 %>%
  group_by(donation)%>%
  mutate(total = n())%>%
  group_by(donation, RISKFI) %>%
  summarise(percent = scales::percent(n()/ total)) %>% unique() %>%
  pivot_wider(names_from = donation, values_from = percent)

M1_risk <- glm(formula = donation ~ Sexe + AGEPR + MATRIPR + RISKFI*Sexe + PATRI, family = binomial(link = "logit"), 
               data = test_risk_2014)%>%
  tbl_regression(exponentiate = T) %>%
  add_global_p() %>%
  bold_p() 

final_stat_reg <- tbl_merge(list(M1, M2, M3, M1_risk), 
                            tab_spanner = c("**M1**", "**M2**", "**M3**", "**M4**"))%>%
  as_flex_table()

flextable::save_as_docx(final_stat_reg, path = here("03_cleant_data/final_stat_reg.docx"))



ggpredict(M3, terms = c("PATRI","Sexe"))%>%
  plot() + 
  labs(
    x = "Gender of the house owner",
    y = "",
    title = "Predicted probability of being anaemic", 
    colour = "Sex"
  ) + 
  scale_color_sjplot("quadro") + 
  theme_bw(base_size=15) + 
  theme(legend.position = "bottom")
