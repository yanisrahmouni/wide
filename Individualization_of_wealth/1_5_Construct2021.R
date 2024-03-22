library("haven")
library("writexl")
library("tidyr")
library("plyr")
library("dplyr")
library("survey")
library("srvyr")
library("questionr")
library("tidyverse")
library("ggplot2")
library("scales")
library("openxlsx")
library("sqldf")

####
# Constitution table ménage ----
####

# On va récupérer les infos sur la situation conjugale (en couple avec une personne vivant dans le logement ou non)

men <- read_dta(paste0(pat21,"/menage.dta")) %>% 
  select(ident,pond_trans,sexepr,sexecj,couplepr) %>%
  mutate(couple=as.integer(couplepr==1)) %>%
  select(-couplepr)

####
# Patrimoine financier ----
####

individu_data <- read_dta(paste0(pat21,"/individu.dta")) %>%
  select(ident, identind20, noi, lienpref, lienprrp, enfrp, sexe) %>%
  # on garde 3 types de liens : PR, CJ ou autres (y compris enfants)
  mutate(liensimpl = case_when(
    lienpref == "00" ~ 1, # personne de référence
    lienpref == "01" ~ 2, # conjoint
    TRUE ~ 3
  )) %>%
  mutate(sexe = ifelse(liensimpl == 3, "0", sexe)) %>% # on met le sexe à 0 si c'est une autre personne que la personne de ref ou conjoint.e
  select(ident, identind20, noi, liensimpl, sexe, lienpref) %>%
  rename(nop = noi) %>% 
  arrange(ident,nop)

# Charger la table produits et sélectionner uniquement les actifs financiers 
produit_data <- read_dta(paste0(pat21,"/produit.dta")) %>%
  filter(nature=="1") %>% 
  select(ident, identprod, starts_with("nop"), identpos, finna, starts_with("mont"), annee)

# /!\ dans 90 % des cas la date d'acquisition est manquante

# Fusionner les données pour obtenir "prodlien" qui va être une table au niveau ménage*produit financier
# autrement dit on joint tous les produits financiers détenus par notre base de ménages de départ
produit <- men %>%
  left_join(produit_data, by = "ident")

# Fusionner les données au niveau produits financiers avec les infos individuelles
produitlien <- produit %>%
  left_join(individu_data, by = c("ident", "nop")) 

# Remplacer la variable liensimpl
produitlien <- produitlien %>%
  # si l'identifiant du produit est 00 (i.e.produit financier détenu en commun) alors on met liensimpl à 0
  mutate(liensimpl=case_when( nop=="00" ~ 0, TRUE ~ liensimpl)) %>% 
  rename(montfi=montcla) %>%
  mutate(montfi=case_when(
    montfi < 0 ~ 0, TRUE ~ montfi # Puisqu'on ne prend pas en compte les dettes pour l'immo, ici non plus pour les produits fi
  )) %>%
  mutate(finna_r=case_when(
    finna == "01" ~ 1,
    finna %in% c("02","03","04","05","06","07") ~ 2,
    finna %in% c("08","09") ~ 3,
    finna %in% c("10") ~ 4,
    finna %in% c("16","17","18","19","20","21") ~ 5,
    finna %in% c("22","23","24","25","26","27","28","29","30") ~ 6,
    finna %in% c("31","32","33","34","35") ~ 7
  ))
# On regroupe les natures de produits financiers dans les grandes catégories Insee
# 01 : compte-chèques
# 02 : livrets 
# 03 : épargne logement
# 04 : épargne salariale
# 05 : assurances vie et épargne retraite
# 06 : valeurs mobilières
# 07 : autres (comptes à terme, comptes courant d'associé, livret d'épargne entreprise, autre produit)

# Regrouper les données et calculer la somme de montfi
result <- produitlien %>%
  group_by(ident, liensimpl, finna_r) %>%
  summarize(montfi = sum(montfi, na.rm=TRUE)) 

for (q in 0:3) {
  df <- result %>% filter(liensimpl == q) %>% ungroup() # Garder les lignes où liensimpl est égal à q
  
  df$finna_r <- as.numeric(df$finna_r)  # Convertir finna en numérique
  df <- df %>% filter(!is.na(finna_r))  # Supprimer les lignes où finna est NA
  
  df <- df %>% 
    pivot_wider(names_from = finna_r, values_from = montfi, names_prefix = "montfi")
  
  
  if (q == 0) {
    df <- df %>% rename_with(~paste0(.x, "comm"), starts_with("montfi")) %>% select(-c("liensimpl"))
  } 
  else if (q == 1) {
    df <- df %>% rename_with(~paste0(.x, "pr"), starts_with("montfi")) %>% select(-c("liensimpl"))
  } 
  else if (q == 2) {
    df <- df %>% rename_with(~paste0(.x, "cj"), starts_with("montfi")) %>% select(-c("liensimpl")) 
  } 
  else if (q == 3) {
    df <- df %>% rename_with(~paste0(.x, "autmen"), starts_with("montfi")) %>% select(-c("liensimpl"))
  }
  
  men <- men %>% left_join(df,by="ident")
}

men <- men %>% 
  mutate(across(starts_with("montfi"), ~ifelse(is.na(.), 0, .)))


# On attribue la moitié de valeur du bien détenu en commun à chaque membre du couple, conjoint et personne de ref,
# lorsqu'il y a des biens détenus en commun
for (i in c(1,2,5,6,7)) {
  men[[paste0("montfi",i,"pr_2")]] <- men[[paste0("montfi",i,"pr")]] + (men[[paste0("montfi",i,"comm")]])/2
  men[[paste0("montfi",i,"cj_2")]] <- men[[paste0("montfi",i,"cj")]] + (men[[paste0("montfi",i,"comm")]])/2
  men <- men[, -which(names(men) == paste0("montfi",i,"pr") )]
  men <- men[, -which(names(men) == paste0("montfi",i,"cj") )]
  colnames(men)[colnames(men) == paste0("montfi",i,"pr_2")] = paste0("montfi",i,"pr")
  colnames(men)[colnames(men) == paste0("montfi",i,"cj_2")] = paste0("montfi",i,"cj")
}

####
# Patrimoine immobilier ----
####

####__Résidence principale ----

rp <- read_dta(paste0(pat21,"/produit.dta"))%>%
  filter(nature == "2" & logna=="01") %>%
  select(ident, parcj, parpr, montcla)

rp <- rp %>%
  rename_with(~paste0(., "_rp"), -c("ident"))

merged_data <- men %>%
  left_join(rp, by = "ident") 


####__Autres biens hors RP (résidences secondaires, parkings, garage...) ----

hors_rp <- read_dta(paste0(pat21,"/produit.dta")) %>%
  filter(nature == "2" & logna !="01") %>%
  select(ident, parcj, parpr, montcla)

# Renommer toutes les variables en ajoutant "_immo" à leur nom (sauf pour ident)
hors_rp <- hors_rp %>%
  rename_with(~paste0(., "_immo"), -c("ident"))

# Créer un identifiant unique pour chaque bien immobilier non RP dans chaque ménage
hors_rp <- hors_rp %>%
  group_by(ident) %>%
  mutate(num = row_number()) %>%
  ungroup()

# Réorganiser les données en format large en utilisant les variables par#, mont#, periode_immo et algacq_immo
hors_rp_wide <- hors_rp %>%
  pivot_wider(names_from = num, values_from = c(starts_with("par"), starts_with("mont")))

merged_data_immo <- merged_data %>%
  left_join(hors_rp_wide , by = "ident")

merged_data_immo <- merged_data_immo %>%
  mutate(across(starts_with("parpr") | starts_with("parcj") | starts_with("paraut"), ~ifelse(is.na(.), 0, .)))

####
# Patrimoine professionnel ----
####

####__Table entreprise ----

ent <- read_dta(paste0(pat21,"/entreprise.dta"))

ent <- ent %>% 
  select(ident, identent, starts_with("chef"), pca, inst, mtentrep) %>%
  rename(acqui_ent=inst)%>%
  mutate(possess_ent = case_when(chef2!="" | chef1=="" ~ "ME",
                                 TRUE ~ chef1)) %>%
  mutate(mtentrep2 = mtentrep * pca / 100) %>%
  # redressement à la main pour tenir compte des enfants qui possèdent aussi une partie de l'entreprise
  mutate(valeur_corr=case_when(
    possess_ent=="ME" & chef1=="01" & chef2=="03" & chef3=="" ~ mtentrep2/2,
    possess_ent=="ME" & chef1=="01" & chef2=="02" & chef3=="03" ~ mtentrep2/3,
    possess_ent=="ME" & chef1=="03" & chef2=="02" & chef3=="01" ~ mtentrep2*2/3,
    TRUE ~ mtentrep2)) %>%
  select(ident, identent, possess_ent, mtentrep2,valeur_corr)
# Si une seule personne dans le ménage détient l'entreprise, possess_ent vaut le numéro identifiant de cette personne
# Si plus d'une personne vaut "ME"

# On somme pour chaque ménage et chaque posesseur le montant des entreprises détenues : on obtient une table au niveau ménage * posesseur
entreprise_summarized <- ent %>%
  group_by(ident, possess_ent) %>%
  summarise(mtentrep2_sum = sum(valeur_corr)) %>%
  rename(nop=possess_ent)

# Merge infos entreprise avec table individuelle 

individu_data <- read_dta(paste0(pat21,"/individu.dta"))

individu_data <- individu_data %>%
  select(ident, identind20, noi, lienpref, lienprrp, enfrp, sexe) %>%
  # on garde 3 types de liens : PR, CJ ou autres (y compris enfants)
  mutate(liensimpl = case_when(
    lienpref == "00" ~ 1, # personne de référence
    lienpref == "01" ~ 2, # conjoint
    TRUE ~ 3
  )) %>%
  mutate(sexe = ifelse(liensimpl == 3, "0", sexe)) %>% # on met le sexe à 0 si c'est une autre personne que la personne de ref ou conjoint.e
  select(ident, identind20, noi, liensimpl, sexe, lienpref) %>%
  rename(nop = noi) %>% 
  arrange(ident,nop)

data_merged <- individu_data %>% 
  left_join(entreprise_summarized, by=c("ident","nop")) %>%
  rename(montant_bienproent=mtentrep2_sum) %>%
  mutate(montant_bienproent_pr = ifelse(liensimpl == 1, montant_bienproent, 0),
         montant_bienproent_cj = ifelse(liensimpl == 2, montant_bienproent, 0),
         montant_bienproent_aut = ifelse(liensimpl == 3, montant_bienproent, 0)) %>%
  mutate(across(starts_with("montant_bienproent"), ~replace(., is.na(.), 0))) 

bienproent_me <- entreprise_summarized %>% filter(nop=="ME") %>% rename(montant_bienproent_me=mtentrep2_sum) %>% select(-nop) 

data_merged <- data_merged %>% 
  left_join(bienproent_me, by="ident") %>% 
  mutate(montant_bienproent_me=ifelse(is.na(montant_bienproent_me),0,montant_bienproent_me))

prof_wealth1 <- data_merged %>%
  group_by(ident) %>%
  summarize(across(starts_with("montant_bienproent"), sum))

####__Table produits ----

# On sélectionne les "Terre ou autre bien professionnel non exploité professionnellement par le ménage" ou 
# "Terre ou autre bien professionnel exploité professionnellement par le ménage"
produits <- read_dta(paste0(pat21,"/produit.dta")) %>%
  filter(nature=="3" | nature=="4") %>%
  select(ident,identpos,nop,profna,parmen,montcla) %>%
  filter(nop=="ME") %>%
  # on ne prend que les biens détenus par le ménage en propre, et non les biens détenus par une entreprise appartenant au ménage
  mutate(montcla2=montcla*parmen/100)

prof_wealth <- produits %>%
  group_by(ident) %>%
  summarise(montant_bienpro_me=sum(montcla2, na.rm=TRUE)) %>%
  mutate(montant_bienpro_me=case_when(is.na(montant_bienpro_me) ~ 0,
                                      TRUE ~ montant_bienpro_me))

merged_data_immo <- merged_data_immo %>% 
  left_join(prof_wealth,by="ident") %>%
  left_join(prof_wealth1,by="ident")


####
# Calcul du montant qui revient à chaque personne ----
####

####__Patrimoine immobilier ----

# Pour RP comme hors RP, on calcule les montants revenant à la personne de référence et à son conjoint

# pour la résidence principale
merged_data_immo <- merged_data_immo %>%
  mutate(resmt=montcla_rp) %>%
  mutate(resmt=ifelse(is.na(resmt),0,resmt)) 

# pour les autres biens immobiliers

for (i in 1:16) {
  merged_data_immo[[paste0("resmt", i)]] <- merged_data_immo[[paste0("montcla_immo_", i)]]
  merged_data_immo[[paste0("resmt",i)]] <- ifelse(is.na(merged_data_immo[[paste0("resmt",i)]]), 0, merged_data_immo[[paste0("resmt",i)]])
}

variables <- c("cj", "pr")
for (s in variables) {
  merged_data_immo[paste0("Wimmo_rp_", s)] <-  (merged_data_immo[, paste0("par", s, "_rp")] / 100) * merged_data_immo$resmt

  
  # Boucle à travers les indices de 1 à 16
  for (k in 1:16) {
    # Mettez à jour la variable "Wimmo_s_strict" en ajoutant les nouvelles valeurs calculées
    merged_data_immo[[paste0("Wimmo_horsrp_", s, "_",k)]] <-  (merged_data_immo[[paste0("par", s, "_immo_", k)]] / 100) * merged_data_immo[[paste0("resmt", k)]]

  }
  
}

# Puis, on agrège les variables : on calcule la richesse liée à la RP, richesse hors RP, et enfin
# richesse immobilière totale. Pour le conjoint comme pour la personne de référence.
merged_data_immo <- merged_data_immo %>% 
  # Ici, on calcule la richesse immobilière hors RP (liée aux biens immo hors RP)
  mutate(Wimmo_horsrp_cj=select(.,starts_with("Wimmo_horsrp_cj_hyp1")) %>% rowSums(na.rm=TRUE)) %>%
  mutate(Wimmo_horsrp_pr=select(.,starts_with("Wimmo_horsrp_pr_hyp1")) %>% rowSums(na.rm=TRUE)) %>%
  # Ici, on calcule la richesse immobilière totale, comme somme de richesse immo RP et hors RP
  mutate(Wimmo_cj=Wimmo_rp_cj + Wimmo_horsrp_cj,
         Wimmo_pr=Wimmo_rp_pr + Wimmo_horsrp_pr) %>%
  select(-c(Wimmo_horsrp_cj_1:Wimmo_horsrp_cj_16),
         -c(Wimmo_horsrp_pr_1:Wimmo_horsrp_pr_16)) %>%
  # dans ce dernier mutate, on remplace les montants des conjoint.es qui valent 0 lorsqu'il ne s'agit pas d'un couple
  # mais d'une personne seule (car sinon fausse les calculs)
  # On croise ensuite avec le sexe de la personne de réf et du conjoint, afin d'obtenir le patrimoine immobilier détenu par la femme du couple et par l'homme du couple
  mutate(Wimmo_h=case_when(
    sexepr=="1" ~ Wimmo_pr,
    sexecj=="1" ~ Wimmo_cj),
    Wimmo_f=case_when(
      sexepr=="2" ~ Wimmo_pr,
      sexecj=="2" ~ Wimmo_cj
      )) %>%
  # On détaille RP et hors RP
  mutate(Wimmo_rp_h=case_when(
    sexepr=="1" ~ Wimmo_rp_pr,
    sexecj=="1" ~ Wimmo_rp_cj),
    Wimmo_rp_f=case_when(
      sexepr=="2" ~ Wimmo_rp_pr,
      sexecj=="2" ~ Wimmo_rp_cj)) %>%
  mutate(Wimmo_horsrp_h=case_when(
    sexepr=="1" ~ Wimmo_horsrp_pr,
    sexecj=="1" ~ Wimmo_horsrp_cj),
    Wimmo_horsrp_f=case_when(
      sexepr=="2" ~ Wimmo_horsrp_pr,
      sexecj=="2" ~ Wimmo_horsrp_cj))

####__Patrimoine financier ----

# On calcule la part des femmes et la part des hommes

# Patrimoine financier sous la forme de compte chèque
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi1_h = ifelse(sexepr == "1", montfi1pr,
                              ifelse(sexecj == "1", montfi1cj, NA)),
         Wfi1_f = ifelse(sexepr == "2", montfi1pr,
                              ifelse(sexecj == "2", montfi1cj, NA))
  )

# Patrimoine financier sous forme de livrets 

merged_data_immo <- merged_data_immo %>%
  mutate(Wfi2_h = ifelse(sexepr == "1", montfi2pr,
                              ifelse(sexecj == "1", montfi2cj, NA)),
         Wfi2_f = ifelse(sexepr == "2", montfi2pr,
                              ifelse(sexecj == "2", montfi2cj, NA)))

# Patrimoine financier sous forme d'épargne logement

merged_data_immo <- merged_data_immo %>%
  mutate(Wfi3_h = ifelse(sexepr == "1", montfi3pr,
                              ifelse(sexecj == "1", montfi3cj, NA)),
         Wfi3_f = ifelse(sexepr == "2", montfi3pr,
                              ifelse(sexecj == "2", montfi3cj, NA))
  ) 


# Patrimoine financier sous forme d'épargne salariale
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi4_h = ifelse(sexepr == "1", montfi4pr,
                              ifelse(sexecj == "1", montfi4cj, NA)),
         Wfi4_f = ifelse(sexepr == "2", montfi4pr,
                              ifelse(sexecj == "2", montfi4cj, NA))
         )
         

# Patrimoine financier sous forme d'assurance vie et épargne retraite
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi5_h = ifelse(sexepr == "1", montfi5pr,
                              ifelse(sexecj == "1", montfi5cj, NA)),
         Wfi5_f = ifelse(sexepr == "2", montfi5pr,
                              ifelse(sexecj == "2", montfi5cj, NA))
  ) 
# Patrimoine financier sous forme de valeurs mobilières
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi6_h = ifelse(sexepr == "1", montfi6pr,
                              ifelse(sexecj == "1", montfi6cj, NA)),
         Wfi6_f = ifelse(sexepr == "2", montfi6pr,
                              ifelse(sexecj == "2", montfi6cj, NA))
  )
         
# Patrimoine financier sous forme d'autres produits financiers
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi7_h = ifelse(sexepr == "1", montfi7pr,
                              ifelse(sexecj == "1", montfi7cj, NA)),
         Wfi7_f = ifelse(sexepr == "2", montfi7pr,
                              ifelse(sexecj == "2", montfi7cj, NA))
  )

# Patrimoine financier total hommes et femmes
merged_data_immo <- merged_data_immo %>%
  mutate(Wfi_h=Wfi1_h+Wfi2_h+Wfi3_h+Wfi4_h+Wfi5_h+Wfi6_h+
           Wfi7_h,
         
         Wfi_f=Wfi1_f+Wfi2_f+Wfi3_f+Wfi4_f+Wfi5_f+Wfi6_f+
           Wfi7_f
         )


####__Patrimoine pro ----

merged_data_immo <- merged_data_immo %>%
  mutate(across(starts_with("montant_bienpro"), ~replace(., is.na(.), 0))) %>%
  mutate(Wproent_tot=montant_bienproent_aut+montant_bienproent_pr+montant_bienproent_cj+montant_bienproent_me,
         Wpro_tot=Wproent_tot+montant_bienpro_me,
         montant_bienpro_me = montant_bienpro_me + montant_bienproent_me)

# Conjoint et personne de référence 
# Hypothèse : on ne tient pas compte du régime matrimonial
merged_data_immo <- merged_data_immo %>%
  mutate(Wpro_pr=case_when(couple==1 ~ (montant_bienproent_pr + montant_bienpro_me/2),
                             couple==0 ~ (montant_bienproent_pr + montant_bienpro_me)),
         Wpro_cj=case_when(couple==1 ~ (montant_bienproent_cj + montant_bienpro_me/2),
         couple==0 ~ montant_bienproent_cj),
         Wpro_au=montant_bienproent_aut)

# On calcule la part des hommes et des femmes
merged_data_immo <- merged_data_immo %>% mutate(Wpro_h=case_when(sexepr=="1" ~ Wpro_pr,
                                                            sexecj=="1" ~ Wpro_cj,
                                                            TRUE ~ NA),
                                      Wpro_f=case_when(sexepr=="2" ~ Wpro_pr,
                                                            sexecj=="2" ~ Wpro_cj,
                                                            TRUE ~ NA))



# Patrimoine immo + financier hommes et femmes 

merged_data_immo <- merged_data_immo %>%
  mutate(W_h=Wfi_h + Wimmo_h,
         W_f=Wfi_f + Wimmo_f,
         W2_h=W_h+Wpro_h,
         W2_f=W_f+Wpro_f) 

# Ne retenir que les variables socio-démo et les variables agrégées
wealth_data <- merged_data_immo %>% select(ident,pond_trans,sexepr,sexecj,couple,starts_with("W") & (ends_with("_h") | ends_with("f")))

# Remplacer les faux 0 par des NA pour le patrimoine financier comme pour les autres
# Pour les personnes ne vivant pas avec un conjoint ou une conjoint.e on remplace par NA les valeurs des montants (qui sont 0)
# pour qu'ensuite ils ne soient pas comptés dans les calculs d'inégalités

wealth_data <- wealth_data %>% 
  mutate(across(contains("W") & contains("_f"), ~ifelse(.==0 & couple==0 & sexepr=="1",NA,.)),
         across(contains("W") & contains("_h"), ~ifelse(.==0 & couple==0 & sexepr=="2",NA,.))) %>%
  select(-starts_with("sexe"),-couple)

saveRDS(wealth_data,"wealth_2021.Rda")

