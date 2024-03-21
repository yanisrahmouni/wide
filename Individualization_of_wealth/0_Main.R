
# BLOC 0 : SET PATHS
# Remplissez les chemins avec vos propres chemins
setwd("E:/Ined formation complémentaire")
dofiles <- "Indiv_pat"  # Remplacez par votre propre chemin
bloc1 <- file.path(dofiles, "BLOC_1_Prepare_data")
bloc2 <- file.path(dofiles, "BLOC_2_Main_results")
bloc3 <- file.path(dofiles, "BLOC_3_Online_Appendix")

indiv <- file.path(dofiles,"indiv")  # Chemin où vont la plupart des jeux de données intermédiaires
results <- file.path(dofiles,"results")  # Chemin où vont certaines tables et jeux de données intermédiaires
wp <- file.path(dofiles,"wp")  # Chemin où vont les tables pour le document principal
appendix <- file.path(dofiles,"appendix")  # Chemin où vont les tables pour l'annexe en ligne

pat98 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/1998/Stata"  # Chemin des données originales de 1998
pat04 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2004/Stata"  # Chemin des données originales de 2004
pat10 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2010/STATA/Methodologie10"  # Chemin des données originales de 2010
pat14 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2014/Stata"  # Chemin des données originales de 2014
pat18 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2018/Stata" # Chemin des données originales de 2018
pat21 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2021/Stata" # Chemin des données originales de 2021

# Changement du répertoire de travail vers indiv
setwd(indiv)

# BLOC 1 : CRÉATION DES JEUX DE DONNÉES
source(file.path(bloc1, "1_1_Construct1998.R"))  # => output: dataset wealth1998
source(file.path(bloc1, "1_2_Construct2004.R"))  # => output: dataset wealth2004
source(file.path(bloc1, "1_3_Construct2010.R"))  # => output: dataset wealth2010
source(file.path(bloc1, "1_4_Construct2014.R"))  # => output: dataset wealth2014

source(file.path(bloc1, "1_5_var_activite_couple.R"))  # => output: cycle_activite_couple
source(file.path(bloc1, "1_6_heritage.R"))  # => output: de nouvelles variables dans wealth1998, wealth2004, wealth2010, wealth2014

source(file.path(bloc1, "1_7_dette.R"))  # => output: dataset debt
source(file.path(bloc1, "1_8_MiseCommun.R"))  # output: wealth_tot_3_def

# BLOC 2 : RÉSULTATS PRINCIPAUX
source(file.path(bloc2, "2_1_statdesc_bs.R"))  # => produit TAB 1 (desc), TAB 2 (indiv of wealth), TAB 3.1 de l'annexe en ligne
source(file.path(bloc2, "2_2_results_decompo_bs.R"))  # => produit TAB 3 (décomposition de theta C) avec IC bootstrap
source(file.path(bloc2, "2_3_results_reg_indiv.R"))  # => produit TAB 4 (régression de l'indiv couples sur les déterminants), TAB 3.3 - TAB 3.4 - TAB 3.5 pour l'annexe en ligne
source(file.path(bloc2, "2_4_results_decompo_inegHF_bs.R"))  # => produit TAB 5: décomposition de GWG avec IC bootstrap
source(file.path(bloc2, "2_5_results_share_indiv_by_distrib_bs.R"))  # => produit TAB 5.1 de l'ANNEXE EN LIGNE, Figure 2 construite à partir de TAB 5.1 de l'ANNEXE EN LIGNE
source(file.path(bloc2, "2_6_comparison_ineq_measures_bs.R"))  # => produit TAB 5.3 de l'ANNEXE EN LIGNE, Figure 3 construite à partir de TAB 5.3 de l'ANNEXE EN LIGNE
source(file.path(bloc2, "2_7_gwg_by_status_bs.R"))  # => produit TAB 5.9 de l'ANNEXE EN LIGNE, utilisé pour construire FIG 4 du DOCUMENT PRINCIPAL, produit TAB 5.10 de l'ANNEXE EN LIGNE, utilisé pour construire FIG 4 de l'ANNEXE EN LIGNE

source(file.path(bloc2, "2_8_reg_GWG_within_couples.R"))  # => produit TAB 6 et 7, produit TABLES 5.11, 5.12 et 5.13 de l'ANNEXE EN LIGNE

# BLOC 3 : ANNEXE EN LIGNE
# Préparation d'autres jeux de données pour les vérifications de robustesse
source(file.path(bloc3, "3_1_MiseCommun_defstricte.R"))  # => output: dataset wealth_tot_3_defstricte
source(file.path(bloc3, "3_2_MiseCommun_robusthousing.R"))  # => output: dataset wealth_tot_3robhousing
source(file.path(bloc3, "3_3_MiseCommun_keeptop.R"))  # => output: dataset wealth_tot_3top
source(file.path(bloc3, "3_4_MiseCommun_robustpro.R"))  # => output: dataset wealth_tot_3rob2
source(file.path(bloc1, "1_1_Construct1998.R"))  # Same as bloc 1
source(file.path(bloc1, "1_2_Construct2004.R"))  # Same as bloc 1
source(file.path(bloc3, "3_5_Construct2010_simulpacs.R"))
source(file.path(bloc3, "3_6_Construct2014_simulpacs.R"))
source(file.path(bloc1, "1_5_var_activite_couple.R"))  # Same as bloc 1
source(file.path(bloc1, "1_6_heritage.R"))  # Same as bloc 1
source(file.path(bloc1, "1_7_dette.R"))  # Same as bloc 1
source(file.path(bloc3, "3_7_MiseCommun_simulpacs.R"))  # => output: wealth_tot_3_simulpacs

# Résultats de l'annexe en ligne
source(file.path(bloc3, "3_8_rob_statdesc_defstricte_bs.R"))  # => produit TAB 2.1 de l'annexe en ligne (définition alternative), FIG 1 de l'annexe en ligne basée sur TAB 2.1
source(file.path(bloc3, "3_9_rob_statdesc_pacs_bs.R"))  # => produit TAB 2.5, 3.2, 2.3, 2.4 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_10_rob_statdesc_agegroups_bs.R"))  # => produit TAB 2.6, 2.7, 2.8 dans l'ANNEXE EN LIGNE, FIG 2 basée sur ces tableaux dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_11_rob_statdesc_cohort_bs.R"))  # => produit TAB 2.9, 2.10 et 2.11 dans l'ANNEXE EN LIGNE, FIG 3 basée sur ces tableaux dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_12_statdesc_compo.R"))  # => produit TAB 2.12 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_13_rob_statdesc_housing_bs.R"))  # => produit TAB 4.1 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_14_rob_statdesc_withoutpro_bs.R"))  # => produit TAB 4.2 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_15_rob_statdesc_net_bs.R"))  # => produit TAB 4.3 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_16_rob_statdesc_keeptop_bs.R"))  # => produit TAB 4.4 dans l'ANNEXE EN LIGNE
source(file.path(bloc3, "3_17_comparison_ineq_measures_byagegroups_bs.R"))  # => produit TAB 5.6, 5.7, 5.8
source(file.path(bloc3, "3_18_comparison_ineq_measures_bycohorts_bs.R"))  # => produit TAB 5.3, 5.4, 5.5

# Quitter R
q()
