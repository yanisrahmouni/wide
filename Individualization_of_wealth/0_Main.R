
# Remplissez les chemins avec vos propres chemins
setwd("E:/Ined formation complémentaire")
dofiles <- "Indiv_pat"  # Remplacez par votre propre chemin

indiv <- file.path(dofiles,"indiv")  # Chemin où vont la plupart des jeux de données intermédiaires
results <- file.path(dofiles,"results")  # Chemin où vont certaines tables et jeux de données intermédiaires
wp <- file.path(dofiles,"wp")  # Chemin où vont les tables pour le document principal
appendix <- file.path(dofiles,"appendix")  # Chemin où vont les tables pour l'annexe en ligne

pat98 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/1998/Stata"  # Chemin des données originales de 1998
pat04 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2004/Stata"  # Chemin des données originales de 2004
pat10 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2010/STATA/Methodologie10"  # Chemin des données originales de 2010
pat15 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2014/Stata"  # Chemin des données originales de 2014
pat19 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2018/Stata" # Chemin des données originales de 2018
pat21 <- "E:/Enquêtes Patrimoine/Quetelet (Mathis)/pat_dta/2021/Stata" # Chemin des données originales de 2021

# Changement du répertoire de travail vers indiv
setwd(indiv)

