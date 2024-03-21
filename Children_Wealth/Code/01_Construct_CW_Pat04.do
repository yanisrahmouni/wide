********************************************************************************
***                                                                          ***
***                         2004 DATA SCRIPT                                 ***
***     This script is designed to prepare data from the 2004 survey.        ***
***                   LETURCQ / SANSU - 11/05/2023                           ***
***                                                                          ***
********************************************************************************

set more off

cap erase $temp\pr_cj.dta
cap erase $temp\pr_cj_men.dta
cap erase $temp\compo_men.dta
cap erase $temp\assets_pr_cj.dta
cap erase $temp\assets_children.dta
cap erase $temp\children.dta


** LIEN + ENF = statut ENF 
** HODLN1-12 = statut ENFANT HORS MENAGE + sexe + age

*** en 2004 : les revenus (source fiscale) sont dans une base à part 
*** => on commence par les mettre avec la base INDIVIDU

use $pat04\revenus2004, clear

drop if identind == ""
duplicates drop
keep identmen identind noi zsali zchoi ztsai zrsti zalri zreti ///
zrtoi zperi zragi zrici zrnci
so identind
tempfile revfiscal
save `revfiscal', replace

use $pat04\INDIVIDU, clear
so identind
merge 1:1 identind using `revfiscal'
drop if _m == 2
drop _m
recode z* (. = 0)
save $temp\INDIVIDU2004, replace

***** repérage des assets

* on traite séparément les individus ENFANTS et les indiv PR/CJ
* on traite séparément les types d'assets

use $temp\INDIVIDU2004, clear
gen PR = (lien == "1"|lien == "2")
gen enfant_PR = (lien == "3" & enf != "4")
gen autre_enf_PR = (PR == 0 & enfant_PR == 0 & age < 25)
gen autre_PR = (PR == 0 & enfant_PR == 0 & age >= 25)
collapse (sum) PR enfant_PR autre_enf_PR autre_PR, by(ident)
so ident
save $temp\compo_men, replace 


***** 1/ ENFANTS 

** 1.0/ identification des enfants (mineurs)

use $temp\INDIVIDU2004, clear
keep if age <= 17
gen PR = (lien == "1" | lien == "2")
gen enfant_PR = (lien == "3" & enf != "4")
drop if enfant_PR == 0 
gen enfant_couple  = (lien == "3" & enf == "1")
gen enfant_PR_seul = (lien == "3" & enf == "2") 
gen enfant_CJ_seul = (lien == "3" & enf == "3")  

keep ident identind age sexe enfant_PR enfant_couple enfant_PR_seul ///
enfant_CJ_seul occupa jepprof jemprof cyfran posit viecou matri etudi ///
gparpat gparmat herdon_i herdon1_i herdon2_i na1 tio paysnai anais pere ///
mere statut cs dieg diep dies diplo pond z* herdon1_i   herdon2_i ///
gestsep_i duree 

tempfile children
so identind
save $temp\children, replace


** 1.1/ assets financiers des enfants

use $pat04\PRODUIT, clear
ren identpos identind
so identind
merge m:1 identind using $temp\children
keep if _m == 3 | _m == 2
destring finna, replace 
recode mtsimul (. = 0)
recode mtsimul (999999990/9999999999 = .)
gen Wfin_cc      = (finna == 1)               * mtsimul
gen Wfin_epargne = (finna > 1 & finna <= 9)   * mtsimul
gen Wfin_nonep   = (finna >= 10 & finna < 32) * mtsimul

gen has_compteCh    = (finna == 1)
gen has_livA        = (finna == 2 | finna == 3 | finna == 4| finna == 5)
gen has_livJeune    = (finna == 6)
gen has_livImpot    = (finna == 7)
gen has_EpLog       = (finna == 8 | finna == 9)
gen has_AssuVie     = (finna == 17)
gen has_other_asset = (finna > 9 & finna != 17 & finna != .)

collapse (sum) Wfin_cc Wfin_epargne Wfin_nonep has_*, by(identind)
recode has* (2 / max = 1)
tempfile fin_enf
so identind
save `fin_enf'

** 1.2/ assets immobilier des enfants 

use $pat04\PRODUIT, clear
keep if nature == "2"
ren algacq acqdet
keep ident identprod mtsimul paraut acqdet
ren mtsimul immo_valeur_brute
so ident
merge m:1 ident using $temp\compo_men
gen total_autre_PR = enfant_PR + autre_PR 

* if other people in the hh, older than 25, who is not a child of PR and CJ,
* we attribute all wealth to this person => minimize wealth of children

gen Wimmo_brut_enf = immo_valeur_brute * (paraut / total_autre_PR) /100 if autre_PR == 0 
drop if enfant_PR == 0
recode Wimmo_brut_enf (. = 0)
gen exist_dette_immo = (acqdet == "1") 
collapse (sum) Wimmo_brut_enf immo_valeur_brute exist_dette_immo, by(ident)
so ident
tempfile immo
save `immo', replace

*** Cas 1: on arrive à retrouver le logement pour lequel l'emprunt a été contracté 
*** => pas possible en 2010, car la dette n'est pas associée à un logement

*** Cas 2: on n'arrive pas à retrouver le logement pour lequel l'emprunt a été contracté

use $pat04\produit_origine, clear
keep if nature == "6" & (detqua== "01" | detqua == "02")
keep ident montcla kdu e_montcla e_kdu
recode montcla kdu (999999998/999999999 = .)
replace montcla = montcla / 6.5596 if e_montcla == "2"
replace kdu = kdu / 6.5596 if e_kdu == "2"
recode montcla kdu (. = 0)
collapse (sum) montcla kdu, by(ident)
ren montcla valeur_dette
so ident
merge 1:1 ident using `immo'
drop if _m == 1
drop _m
gen immo_valeur_nette = immo_valeur_brute - kdu
gen Wimmo_net_enf = Wimmo_brut_enf - (Wimmo_brut_enf / immo_valeur_brute) * kdu
recode exist_dette_immo (1/11 = 1)
recode immo_valeur_nette  Wimmo_net_enf (. = 0)

collapse (sum) Wimmo_brut_enf Wimmo_net_enf exist_dette_immo, by(ident)
replace exist_dette_immo = 0 if Wimmo_brut_enf == 0
so ident
tempfile immo_enf
save `immo_enf'

use $temp\children, clear
so ident
merge m:1 ident using `immo_enf'
drop if _m == 2 
drop _merge
so identind
merge 1:1 identind using `fin_enf'
drop _m
gen Wtot =  Wimmo_net_enf + Wfin_epargne + Wfin_nonep

** 1.3/ assets professionnels
* => on fait l'hypothèse que les biens professionnels détenus 
* par des enfants vivants chez leurs parents est négligeable
* => on ne tient pas comtpe des dettes professionnelles du coup

so ident identind
save $temp\assets_children, replace
 
*annee finna logna montcla nature pond profna


***** 2/ PR et CJ (parents des enfants)

** 2.0/ identification des PR et CJ

use $temp\INDIVIDU2004, clear
gen PR = (lien == "1" | lien == "2")
gen enfant_PR = (lien == "3")
drop if PR == 0
keep ident identind age sexe PR lien noi occupa cyfran posit viecou matri ///
etudi gparpat gparmat herdon_i herdon1_i herdon2_i na1 tio paysnai anais ///
pere mere statut cs dieg diep dies diplo pond z* jefrso jeargt jegrave ///
jepprof jemact jemprof jepro_* ///
gestsep_i duree 
so identind 
save $temp\pr_cj, replace

use $temp\pr_cj, clear
keep ident age sexe PR lien noi occupa cyfran posit viecou matri ///
etudi gparpat gparmat herdon_i herdon1_i herdon2_i na1 tio paysnai anais ///
pere mere statut cs dieg diep dies diplo pond z* jefrso jeargt jegrave ///
jepprof jemact jemprof jepro_* ///
gestsep_i duree 
destring lien, replace

reshape wide age sexe PR noi occupa cyfran posit viecou matri ///
etudi gparpat gparmat herdon_i herdon1_i herdon2_i na1 tio paysnai anais ///
pere mere statut cs dieg diep dies diplo pond z* jefrso jeargt jegrave ///
jepprof jemact jemprof jepro_* ///
gestsep_i duree , i(ident) j(lien)

foreach v in age sexe PR noi occupa cyfran posit viecou matri ///
etudi gparpat gparmat herdon_i herdon1_i herdon2_i na1 tio paysnai anais ///
pere mere statut cs dieg diep dies diplo pond z* jefrso jeargt jegrave ///
jepprof jemact jemprof jepro_* ///
gestsep_i duree {
	ren `v'1 `v'_pr
	ren `v'2 `v'_cj
}

so ident 
save $temp\pr_cj_men,replace


** 2.1/ assets financiers PR et CJ 

**** 2.1.1/ assets financiers PR et CJ détenu en biens propres

use $pat04\PRODUIT, clear
ren identpos identind
so identind
merge m:1 identind using $temp\pr_cj
keep if _m == 3 | _m == 2

destring finna, replace 
recode mtsimul (. = 0)

gen Wfin_cc      = (finna == 1)               * mtsimul
gen Wfin_epargne = (finna > 1 & finna <= 9)   * mtsimul
gen Wfin_nonep   = (finna >= 10 & finna < 32) * mtsimul

collapse (sum) Wfin_epargne Wfin_nonep Wfin_cc, by(ident lien)
destring lien, replace

reshape wide Wfin_cc Wfin_epargne Wfin_nonep, i(ident) j(lien)
ren Wfin_cc1 Wfin_cc_pr
ren Wfin_epargne1 Wfin_epargne_pr  
ren Wfin_nonep1 Wfin_nonep_pr
ren Wfin_cc2 Wfin_cc_cj
ren Wfin_epargne2 Wfin_epargne_cj  
ren Wfin_nonep2 Wfin_nonep_cj

tempfile fin_pr_cj
so ident
save `fin_pr_cj'

**** 2.1.2/ assets financiers PR et CJ détenus conjointement

use $pat04\PRODUIT, clear
keep if nature == "1" & nop == "00"
keep ident finna montcla mtsimul
so ident 
merge m:1 ident using $temp\pr_cj_men
drop if _m != 3
drop _m

* en 2004, on ne connait pas nop1 et nop2 (qui détient conjointement le compte)
* => on fait l'hypothèse que ce sont les membres du couple 

* Cas 1 : détenu par PR et CJ conjointement

gen W_fin_conj_pr = mtsimul / 2 
gen W_fin_conj_cj = mtsimul / 2 

destring finna, replace
gen Wfin_cc_pr      = (finna == 1)               * W_fin_conj_pr
gen Wfin_epargne_pr = (finna > 1 & finna <= 9)   * W_fin_conj_pr
gen Wfin_nonep_pr   = (finna >= 10 & finna < 32) * W_fin_conj_pr
gen Wfin_cc_cj      = (finna == 1)               * W_fin_conj_cj
gen Wfin_epargne_cj = (finna > 1 & finna <= 9)   * W_fin_conj_cj
gen Wfin_nonep_cj   = (finna >= 10 & finna < 32) * W_fin_conj_cj

collapse (sum) Wfin_*, by(ident)
append using `fin_pr_cj'

collapse (sum) Wfin_*, by(ident)
save `fin_pr_cj', replace


** 2.2/ assets immobilier des PR et CJ

use $pat04\PRODUIT, clear
keep if nature == "2"
ren algacq acqdet
keep ident identprod montcla parpr parcj parmen paraut acqdet mtsimul e_montmin

ren mtsimul immo_valeur_brute
so ident
merge m:1 ident using $temp\compo_men

gen Wimmo_brut_pr = immo_valeur_brute * (parpr) / 100
gen Wimmo_brut_cj = immo_valeur_brute * (parcj) / 100

drop if PR == 0
recode Wimmo_brut_pr Wimmo_brut_cj (. = 0)
gen exist_dette_immo = (acqdet == "1") 
collapse (sum) Wimmo_brut_pr Wimmo_brut_cj immo_valeur_brute exist_dette_immo, by(ident)
so ident
tempfile immo
save `immo', replace

*** Cas 1: on arrive à retrouver le logement pour lequel l'emprunt a été contracté
* => pas possible en 2004, car la dette n'est pas associée à un logement

*** Cas 2: on n'arrive pas à retrouver le logement pour lequel l'emprunt a été contracté

* If other people in the hh who is not a child, we attribute all wealth to this person 
* => minimize wealth of children
*gen Wimmo_net_enf = immo_valeur_nette * (paraut / total_autre_PR) / 100 if autre_PR == 0 

use $pat04\PRODUIT_origine, clear
keep if nature == "6" & (detqua == "01" | detqua == "02")
keep ident montcla kdu e_montcla
recode montcla kdu (999999998 999999999 = .)
replace montcla = montcla / 6.5596 if e_montcla == "2"
replace kdu     = kdu    / 6.5596  if e_montcla == "2"
collapse (sum) montcla kdu, by(ident)
ren montcla valeur_dette
so ident
merge 1:1 ident using `immo'
drop if _m == 1
drop _m
recode kdu (.=0)
gen immo_valeur_nette = immo_valeur_brute - kdu
gen Wimmo_net_pr = Wimmo_brut_pr - (Wimmo_brut_pr / immo_valeur_brute) * kdu
gen Wimmo_net_cj = Wimmo_brut_cj - (Wimmo_brut_cj / immo_valeur_brute) * kdu
recode exist_dette_immo (1/11 = 1)
recode immo_valeur_nette  Wimmo_net_* (.=0)

collapse (sum) Wimmo_brut_* Wimmo_net_* exist_dette_immo, by(ident)
so ident
tempfile immo_pr_cj
save `immo_pr_cj'


** 2.3 Business assets 

* 2.3.1 Actifs
use $pat04\PRODUIT, clear
keep if nature == "3" |nature == "4"
keep identprod ident identpos montmin montmax nature parhm parpr parcj parmen ///
					 profna mtsimul pond npoids
* attention, dans la table PRODUIT: les variables mtsimul montmin montmax ont toutes été converties en euros => pas besoin de convertir même si montants donnés initialmenet en francs
recode parmen (999 = .)
gen Wpro_brut_pr   = parpr  * mtsimul / 100 if parmen == .
gen Wpro_brut_cj   = parcj  * mtsimul / 100 if parmen == .
gen Wpro_brut_hm   = parhm  * mtsimul / 100 if parmen == .
gen Wpro_brut_prcj = parmen * mtsimul / 100 if parmen != .
recode Wpro_brut_* (. = 0)
collapse (sum) Wpro* mtsimul, by(ident)
so ident 
tempfile pro
save `pro'

* 2.3.1 Passifs
use $pat04\PRODUIT_origine, clear
* il faut revenir à la table d'origine pour avoir les dettes 
* attention : ici aussi, les montants sont donnés en euros => pas besoin de convertir
keep if nature == "6" & (natemp == "05" | natemp == "06")
keep ident montcla kdu e_montcla
recode montcla kdu (999999998 999999999 = .)
*replace montcla = montcla/6.5596 if e_montcla == "2"
*replace kdu = kdu/6.5596 if e_montcla == "2"
collapse (sum) montcla kdu , by(ident)
ren montcla valeur_dette_prof
ren kdu kdu_prof
so ident
merge 1:1 ident using `pro'
drop _m
recode kdu (. = 0)

egen pro_valeur_brute =  rsum(Wpro_brut_pr Wpro_brut_cj Wpro_brut_prcj)
replace Wpro_brut_pr = Wpro_brut_pr + Wpro_brut_prcj * 0.5
replace Wpro_brut_cj = Wpro_brut_cj + Wpro_brut_prcj * 0.5
gen pro_valeur_nette = pro_valeur_brute - kdu
gen Wpro_net_pr = Wpro_brut_pr - (Wpro_brut_pr /(Wpro_brut_pr + Wpro_brut_cj))* kdu
gen Wpro_net_cj = Wpro_brut_cj - (Wpro_brut_cj /(Wpro_brut_pr + Wpro_brut_cj))* kdu
gen Wpro_brut = Wpro_brut_pr + Wpro_brut_cj
gen Wpro_net  = Wpro_net_pr  + Wpro_net_cj
recode pro_valeur_brute pro_valeur_nette Wpro_net_* (. = 0)
collapse (sum) Wpro_brut Wpro_net Wpro_brut_* Wpro_net_*, by(ident)

so ident 
tempfile pro_pr_cj
save `pro_pr_cj'

* 2.4 Autres dettes
use $pat04\PRODUIT_origine, clear
* il faut revenir à la table d'origine pour avoir les dettes 
* attention : ici aussi, les montants sont donnés en euros => pas besoin de convertir
keep if nature == "6" & (detqua == "04" | detqua == "05" | detqua == "06")
keep ident montcla kdu e_montcla
recode montcla kdu (999999998 999999999 = .)
*replace montcla = montcla/6.5596 if e_montcla == "2"
*replace kdu = kdu/6.5596 if e_montcla == "2"
collapse (sum) montcla kdu , by(ident)
ren montcla valeur_dette_autre
ren kdu kdu_dette_autre
so ident 
tempfile dette_pr_cj
save `dette_pr_cj'

* 2.5 Autres créances (prêts)
use $pat04\PRODUIT_origine, clear
* il faut revenir à la table d'origine pour avoir les dettes 
* attention : ici aussi, les montants sont donnés en euros => pas besoin de convertir
keep if nature == "5" 
keep ident montcla kdu e_montcla
recode montcla kdu (999999998 999999999 = .)
*replace montcla = montcla/6.5596 if e_montcla == "2"
*replace kdu = kdu/6.5596 if e_montcla == "2"
collapse (sum) montcla kdu , by(ident)
ren montcla valeur_creance_autre
ren kdu kdu_creance_autre
so ident
tempfile creance_pr_cj
save `creance_pr_cj'


** 2.6 Merge assets of PR and CJ

use $temp\pr_cj_men, clear
so ident
merge 1:1 ident using `immo_pr_cj'
drop _m
merge 1:1 ident using `fin_pr_cj'
drop _m
merge 1:1 ident using `pro_pr_cj'
drop _m
merge 1:1 ident using `dette_pr_cj'
drop _m
merge 1:1 ident using `creance_pr_cj'

drop _m
so ident
save $temp\assets_pr_cj,replace


***** 3/ MERGE assets of children with assets of parents

use $temp\compo_men, clear
so ident
merge 1:1 ident using $temp\assets_pr_cj
drop _m
so ident
merge 1:m ident using $temp\assets_children
drop if _m == 1
* on vire les couples homo (3 HH et 0 FF)
drop if sexe_pr == sexe_cj
drop _m
foreach v in noi age Wimmo_brut Wimmo_net Wfin_cc Wfin_epargne Wfin_nonep ///
					 Wpro_brut Wpro_net occupa cyfran posit viecou matri ///
					 etudi gparpat gparmat herdon_i herdon1_i herdon2_i na1 ///
					 tio paysnai anais pere mere statut cs dieg diep dies ///
					 diplo pond zsali zchoi ztsai zrsti zalri zreti zrtoi ///
					 zperi zragi zrici zrnci jefrso jeargt jegrave jepprof ///
					 jemact jemprof jepro_a jepro_b jepro_c jepro_d jepro_e ///
					 jepro_f jepro_g ///
gestsep_i duree {
destring `v'*,replace
gen `v'_h =.
gen `v'_f =.
replace `v'_h = `v'_pr if sexe_pr == "1" 
replace `v'_h = `v'_cj if sexe_cj == "1" 
replace `v'_f = `v'_pr if sexe_pr == "2"
replace `v'_f = `v'_cj if sexe_cj == "2" 
}
recode Wimmo* Wfin* Wpro* kdu_creance_autre kdu_dette_autre  (. = 0)
*gen Wtot_h = Wimmo_net_h + Wfin_epargne_h + Wfin_nonep_h
*gen Wtot_f = Wimmo_net_f + Wfin_epargne_f + Wfin_nonep_f


* Patrimoine financier (n'inclut pas les comptes courants)
gen Wfin_brut_h = Wfin_epargne_h + Wfin_nonep_h 
gen Wfin_brut_f = Wfin_epargne_f + Wfin_nonep_f

*** PATRIMOINE NET ***
* même si le patrimoine professionnel est individualisable, on reconstruit
* le patirmoine prof de la même façon que pour les enquêtes suivantes

* W_temp = variable de construction, = tout le patrimoine individualisable
*gen W_temp = Wimmo_net_h + Wfin_brut_h + Wpro_net_h + Wimmo_net_f + Wfin_brut_f + Wpro_net_f
* W_tot_net_men = tout le patrimoine individualisable + non individualisable
*gen Wtot_net_men = W_temp +  kdu_creance_autre - kdu_dette_autre
* Wtot_net_h, Wtot_net_f = patrimoine individuel. 
*** On attribue les creances et dettes non individualisables au pro-rata des autres biens 
*gen Wtot_net_h = Wimmo_net_h + Wfin_epargne_h + Wfin_nonep_h + Wpro_net_h + (kdu_creance_autre - kdu_dette_autre)*(Wimmo_net_h + Wfin_brut_h + Wpro_net_h)/W_temp
*gen Wtot_net_f = Wimmo_net_f + Wfin_epargne_f + Wfin_nonep_f + Wpro_net_f + (kdu_creance_autre - kdu_dette_autre)*(Wimmo_net_f + Wfin_brut_f + Wpro_net_f)/W_temp

* W_temp = variable de construction, = tout le patrimoine individualisable
gen W_temp = Wimmo_net_h + Wfin_brut_h + Wimmo_net_f + Wfin_brut_f
* W_temp2 = variable de construction = patrimoine non individualisable
gen W_temp2 = Wpro_net_h + Wpro_net_f + kdu_creance_autre - kdu_dette_autre
* W_tot_net_men = tout le patrimoine individualisable + non individualisable
gen Wtot_net_men = W_temp + W_temp2 
* Wtot_net_h, Wtot_net_f = patrimoine individuel. 
*** On attribue les creances et dettes non individualisables (et patrimoine pro)
*** au pro-rata des autres biens 
gen Wtot_net_h = Wimmo_net_h + Wfin_brut_h + W_temp2 * (Wimmo_net_h + Wfin_brut_h) / W_temp
gen Wtot_net_f = Wimmo_net_f + Wfin_brut_f + W_temp2 * (Wimmo_net_f + Wfin_brut_f) / W_temp
replace Wtot_net_h = W_temp2 / PR if W_temp == 0 
replace Wtot_net_f = W_temp2 / PR if W_temp == 0 

drop W_temp*

*** PATRIMOINE BRUT ***
* W_temp = variable de construction, = tout le patrimoine individualisable
gen W_temp = Wimmo_brut_h + Wfin_brut_h  + Wimmo_brut_f + Wfin_brut_f
* W_temp2 = variable de construction = patrimoine non individualisable
gen W_temp2 = Wpro_brut_h + Wpro_brut_f + kdu_creance_autre 
* W_tot_net_men = tout le patrimoine individualisable + non individualisable
gen Wtot_brut_men = W_temp + W_temp2 
* Wtot_net_h, Wtot_net_f = patrimoine individuel. 
*** On attribue les creances et dettes non individualisables au pro-rata des autres biens 
gen Wtot_brut_h = Wimmo_brut_h + Wfin_brut_h + W_temp2 * (Wimmo_brut_h + Wfin_brut_h) / W_temp
gen Wtot_brut_f = Wimmo_brut_f + Wfin_brut_f + W_temp2 * (Wimmo_brut_f + Wfin_brut_f) / W_temp
replace Wtot_brut_h = W_temp2/PR if W_temp == 0 
replace Wtot_brut_f = W_temp2/PR if W_temp == 0 

drop W_temp*


** variables enfants***

gen enfant_h_seul = .
gen enfant_f_seul = .
replace enfant_h_seul = 1 if enfant_PR_seul == 1 & sexe_pr == "1"
replace enfant_h_seul = 1 if enfant_CJ_seul == 1 & sexe_cj == "1"
replace enfant_f_seul = 1 if enfant_PR_seul == 1 & sexe_pr == "2"
replace enfant_f_seul = 1 if enfant_CJ_seul == 1 & sexe_cj == "2"
recode enfant*seul (. = 0)
drop enfant_PR_seul enfant_CJ_seul
ren sexe_pr sexe1 
ren sexe_cj sexe2
drop *_pr *_cj
ren sexe1 sexe_pr
ren sexe2 sexe_cj
so identind
gen wave = 2004
drop matri gparmat gparpat jemprof jepprof  

save $temp/assets_2004, replace


use $pat04/menage, clear 

forv k = 1/12{
	gen enf_hm_couple_`k' = (hodln`k' == "1")
	gen enf_hm_PR_`k'     = (hodln`k' == "2")
	gen enf_hm_CJ_`k'     = (hodln`k' == "3")
}

egen nbenf_hm_couple = rsum(enf_hm_couple_*)
egen nbenf_hm_PR = rsum(enf_hm_PR_*)
egen nbenf_hm_CJ = rsum(enf_hm_CJ_*)
gen nbenf_hm = nbenf_hm_couple + nbenf_hm_PR + nbenf_hm_CJ

keep ident pond zeat nbenf_hm_couple nbenf_hm_PR nbenf_hm_CJ nbenf_hm
so ident 
merge 1:m ident using $temp/assets_2004
drop if _m == 1
drop _m

gen nbenf_hm_hseul = .
gen nbenf_hm_fseul = .
replace nbenf_hm_hseul = nbenf_hm_PR if sexe_pr == "1"
replace nbenf_hm_hseul = nbenf_hm_CJ if sexe_cj == "1"
replace nbenf_hm_fseul = nbenf_hm_PR if sexe_pr == "2"
replace nbenf_hm_fseul = nbenf_hm_CJ if sexe_cj == "2"
drop nbenf_hm_PR nbenf_hm_CJ
so ident identind  

save $temp/assets_2004, replace


** construction du rang de l'enfant dans la fratrie 

* enfants hors ménage
use $pat04/menage, clear 
keep ident hod*  
reshape long hodsex hodln hodan hodip hodsect hodco hodenf hodemp hodpub ///
		hodcho hodniv hodpri hodind, i(ident) j(nb_hm) 
drop if hodsex == ""
so ident nb_hm
gen enfant_couple  = (hodln == "1")
gen enfant_PR_seul = (hodln == "2")
gen enfant_CJ_seul = (hodln == "3")
keep ident nb hodnb hodsex hodan enfant_* 
ren hodnb nb_enf_hm
ren hodsex sexe 
ren hodan anais
drop if sexe == "8"
tempfile enf_hm
save `enf_hm', replace


use $pat04\INDIVIDU, clear
*keep if age <= 17
*gen PR = (lienpref == "00" | lienpref == "01")
gen enfant = (lien == "3")
drop if enfant == 0
drop enfant

gen enfant_couple  = (lien == "3" & enf == "1")
gen enfant_PR_seul = (lien == "3" & enf == "2") 
gen enfant_CJ_seul = (lien == "3" & enf == "3") 
keep ident identind age anais mnais sexe enfant_*

so ident anais mnais
by ident: gen nb_m     = _n
by ident: gen nb_enf_m = _N

append using  `enf_hm'
so ident anais mnais

* identification des naissances multiples
destring sexe, gen(sexe_num)
tostring mnais, gen(str_mnais)
foreach v in enfant_couple enfant_PR_seul enfant_CJ_seul{
gen anais_`v' = anais*`v'
tostring anais_`v', gen(str_anais_`v')
gen mnais_anais_`v' = str_anais_`v' + str_mnais
destring mnais_anais_`v', gen(mnais_anais_`v'_num)
bys ident mnais_anais_`v'_num : egen multiple_`v' = count(sexe_num)
replace multiple_`v' = . if anais_`v' == . | anais_`v' == 0
}
drop anais_* mnais_*
recode multiple* (. = 0)
egen multiple = rsum(multiple*)
recode multiple (0 = .)

so ident anais mnais
by ident: gen rank_all_temp = _n
by ident: gen nb_enf_all = _N

* on construit une variable avec le sexe des 2 premiers enfants
preserve
keep ident  rank_all_temp sexe
reshape wide sexe, i(ident) j(rank_all_temp)
keep ident sexe1 sexe2 
tempfile sexcompo
so ident
save `sexcompo', replace
restore

so ident 
merge m:1 ident using  `sexcompo'
drop _m
ren sexe1 sexe_enf1 
ren sexe2 sexe_enf2 
* fin de la construction de la variable sexe des deux premiers

bys ident multiple anais : egen rank_all = min(rank_all_temp)
drop rank_all_temp

*by ident: gen nb_all = _n
*by ident: gen nb_enf_all = _N
so ident anais mnais
order ident nb_enf_m nb_enf_hm nb_enf_all  nb_m nb_hm rank_all nb_enf_all
gen enfant_PR = enfant_couple | enfant_PR_seul
gen enfant_CJ = enfant_couple | enfant_CJ_seul


gen fille  = sexe == "2" 
gen garcon = sexe == "1" 
replace age = 2004 - anais if age == .
gen enf_17m = (age <= 17)
gen enf_18p = (age >  17)

so ident anais mnais
* rang parmi tous les enfants
* gen rank_all = nb_all
* rang parmi les toutes les filles
so ident fille anais mnais
by ident fille : gen rank_girls_temp = _n
bys ident multiple anais : egen rank_girls = min(rank_girls_temp)
drop rank_girls_temp
replace rank_girls = . if fille == 0

* rang parmi les tous les garçons
so ident garcon anais mnais
by ident garcon : gen rank_boys_temp = _n
bys ident multiple anais : egen rank_boys = min(rank_boys_temp)
drop rank_boys_temp
replace rank_boys = . if garcon == 0


* rang parmi les enfants du couple
so ident enfant_couple anais mnais
by ident enfant_couple : gen rank_couple_temp = _n
bys ident multiple anais : egen rank_couple = min(rank_couple_temp)
replace rank_couple = . if enfant_couple == 0
drop rank_couple_temp

* rang parmi les enfants du PR
so ident enfant_PR anais mnais
by ident enfant_PR : gen rank_PR_temp = _n
bys ident multiple anais : egen rank_PR = min(rank_PR_temp)
replace rank_PR = . if enfant_PR == 0
drop rank_PR_temp

* rang parmi les enfants du CJ
so ident enfant_CJ anais mnais
by ident enfant_CJ : gen rank_CJ_temp = _n
bys ident multiple anais : egen rank_CJ = min(rank_CJ_temp)
replace rank_CJ = . if enfant_CJ == 0
drop rank_CJ_temp

so ident anais mnais
bys ident : egen nb_girls_all    = sum(fille)
bys ident : egen nb_boys_all     = sum(garcon)
bys ident : egen nb_girls_couple = sum(fille  * enfant_couple)
bys ident : egen nb_boys_couple  = sum(garcon * enfant_couple)
bys ident : egen nb_girls_PR     = sum(fille  * enfant_PR)
bys ident : egen nb_boys_PR      = sum(garcon * enfant_PR)
bys ident : egen nb_girls_CJ     = sum(fille  *enfant_CJ)
bys ident : egen nb_boys_CJ      = sum(garcon *enfant_CJ)
bys ident : egen nb_enf_17m      = sum(enf_17m)
bys ident : egen nb_enf_18p      = sum(enf_18p)

drop multiple_*

preserve
keep ident multiple rank_all 
duplicates drop 
drop if multiple == 1 | multiple == .
ren multiple mult_birth_hh
ren rank_all rank_mult_birth
so ident rank_mult_birth
by ident : gen dup = _n
reshape wide mult_birth_hh rank_mult_birth , i(ident) j(dup) 
tempfile test
so ident
save `test' , replace
restore 
merge m:1 ident using `test'
drop _m

drop if identind == ""
keep if age <= 17

keep ident identind multiple rank_* nb_girls_* nb_boys_* nb_enf_m ///
	 nb_enf_17m nb_enf_18p nb_enf_all rank_mult_birth1 mult_birth_hh1 ///
	 rank_mult_birth2 mult_birth_hh2 sexe_enf*

so ident identind

merge 1:1 ident identind using $temp/assets_2004
drop if _m == 1
drop _m

gen rank_enf_h = .
gen nb_girls_h = .
gen nb_boys_h  = .
gen rank_enf_f = .
gen nb_girls_f = .
gen nb_boys_f  = .
replace rank_enf_h = rank_PR 	 if sexe_pr == "1"
replace rank_enf_h = rank_CJ 	 if sexe_cj == "1"
replace rank_enf_f = rank_PR 	 if sexe_pr == "2"
replace rank_enf_f = rank_CJ 	 if sexe_cj == "2"
replace nb_boys_h  = nb_boys_PR  if sexe_pr == "1"
replace nb_boys_h  = nb_boys_CJ  if sexe_cj == "1"
replace nb_boys_f  = nb_boys_PR  if sexe_pr == "2"
replace nb_boys_f  = nb_boys_CJ  if sexe_cj == "2"
replace nb_girls_h = nb_girls_PR if sexe_pr == "1"
replace nb_girls_h = nb_girls_CJ if sexe_cj == "1"
replace nb_girls_f = nb_girls_PR if sexe_pr == "2"
replace nb_girls_f = nb_girls_CJ if sexe_cj == "2"

drop rank_PR nb_girls_PR nb_boys_PR rank_CJ nb_girls_CJ nb_boys_CJ

so ident identind
save $temp/assets_2004, replace



**** autres variables nécessaires issues de la base ménage 
* LIVJMO  Motif principal de détention de livret jeune

use $pat04\menage, clear
keep ident logoc formreg formcon formdif /*surface*/
ren logoc stoc
so ident
merge 1:m ident using $temp/assets_2004
drop if _m != 3
drop _m

so ident identind
save $temp/assets_2004, replace
