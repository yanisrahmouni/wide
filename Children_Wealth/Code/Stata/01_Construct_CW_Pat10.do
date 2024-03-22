********************************************************************************
***                                                                          ***
***                         2010 DATA SCRIPT                                 ***
***     This script is designed to prepare data from the 2010 survey.        ***
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



** on traite séparément les individus ENFANTS et les indiv PR/CJ
** on traite séparément les types d'assets

use $pat10\INDIVIDU, clear
gen PR           = (lienpref == "00" | lienpref == "01")
gen enfant_PR    = (lienpref == "02" | lienpref == "31")
gen autre_PR     = (PR == 0 & enfant_PR == 0 & age >= 25)
gen autre_enf_PR = (PR == 0 & enfant_PR == 0 & age < 25)
collapse (sum) PR enfant_PR autre_enf_PR autre_PR, by(identmen)
so identmen
save $temp\compo_men, replace 


***** 1/ ENFANTS 

** 1.0/ identification des enfants

use $pat10\INDIVIDU, clear
keep if age <= 17
gen PR        = (lienpref == "00" | lienpref == "01")
gen enfant_PR = (lienpref == "02" | lienpref == "31")
drop if enfant_PR == 0
gen enfant_couple  = (lienpref == "02" & enf == "1")
gen enfant_PR_seul = (lienpref == "02" & enf == "2") 
gen enfant_CJ_seul = (lienpref == "31" & enf == "3")  

keep identmen identind age sexe enfant_PR enfant_couple enfant_PR_seul ///
	 enfant_CJ_seul actif actoccup anarriv classif classifante couple ///
	 etamatri etudes forminit  hertabd_i hertabh_i rechemploi ///
	 pacs natio7 nais7 anais per1e mer1e statut cs42 cs24 cs_ante  dip14 ///
	 pond jemprof jepprof  z* gparpat gparmat hertabd_i hertabh_i ///
gestsep_i duree  

tempfile children
so identind
save $temp\children, replace

** 1.1/ assets financiers des enfants

use $pat10\PRODUIT, clear
ren identpos identind
so identind
merge m:1 identind using $temp\children
keep if _m==3|_m==2

destring finna, replace 
recode montcla (. = 0)
recode montcla (. = 0)

gen Wfin_cc      = (finna == 1)               * montcla
gen Wfin_epargne = (finna > 1 & finna <= 9)   * montcla
gen Wfin_nonep   = (finna >= 10 & finna < 36) * montcla

gen has_compteCh    = (finna == 1)
gen has_livA        = (finna == 2 | finna == 3 | finna == 4 | finna == 5)
gen has_livJeune    = (finna == 6)
gen has_livImpot    = (finna == 7)
gen has_EpLog       = (finna == 8 | finna == 9)
gen has_AssuVie     = (finna == 20)
gen has_other_asset = (finna > 9 & finna != 20 & finna != .)

collapse (sum) Wfin_cc Wfin_epargne Wfin_nonep has_*, by(identind)
recode has* (2/max = 1)
tempfile fin_enf
so identind
save `fin_enf'


** 1.2/ assets immobilier des enfants 

use $pat10\PRODUIT, clear
keep if nature == "2"
ren algacq acqdet
keep identmen identprod montcla paraut acqdet
ren montcla immo_valeur_brute
so identmen
merge m:1 identmen using $temp\compo_men
gen total_autre_PR = enfant_PR + autre_PR 
*gen Wimmo_brut_enf = immo_valeur_brute*(paraut/total_autre_PR)/100
*gen Wimmo_net_enf = immo_valeur_nette*(paraut/total_autre_PR)/100

* If other people in the hh who is not a child, we attribute all wealth to this person
* => minimize wealth of children
gen Wimmo_brut_enf = immo_valeur_brute * (paraut / total_autre_PR) / 100 if autre_PR == 0 
drop if enfant_PR == 0
recode Wimmo_brut_enf (. = 0)
gen exist_dette_immo = (acqdet == "1") 
collapse (sum) Wimmo_brut_enf immo_valeur_brute exist_dette_immo, by(identmen)
so identmen
tempfile immo
save `immo', replace

*** Cas 1: on arrive à retrouver le logement pour lequel l'emprunt a été contracté
* => pas possible en 2010, car la dette n'est pas associée à un logement

*** Cas 2: on n'arrive pas à retrouver le logement pour lequel l'emprunt a été contracté
*gen Wimmo_net_enf = immo_valeur_nette*(paraut/total_autre_PR)/100 if autre_PR == 0 

use $pat10\PRODUIT, clear
keep if nature == "6" & (detqua== "01" | detqua == "02")
keep identmen montcla kdu montcla_corr kdu_corr 
collapse (sum) montcla kdu kdu_corr, by(identmen)
ren montcla valeur_dette
so identmen
merge 1:1 identmen using `immo'
drop if _m == 1
drop _m
gen immo_valeur_nette = immo_valeur_brute - kdu_corr
gen Wimmo_net_enf = Wimmo_brut_enf - (Wimmo_brut_enf / immo_valeur_brute) * kdu_corr
recode exist_dette_immo (1/9 = 1)
recode immo_valeur_nette  Wimmo_net_enf (. = 0)

collapse (sum) Wimmo_brut_enf Wimmo_net_enf exist_dette_immo, by(identmen)
replace exist_dette_immo = 0 if Wimmo_brut_enf == 0
so ident
tempfile immo_enf
save `immo_enf'

** 1.3/ assets professionnels
*=> on fait l'hypothèse que les biens professionnels détenus par des enfants vivants chez leurs parents est négligeable
*=> on ne tient pas comtpe des dettes professionnels du coup


use $temp\children, clear
so identmen
merge m:1 identmen using `immo_enf'
drop if _m == 2 
drop _merge
so identind
merge 1:1 identind using `fin_enf'
drop _m
gen Wtot =  Wimmo_net_enf + Wfin_epargne + Wfin_nonep

so identmen identind
save $temp\assets_children, replace
 
*annee finna logna montcla nature pond profna



***** 2/ PR et CJ (parents des enfants)

** 2.0/ identification des PR et CJ

use $pat10\INDIVIDU, clear
*keep if age<=40
gen PR        = (lienpref == "00" | lienpref == "01")
gen enfant_PR = (lienpref == "02" | lienpref == "31")
drop if PR == 0
keep identmen identind age sexe PR lienpref noi actif actoccup anarriv classif ///
	 classifante couple etamatri etudes forminit  hertabd_i hertabh_i ///
	 rechemploi pacs natio7 nais7 anais per1e mer1e statut cs42 cs24 cs_ante ///
	 dip14 pond z* gparpat gparmat  jefrso jeargt jegrave jepprof jemact jemprof jepro_* ///
gestsep_i duree 
so identind 
save $temp\pr_cj, replace

use $temp\pr_cj, clear
keep identmen noi age lienpref sexe actif actoccup anarriv classif classifante ///
	 couple etamatri etudes forminit  hertabd_i hertabh_i rechemploi ///
	 pacs natio7 nais7 anais per1e mer1e statut cs42 cs24 cs_ante  dip14 pond ///
	 z* gparpat gparmat  jefrso jeargt jegrave jepprof jemact jemprof jepro_* ///
gestsep_i duree 
	 
destring lienpref, replace
reshape wide noi age sexe actif actoccup anarriv classif classifante couple ///
		etamatri etudes forminit  hertabd_i hertabh_i rechemploi pacs ///
		natio7 nais7 anais per1e mer1e statut cs42 cs24 cs_ante  dip14 pond z* ///
		gparpat gparmat jefrso jeargt jegrave jepprof jemact ///
		jemprof jepro_* ///
gestsep_i duree , i(ident) j(lienpref)

foreach v in noi age sexe actif actoccup anarriv classif classifante couple ///
			 etamatri etudes forminit  hertabd_i hertabh_i rechemploi ///
			 pacs natio7 nais7 anais per1e mer1e statut cs42 cs24 cs_ante  ///
			 dip14 pond z* gparpat gparmat jefrso jeargt jegrave jepprof ///
			 jemact jemprof jepro_* ///
gestsep_i duree {
ren `v'0 `v'_pr
ren `v'1 `v'_cj
}
so ident 
save $temp\pr_cj_men, replace


** 2.1/ assets financiers PR et CJ 

**** 2.1.1/ assets financiers PR et CJ détenu en biens propres

use $pat10\PRODUIT, clear

ren identpos identind
so identind
merge m:1 identind using $temp\pr_cj
keep if _m==3 | _m==2

destring finna, replace 
recode montcla (. = 0)
gen Wfin_cc      = (finna == 1)               * montcla
gen Wfin_epargne = (finna > 1 & finna <= 9)   * montcla
gen Wfin_nonep   = (finna >= 10 & finna < 36) * montcla

collapse (sum) Wfin_cc Wfin_epargne Wfin_nonep, by(identmen lienpref)
destring lienpref, replace

reshape wide Wfin_cc Wfin_epargne Wfin_nonep, i(identmen) j(lienpref)
ren Wfin_cc0 Wfin_cc_pr
ren Wfin_epargne0 Wfin_epargne_pr  
ren Wfin_nonep0 Wfin_nonep_pr
ren Wfin_cc1 Wfin_cc_cj
ren Wfin_epargne1 Wfin_epargne_cj  
ren Wfin_nonep1 Wfin_nonep_cj
tempfile fin_pr_cj
so identmen
save `fin_pr_cj'

**** 2.1.2/ assets financiers PR et CJ détenus conjointement

use $pat10\PRODUIT, clear

keep if nature == "1" & nop == "00"
keep identmen nop1 nop2 finna montcla
so identmen 
merge m:1 ident using $temp\pr_cj_men
drop if _m != 3
drop _m

* cas 1 : détenu par PR et CJ conjointement
gen W_fin_conj_pr = montcla / 2 if ((nop1 == noi_pr & nop2 == noi_cj) | (nop1 == noi_cj & nop2 == noi_pr))
gen W_fin_conj_cj = montcla / 2 if ((nop1 == noi_pr & nop2 == noi_cj) | (nop1 == noi_cj & nop2 == noi_pr))

* cas 2 : détenu par PR ou CJ + extérieur
replace W_fin_conj_pr = montcla / 2 if ((nop1 == noi_pr & nop2 != noi_cj) | (nop1 != noi_cj & nop2 == noi_pr))
replace W_fin_conj_cj = montcla / 2 if ((nop1 != noi_pr & nop2 == noi_cj) | (nop1 == noi_cj & nop2 != noi_pr))
drop if ((nop1 != noi_pr & nop2 != noi_cj) & (nop1 != noi_cj & nop2 != noi_pr))

destring finna, replace
gen Wfin_cc_pr      = (finna == 1)               * W_fin_conj_pr
gen Wfin_epargne_pr = (finna > 1 & finna <= 9)   * W_fin_conj_pr
gen Wfin_nonep_pr   = (finna >= 10 & finna < 36) * W_fin_conj_pr
gen Wfin_cc_cj      = (finna == 1)               * W_fin_conj_cj
gen Wfin_epargne_cj = (finna > 1 & finna <= 9)   * W_fin_conj_cj
gen Wfin_nonep_cj   = (finna >= 10 & finna < 36) * W_fin_conj_cj

collapse (sum) Wfin_*, by(identmen)
append using `fin_pr_cj'

collapse (sum) Wfin_*, by(identmen)
save `fin_pr_cj', replace


** 2.2/ assets immobilier des PR et CJ

use $pat10\PRODUIT, clear
keep if nature == "2"
ren algacq acqdet
keep identmen identprod montcla parpr parcj parmen paraut acqdet
ren montcla immo_valeur_brute
so identmen
merge m:1 identmen using $temp\compo_men

gen Wimmo_brut_pr = immo_valeur_brute * (parpr) / 100
gen Wimmo_brut_cj = immo_valeur_brute * (parcj) / 100

drop if PR == 0
recode Wimmo_brut_pr Wimmo_brut_cj (. = 0)
gen exist_dette_immo = (acqdet == "1") 
collapse (sum) Wimmo_brut_pr Wimmo_brut_cj immo_valeur_brute exist_dette_immo, by(identmen)
so identmen
tempfile immo
save `immo', replace

*** Cas 1: on arrive à retrouver le logement pour lequel l'emprunt a été contracté
* => pas possible en 2010, car la dette n'est pas associée à un logement

*** Cas 2: on arrive à retrouver le logement pour lequel l'emprunt a été contracté
*gen Wimmo_net_enf = immo_valeur_nette*(paraut/total_autre_PR)/100 if autre_PR == 0 /* if other people in the hh who is not a child, we attribute all wealth to this person => minimize wealth of children */

use $pat10\PRODUIT, clear
keep if nature == "6" & (detqua == "01" | detqua == "02")
keep identmen montcla kdu montcla_corr kdu_corr 
collapse (sum) montcla kdu kdu_corr, by(identmen)
ren montcla valeur_dette
so identmen
merge 1:1 identmen using `immo'
drop if _m == 1
drop _m
gen immo_valeur_nette = immo_valeur_brute - kdu_corr
gen Wimmo_net_pr = Wimmo_brut_pr - (Wimmo_brut_pr / immo_valeur_brute) * kdu_corr
gen Wimmo_net_cj = Wimmo_brut_cj - (Wimmo_brut_cj / immo_valeur_brute) * kdu_corr
recode exist_dette_immo (1/9 = 1)
recode immo_valeur_nette  Wimmo_net_* (. = 0)

collapse (sum) Wimmo_brut_* Wimmo_net_* exist_dette_immo, by(identmen)
so ident
tempfile immo_pr_cj
save `immo_pr_cj'



*** 2.3 Business assets

* la base PRODUIT donne les actifs professionnels, détenus directement ou via des entreprises.
* la base ENTREPRISE donne la valeur de l'entreprise, qui peut être égale à la valeurs des actifs détenus par l'entreprise
* il y a donc redondance de certains actifs entre les 2 bases (actifs détenus par l'entreprise)
* mais certains actifs sont dans PRODUIT seulement (actif pro non entreprise)
* et la valeur de l'entreprise peut être plus grande / petite que ses actifs (à vérifier ??)
* Remarque : pas possible d'identifier les parts qui appartiennent à PR et à CJ de façon distincte
* Même chose pour les dettes, qui sont codées à 2 endroits si ce sont les dettes de l'entreprise détenue par un ménage

* 2.3.1 Actifs

use $pat10\PRODUIT, clear
keep if nature == "3" |nature == "4"
gen bien_pro_men = (nop == "ME")
gen bien_pro_ent = (nop == "E1" | nop == "E2" | nop == "E3" | nop == "E4" | nop == "E5" | nop == "E6")
keep identprod identmen identpos bien_pro_men  bien_pro_ent profna montcla montcla_corr ///
	 montmin_corr montmin_decl montmax_corr montmax_decl  nature paraut parhm parpr ///
	 parcj parmen profna pond

recode parmen (999 = .)
gen mont_bien_pro_men = bien_pro_men * montcla * (parmen / 100)
gen mont_bien_pro_ent = bien_pro_ent * montcla * (parmen / 100)
collapse (sum) mont_bien_pro_men mont_bien_pro_ent, by(identmen)
so identmen
tempfile bien_pro
save `bien_pro'

use $pat10\ENTREPRISE, clear
keep identmen identent mtentrep totmin_decl totmax_decl pca kduent kduent_decl ///
	 detmen kdumen detent ca nat_ca jur r_proent r_terent
gen mtentrep_men = (pca / 100) * mtentrep
collapse (sum) mtentrep_men, by(identmen)
ren mtentrep mont_valeur_ent
so identmen
merge 1:1 identmen using `bien_pro'
*gen valeur_ent_hors_biens = mont_valeur_ent - mont_bien_pro_ent
*su valeur_ent_hors_biens, det
drop _m
so identmen 
save `bien_pro',replace

* 2.3.1 Passifs

use $pat10\PRODUIT, clear
keep if nature == "6" & natemp == "05"
gen dette_pro_men = (nop == "ME")
gen dette_pro_ent = (nop == "E1" | nop == "E2" | nop == "E3" | nop == "E4" | nop == "E5" | nop == "E6")
keep identprod identmen identpos dette_pro_men dette_pro_ent kdu kdu_corr ///
	 profna montcla montcla_corr montmin_corr montmin_decl montmax_corr ///
	 montmax_decl  nature paraut parhm parpr parcj parmen profna pond

recode kdu (999999998 999999999 = .)
gen mont_dette_pro_men = dette_pro_men * kdu
gen mont_dette_pro_ent = dette_pro_ent * kdu
collapse (sum) mont_dette_pro_men mont_dette_pro_ent, by(identmen)
so identmen
tempfile dette_pro
save `dette_pro'

use $pat10\ENTREPRISE, clear
keep identmen identent mtentrep totmin_decl totmax_decl pca kduent ///
	 kduent_decl detmen kdumen detent ca nat_ca jur r_proent r_terent

*gen mont_dette_pro_men_2 = (pca / 100) * kdumen
gen mont_dette_pro_ent_2 = (pca / 100) * kduent
collapse (sum) mont_dette_pro_ent_2, by(identmen)
so identmen
merge 1:1 identmen using `dette_pro'
drop _m
so identmen
merge 1:1 identmen using `bien_pro'

* mise en commun 
recode mont* (. = 0)

* patrimoine prof détenu par les ménages directement
gen Wpro_brut_men = mont_bien_pro_men
gen Wpro_net_men  = mont_bien_pro_men - mont_dette_pro_men

* patrimoine prof détenu via des entreprises
*** on garde la valeur de l'entreprise si celle-ci est renseignée dans la table ENTREPRISE (moins les dettes)
*** sinon on garde les valeurs renseignées dans la table PRODUIT
gen Wpro_brut_ent = mont_valeur_ent                        if mont_valeur_ent != 0
gen Wpro_net_ent  = mont_valeur_ent - mont_dette_pro_ent_2 if mont_valeur_ent != 0

replace Wpro_brut_ent = mont_bien_pro_ent                      if mont_valeur_ent == 0 
replace Wpro_net_ent  = mont_bien_pro_ent - mont_dette_pro_ent if mont_valeur_ent == 0

* la patrimoine pro est défini comme la somme des deux patrimoines (détenu de façon directe ou via les entreprises)
gen Wpro_brut = Wpro_brut_men + Wpro_brut_ent
gen Wpro_net  = Wpro_net_men  + Wpro_net_ent

keep identmen Wpro_brut Wpro_net
so identmen 
save `bien_pro',replace


*** 2.4 Autres dettes 

use $pat10\PRODUIT, clear
keep if nature == "6" & (detqua == "04" | detqua=="05" | detqua=="06")
keep identmen montcla kdu 
recode montcla kdu (999999998 999999999 = .)
collapse (sum) montcla kdu, by(identmen)
ren montcla valeur_dette_autre
ren kdu kdu_dette_autre
so identmen
tempfile dette_pr_cj
save `dette_pr_cj'

*** 2.5 Autres actifs (prêts)

use $pat10\PRODUIT, clear
keep if nature == "5"
keep identmen montcla kdu 
recode montcla kdu (999999998 999999999 = .)
collapse (sum) montcla kdu, by(identmen)
ren montcla valeur_creance_autre
ren kdu kdu_creance_autre
so identmen
tempfile creance_pr_cj
save `creance_pr_cj'


** 2.6 Merge assets of PR and CJ

use $temp\pr_cj_men, clear
so identmen
merge 1:1 identmen using `immo_pr_cj'
drop _m
merge 1:1 identmen using `fin_pr_cj'
drop _m
merge 1:1 identmen using `bien_pro'
drop _m
merge 1:1 identmen using `dette_pr_cj'
drop _m
merge 1:1 identmen using `creance_pr_cj'
drop _m

so identmen
save $temp\assets_pr_cj,replace


*** 3/ Merge assets of children with assets of parents

use $temp\compo_men, clear
so identmen
merge 1:1 identmen using $temp\assets_pr_cj
drop _m
so identmen
merge 1:m ident using $temp\assets_children
drop if _m == 1
* on vire les couples homo (0 HH et 6 FF)
drop if sexe_pr == sexe_cj
drop _m

foreach v in noi age Wimmo_brut Wimmo_net Wfin_cc Wfin_epargne Wfin_nonep ///
		actif actoccup anarriv classif classifante couple etamatri etudes ///
		forminit  hertabd_i hertabh_i rechemploi pacs natio7 nais7 ///
		anais per1e mer1e statut cs42 cs24 cs_ante  dip14 pond zsalaires_i ///
		zchomage_i zretraites_i zpenalir_i zrentes_i zrag_i zric_i zrnc_i ///
		gparpat gparmat jefrso jeargt jegrave jepprof jemact jemprof jepro_rp ///
		jepro_immo jepro_ter jepro_vmob jepro_avi jepro_trav jepro_agri ///
gestsep_i duree {
		
destring `v'*, replace
gen `v'_h =.
gen `v'_f =.
replace `v'_h = `v'_pr if sexe_pr == "1" 
replace `v'_h = `v'_cj if sexe_cj == "1" 
replace `v'_f = `v'_pr if sexe_pr == "2"
replace `v'_f = `v'_cj if sexe_cj == "2" 
}
*gen Wtot_h = Wimmo_net_h + Wfin_epargne_h + Wfin_nonep_h
*gen Wtot_f = Wimmo_net_f + Wfin_epargne_f + Wfin_nonep_f

* Patrimoine financier (n'inclut pas les comptes courants)
gen Wfin_brut_h = Wfin_epargne_h + Wfin_nonep_h 
gen Wfin_brut_f = Wfin_epargne_f + Wfin_nonep_f

recode Wimmo* Wfin* Wpro* kdu* (. = 0)

*****  PATRIMOINE NET ******
* W_temp = variable de construction => tout le patrimoine individualisable
gen W_temp = Wimmo_net_h + Wfin_brut_h + Wimmo_net_f + Wfin_brut_f
* W_temp2 = variable de construction = patrimoine non individualisable
gen W_temp2 = Wpro_net + kdu_creance_autre - kdu_dette_autre
* W_tot_net_men = tout le patrimoine individualisable + non individualisable
gen Wtot_net_men = W_temp + W_temp2 
* Wtot_net_h, Wtot_net_f = patrimoine individuel. 
*** On attribue les creances et dettes non individualisables au pro-rata des autres biens 
gen Wtot_net_h = Wimmo_net_h + Wfin_brut_h + W_temp2*(Wimmo_net_h + Wfin_brut_h)/W_temp
gen Wtot_net_f = Wimmo_net_f + Wfin_brut_f + W_temp2*(Wimmo_net_f + Wfin_brut_f)/W_temp
replace Wtot_net_h = W_temp2/PR if W_temp == 0 
replace Wtot_net_f = W_temp2/PR if W_temp == 0

drop W_temp W_temp2

*** PATRIMOINE BRUT ***
* W_temp = variable de construction, = tout le patrimoine individualisable
gen W_temp = Wimmo_brut_h + Wfin_brut_h  + Wimmo_brut_f + Wfin_brut_f
* W_temp2 = variable de construction = patrimoine non individualisable
gen W_temp2 = Wpro_brut + kdu_creance_autre 
* W_tot_net_men = tout le patrimoine individualisable + non individualisable
gen Wtot_brut_men = W_temp + W_temp2 
* Wtot_net_h, Wtot_net_f = patrimoine individuel. 
*** On attribue les creances et dettes non individualisables au pro-rata des autres biens 
gen Wtot_brut_h = Wimmo_brut_h + Wfin_brut_h + W_temp2 * (Wimmo_brut_h + Wfin_brut_h) / W_temp
gen Wtot_brut_f = Wimmo_brut_f + Wfin_brut_f + W_temp2 * (Wimmo_brut_f + Wfin_brut_f) / W_temp
replace Wtot_brut_h = W_temp2 / PR if W_temp == 0 
replace Wtot_brut_f = W_temp2 / PR if W_temp == 0 

drop W_temp W_temp2



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
gen wave = 2010
drop etamatri gparmat gparpat cs42 jemprof jepprof pacs 

save $temp/assets_2010, replace


use $pat10/menage, clear 
forv k = 1/12{
gen enf_hm_couple_`k' = (hodln`k' == "1")
gen enf_hm_PR_`k'     = (hodln`k' == "2")
gen enf_hm_CJ_`k'     = (hodln`k' == "3")
}
egen nbenf_hm_couple = rsum(enf_hm_couple_*)
egen nbenf_hm_PR     = rsum(enf_hm_PR_*)
egen nbenf_hm_CJ     = rsum(enf_hm_CJ_*)
gen nbenf_hm         = nbenf_hm_couple + nbenf_hm_PR + nbenf_hm_CJ


keep ident pond zeat nbenf_hm_couple nbenf_hm_PR nbenf_hm_CJ nbenf_hm

so ident 
merge 1:m ident using $temp/assets_2010
drop if _m == 1
drop _m
gen nbenf_hm_hseul = .
gen nbenf_hm_fseul = .
replace nbenf_hm_hseul = nbenf_hm_PR if sexe_pr == "1"
replace nbenf_hm_hseul = nbenf_hm_CJ if sexe_cj == "1"
replace nbenf_hm_fseul = nbenf_hm_PR if sexe_pr == "2"
replace nbenf_hm_fseul = nbenf_hm_CJ if sexe_cj == "2"
drop nbenf_hm_PR nbenf_hm_CJ
so identmen

save $temp/assets_2010, replace


***** RANKS 

** construction du rang de l'enfant dans la fratrie 

* enfants hors ménage
use $pat10/menage, clear 
keep ident hod* nenfants 
reshape long hodsex hodln hodan hodip hodsect hodco hodenf hodemp hodpub ///
		hodcho hodniv hodpri hodind, i(ident) j(nb_hm) 
drop if hodsex == ""
so ident nb_hm
gen enfant_couple  = (hodln == "1")
gen enfant_PR_seul = (hodln == "2")
gen enfant_CJ_seul = (hodln == "3")
keep ident nb hodnb hodsex hodan enfant_* nenfants
ren hodnb nb_enf_hm
ren hodsex sexe 
ren hodan anais
tempfile enf_hm
save `enf_hm', replace


use $pat10\INDIVIDU, clear
*keep if age<=17
*gen PR = (lienpref == "00"|lienpref == "01")
drop enfant
gen enfant = (lienpref == "02" | lienpref == "31")
drop if enfant == 0
drop enfant
gen enfant_couple  = (lienpref == "02" & enf == "1")
gen enfant_PR_seul = (lienpref == "02" & enf == "2") 
gen enfant_CJ_seul = (lienpref == "31" & enf == "3") 
keep identmen identind age anais sexe enfant_* mnais

so identmen anais mnais
by identmen: gen nb_m     = _n
by identmen: gen nb_enf_m = _N

append using `enf_hm'
so identmen anais mnais


* identification des naissances multiples
destring sexe, gen(sexe_num)
recode mnais (. = 0)
tostring mnais, gen(str_mnais)
foreach v in enfant_couple enfant_PR_seul enfant_CJ_seul{
gen anais_`v' = anais*`v'
tostring anais_`v', gen(str_anais_`v')
gen mnais_anais_`v' = str_anais_`v' + str_mnais
destring mnais_anais_`v', gen(mnais_anais_`v'_num)
bys identmen mnais_anais_`v'_num : egen multiple_`v' = count(sexe_num)
replace multiple_`v' = . if anais_`v' == . | anais_`v' ==0
}
drop anais_* mnais_*
recode multiple* (. = 0)
egen multiple = rsum(multiple*)
recode multiple (0 = .)


so identmen anais mnais
by identmen: gen rank_all_temp = _n
by identmen: gen nb_enf_all = _N

* on construit une variable avec le sexe dex 2 premiers enfants
preserve
keep identmen rank_all_temp sexe
reshape wide sexe, i(identmen) j(rank_all_temp)
keep identmen sexe1 sexe2 
tempfile sexcompo
so identmen
save `sexcompo', replace
restore

so identmen 
merge m:1 identmen using `sexcompo'
drop _m
ren sexe1 sexe_enf1 
ren sexe2 sexe_enf2 
* fin de la construction de la variable sexe des deux premiers


bys identmen multiple anais mnais : egen rank_all = min(rank_all_temp)
drop rank_all_temp

so identmen anais mnais
drop nenfants
order identmen nb_enf_m nb_enf_hm nb_enf_all rank_all
gen enfant_PR = enfant_couple | enfant_PR_seul
gen enfant_CJ = enfant_couple | enfant_CJ_seul

gen fille   = sexe == "2" 
gen garcon  = sexe == "1"

gen enf_17m = (age <= 17)
gen enf_18p = (age > 17)

* rang parmi tous les enfants
*gen rank_all = nb_all

* rang parmi les toutes les filles
so identmen fille anais mnais
by identmen fille : gen rank_girls_temp = _n
bys identmen multiple anais mnais : egen rank_girls = min(rank_girls_temp)
drop rank_girls_temp
replace rank_girls = . if fille == 0


* rang parmi les tous les garçons
so identmen garcon anais mnais
by identmen garcon : gen rank_boys_temp = _n
bys identmen multiple anais mnais : egen rank_boys = min(rank_boys_temp)
drop rank_boys_temp
replace rank_boys = . if garcon == 0

* rang parmi les enfants du couple
so identmen enfant_couple anais mnais
by identmen enfant_couple : gen rank_couple_temp = _n
bys identmen multiple anais mnais : egen rank_couple = min(rank_couple_temp)
replace rank_couple = . if enfant_couple == 0
drop rank_couple_temp

* rang parmi les enfants du PR
so identmen enfant_PR anais mnais
by identmen enfant_PR : gen rank_PR_temp = _n
bys identmen multiple anais mnais : egen rank_PR = min(rank_PR_temp)
replace rank_PR = . if enfant_PR == 0
drop rank_PR_temp

* rang parmi les enfants du CJ
so identmen enfant_CJ anais mnais
by identmen enfant_CJ : gen rank_CJ_temp = _n
bys identmen multiple anais mnais : egen rank_CJ = min(rank_CJ_temp)
replace rank_CJ = . if enfant_CJ == 0
drop rank_CJ_temp

so identmen anais mnais
bys identmen : egen nb_girls_all    = sum(fille)
bys identmen : egen nb_boys_all     = sum(garcon)
bys identmen : egen nb_girls_couple = sum(fille * enfant_couple)
bys identmen : egen nb_boys_couple  = sum(garcon * enfant_couple)
bys identmen : egen nb_girls_PR     = sum(fille * enfant_PR)
bys identmen : egen nb_boys_PR      = sum(garcon * enfant_PR)
bys identmen : egen nb_girls_CJ     = sum(fille * enfant_CJ)
bys identmen : egen nb_boys_CJ      = sum(garcon * enfant_CJ)
bys identmen : egen nb_enf_17m      = sum(enf_17m)
bys identmen : egen nb_enf_18p      = sum(enf_18p)

drop multiple_*

preserve
keep identmen multiple rank_all 
duplicates drop 
drop if multiple == 1 | multiple == .
ren multiple mult_birth_hh
ren rank_all rank_mult_birth
so identmen rank_mult_birth
by identmen : gen dup = _n
reshape wide mult_birth_hh rank_mult_birth, i(identmen) j(dup) 
tempfile test
so ident
save `test', replace
restore 
merge m:1 identmen using `test'
drop _m

drop if identind == ""
keep if age <= 17

* rank_mult_birth1 traduit le rang de la première naissance gémellaire (certains ménages peuvent avoir plusieurs naissances gémellaires)
keep identmen identind multiple rank_* nb_girls_* nb_boys_* nb_enf_m ///
	 nb_enf_17m nb_enf_18p nb_enf_all rank_mult_birth1 mult_birth_hh1 ///
	 rank_mult_birth2 mult_birth_hh2 sexe_enf*
so identmen identind

merge 1:1 identmen identind using $temp/assets_2010
drop if _m == 1
drop _m

gen rank_enf_h = .
gen nb_girls_h = .
gen nb_boys_h  = .
gen rank_enf_f = .
gen nb_girls_f = .
gen nb_boys_f  = .
replace rank_enf_h = rank_PR     if sexe_pr == "1"
replace rank_enf_h = rank_CJ     if sexe_cj == "1"
replace rank_enf_f = rank_PR     if sexe_pr == "2"
replace rank_enf_f = rank_CJ     if sexe_cj == "2"
replace nb_boys_h  = nb_boys_PR  if sexe_pr == "1"
replace nb_boys_h  = nb_boys_CJ  if sexe_cj == "1"
replace nb_boys_f  = nb_boys_PR  if sexe_pr == "2"
replace nb_boys_f  = nb_boys_CJ  if sexe_cj == "2"
replace nb_girls_h = nb_girls_PR if sexe_pr == "1"
replace nb_girls_h = nb_girls_CJ if sexe_cj == "1"
replace nb_girls_f = nb_girls_PR if sexe_pr == "2"
replace nb_girls_f = nb_girls_CJ if sexe_cj == "2"

drop rank_PR nb_girls_PR nb_boys_PR rank_CJ nb_girls_CJ nb_boys_CJ 

so identmen identind
save $temp/assets_2010, replace




**** autres variables nécessaires issues de la base ménage 

use $pat10\menage, clear
keep ident stoc formreg formdif surface
so ident
merge 1:m ident using $temp/assets_2010
drop if _m != 3
drop _m

so identmen identind
save $temp/assets_2010, replace

