*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*                                                                              *
*                           CHILDREN'S WEALTH                                  *
*                                                                              *
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*                                                                              *
* 02_Construct_CW_MergeAllWaves: merges temporary outputs and                  *
* generates the final data.                                                    *
*                                                                              *
*------------------------------------------------------------------------------*
* Inputs: assets_2004.dta, assets_2010.dta , assets_2014.dta, assets_2018.dta, *
* assets_2020.dta                                                              *
* Outputs: assets_allwaves.dta                                                     *
*------------------------------------------------------------------------------*
* Creation date: 2020                                                          *
* Update: 03/2024                                                              *
*------------------------------------------------------------------------------*
* Author(s): Marion Leturcq / Mathis Sansu                                     *
*------------------------------------------------------------------------------*

set more off

* construire une variable salarié privé/ salarié / indépendant / sans emploi

use $temp/assets_2004, clear
append using $temp/assets_2010
append using $temp/assets_2014
append using $temp/assets_2017
append using $temp/assets_2020

destring stoc, replace
gen stoc2 = stoc if wave != 2004
replace stoc2 = 1 if stoc ==3 & wave == 2004
replace stoc2 = 2 if stoc ==4 & wave == 2004
replace stoc2 = 3 if stoc ==5 & wave == 2004
replace stoc2 = 4 if stoc ==2 & wave == 2004
replace stoc2 = 5 if stoc ==6 & wave == 2004
replace stoc2 = . if stoc ==1 & wave == 2004
drop stoc
ren stoc2 stoc
label var stoc "Statut d'occupation du logement"
label define stoc 1 "Accédant propriété" 2 "Propriétaire non accédant" 3 "Usufruitier" 4 "Locataire" 5 "Logé gratuitement"
label values stoc stoc

replace actif = 1 if (occupa == 1|occupa == 2) & wave == 2004
replace actif = 2 if (occupa == 3|occupa == 8|occupa == .) & wave == 2004
replace actif = . if age < 15

foreach p in f h{
replace actif_`p' = 1 if (occupa_`p' == 1|occupa_`p' == 2) & wave == 2004
replace actif_`p' = 2 if (occupa_`p' == 3|occupa_`p' == 5|occupa_`p' == 7|occupa_`p' == 8|occupa_`p' == .) & wave == 2004
replace actif_`p' = . if noi_`p' == .
}

replace actoccup = 1 if (occupa == 1) & wave == 2004
replace actoccup = 2 if (occupa == 2|occupa == 3|occupa == 8|occupa == .) & wave == 2004
replace actoccup = . if age < 15

foreach p in f h{
replace actoccup_`p' = 1 if (occupa_`p' == 1) & wave == 2004
replace actoccup_`p' = 2 if (occupa_`p' == 2|occupa_`p' == 3|occupa_`p' == 5|occupa_`p' == 7|occupa_`p' == 8|occupa_`p' == .) & wave == 2004
replace actoccup_`p' = . if noi_`p' == .
}

foreach p in f h{
gen mere_active_`p' = inlist(jemact_`p',1,2,3)
replace mere_active_`p' = . if jemact_`p' == .
} 
recode jepprof* jemprof* (99 = .)
replace jemprof_h = 9 if jemprof_h == . & mere_active_h == 0
replace jemprof_f = 9 if jemprof_f == . & mere_active_f == 0
recode jefrso* (99 = .)(98 = .)
recode jeargt* jegrave* (9=.)
foreach p in f h{
replace jepro_rp_`p' = jepro_a_`p' if wave == 2004
replace jepro_immo_`p' = jepro_b_`p' if wave == 2004
replace jepro_ter_`p' = jepro_c_`p' if wave == 2004
replace jepro_vmob_`p' = jepro_d_`p' if wave == 2004
replace jepro_avi_`p' = jepro_e_`p' if wave == 2004
replace jepro_trav_`p' = jepro_f_`p' if wave == 2004
replace jepro_agri_`p' = jepro_g_`p' if wave == 2004
}
drop jepro_a_* jepro_b_* jepro_c_* jepro_d_* jepro_e_* jepro_f_* jepro_g_*
recode jepro* (9=.)

replace anarriv_h = cyfran_h if wave == 2004
replace anarriv_f = cyfran_f if wave == 2004
recode anarriv_* (9999=.)
drop cyfran*

foreach p in f h{
replace couple_`p' = 2 if couple_`p' == 3
replace couple_`p' = viecou_`p'  if wave == 2004
}
drop viecou_*

foreach p in f h{
replace etamatri_`p' = matri_`p' if wave == 2004
}
drop matri_*

*drop gestsep_i*

replace hertabh_i = herdon1_i if wave == 2004
replace hertabd_i = herdon2_i if wave == 2004

foreach p in f h{
replace hertabh_i_`p' = herdon1_i_`p' if wave == 2004
replace hertabd_i_`p' = herdon2_i_`p' if wave == 2004
}
drop herdon*

replace natio7 = 1 if na1 == 1 & wave == 2004
replace natio7 = 2 if na1 == 2 & wave == 2004
replace natio7 = 3 if na1 == 3 & tio == 2 & wave == 2004
replace natio7 = 4 if na1 == 3 & tio == 3 & wave == 2004
replace natio7 = 5 if na1 == 3 & tio == 4 & wave == 2004
replace natio7 = 6 if na1 == 3 & tio == 5 & wave == 2004
replace natio7 = 7 if na1 == 3 & tio == 6 & wave == 2004

foreach p in f h{
replace natio7_`p' = 1 if na1_`p' == 1 & wave == 2004
replace natio7_`p' = 2 if na1_`p' == 2 & wave == 2004
replace natio7_`p' = 3 if na1_`p' == 3 & tio_`p' == 2 & wave == 2004
replace natio7_`p' = 4 if na1_`p' == 3 & tio_`p' == 3 & wave == 2004
replace natio7_`p' = 5 if na1_`p' == 3 & tio_`p' == 4 & wave == 2004
replace natio7_`p' = 6 if na1_`p' == 3 & tio_`p' == 5 & wave == 2004
replace natio7_`p' = 7 if na1_`p' == 3 & tio_`p' == 6 & wave == 2004
}
drop na1* tio*

replace paysnai = paysnai+1 if paysnai>=2 & wave == 2004
replace nais7 = paysnai if wave == 2004

foreach p in f h{
replace paysnai_`p' = paysnai_`p'+1 if paysnai_`p'>=2 & wave == 2004
replace nais7_`p' = paysnai_`p' if wave == 2004
}
drop paysnai*


gen etranger_h = (natio7_h != 1 & natio7_h !=2)
replace etranger_h = . if natio7_h == .
gen etranger_f = (natio7_f != 1 & natio7_f !=2)
replace etranger_f = . if natio7_f == .

foreach a in p m{ 
gen `a'ere_vie = 1 if (`a'er1e == 1 | `a'er1e == 2)&wave>2004
replace `a'ere_vie = 1 if `a'ere == 1 & wave == 2004
replace `a'ere_vie = 2 if `a'ere == 2 & wave == 2004
replace `a'ere_vie = 2 if `a'er1e == 3 & wave > 2004
replace `a'ere_vie = 3 if `a'ere == 3 & wave == 2004
replace `a'ere_vie = 3 if `a'er1e == 4 & wave > 2004
}

foreach s in h f{
	foreach a in p m{ 
		gen `a'ere_vie_`s' = 1 if (`a'er1e_`s' == 1 | `a'er1e_`s' == 2)&wave>2004
		replace `a'ere_vie_`s' = 1 if `a'ere_`s' == 1 & wave == 2004
		replace `a'ere_vie_`s' = 2 if `a'ere_`s' == 2 & wave == 2004
		replace `a'ere_vie_`s' = 2 if `a'er1e_`s' == 3 & wave > 2004
		replace `a'ere_vie_`s' = 3 if `a'ere_`s' == 3 & wave == 2004
		replace `a'ere_vie_`s' = 3 if `a'er1e_`s' == 4 & wave > 2004
	}
}

recode cs24* (0=.)

tostring cs cs24, replace
gen cs8 = ""
replace cs8 = substr(cs,1,1) if wave == 2004
replace cs8 = substr(cs24,1,1) if wave != 2004
replace cs8 = "8" if cs8=="." & age >= 15	

foreach p in h f{
tostring cs_`p' cs24_`p', replace
gen cs8_`p' = ""
replace cs8_`p' = substr(cs_`p',1,1) if wave == 2004
replace cs8_`p' = "8" if wave == 2004 & actif_`p' == 2
replace cs8_`p' = substr(cs24_`p',1,1) if wave != 2004
replace cs8_`p' = "8" if cs8_`p'=="." & age_`p' >= 15	
replace cs8_`p' = "8" if cs8_`p' == "7" 
}
destring cs8*, replace

destring statut* , replace
gen statutb = .
replace statutb = 1 if statut == 1 & wave == 2004
replace statutb = 1 if statut == 1 & wave != 2004
replace statutb = 2 if (statut == 2|statut==3) & wave == 2004
replace statutb = 2 if statut == 2 & wave != 2004
replace statutb = 3 if statut == 4 & wave == 2004
replace statutb = 3 if (statut == 3|statut == 4) & wave != 2004 
replace statutb = 4 if statut == 6 & wave == 2004
replace statutb = 4 if statut == 5 & wave != 2004 
replace statutb = 5 if statut == 5 & wave == 2004
replace statutb = 5 if statut == 6 & wave != 2004 
replace statutb = 6 if statut == 7 & wave == 2004
replace statutb = 6 if statut == 7 & wave != 2004 

foreach p in h f{
gen statutb_`p' = .
replace statutb_`p' = 1 if statut_`p' == 1 & wave == 2004
replace statutb_`p' = 1 if statut_`p' == 1 & wave != 2004
replace statutb_`p' = 2 if (statut_`p' == 2|statut_`p'==3) & wave == 2004
replace statutb_`p' = 2 if statut_`p' == 2 & wave != 2004
replace statutb_`p' = 3 if statut_`p' == 4 & wave == 2004
replace statutb_`p' = 3 if (statut_`p' == 3|statut_`p' == 4) & wave != 2004 
replace statutb_`p' = 4 if statut_`p' == 6 & wave == 2004
replace statutb_`p' = 4 if statut_`p' == 5 & wave != 2004 
replace statutb_`p' = 5 if statut_`p' == 5 & wave == 2004
replace statutb_`p' = 5 if statut_`p' == 6 & wave != 2004 
replace statutb_`p' = 6 if statut_`p' == 7 & wave == 2004
replace statutb_`p' = 6 if statut_`p' == 7 & wave != 2004 
}

label define statutb 1 "Salarié de l'Etat" 2 "Salarié collectivité local et entreprise publique" 3 "Salarié privé" 4 "Aide un membre de sa famille" 5 "Salarié chef de son entreprise" 6 "Indépendant" 
label values statutb* statutb

replace statutb = . if actif == 2 
replace statutb_h = . if actif_h == 2   
replace statutb_f = . if actif_f == 2 

gen public_h = (statutb_h == 1 | statutb_h == 2 )
gen public_f = (statutb_f == 1 | statutb_f == 2 ) 

foreach p in h f{
gen cs8pub_`p' = cs8_`p'
replace cs8pub_`p' = 31 if cs8_`p' == 3 & public_`p' == 1
replace cs8pub_`p' = 41 if cs8_`p' == 4 & public_`p' == 1
replace cs8pub_`p' = 51 if cs8_`p' == 5 & public_`p' == 1
replace cs8pub_`p' = 61 if cs8_`p' == 6 & public_`p' == 1
}
replace cs8pub_h = . if cs8pub_h ==0

foreach p in h f{
gen statut_simple_`p' = 1 if statutb_`p' == 1 |statutb_`p' == 2 
replace statut_simple_`p' = 2 if statutb_`p' == 3 | statutb_`p' == 4
replace statut_simple_`p' = 3 if statutb_`p' == 5 | statutb_`p' == 6
replace statut_simple_`p' = 4 if statutb_`p' == . & age_`p' !=.
}
label define statut_simple 1 "Salarié du public (Etat, collectivité, entreprise)" 2 "Salarié privé" 3 "Indépendant" 4 "Sans emploi ou inactif" 
label values statut_simple* statut_simple


*** PCS incluant statut public/privé
gen cs8statut_h = .
replace cs8statut_h = 10 if cs8pub_h == 1
replace cs8statut_h = 20 if cs8pub_h == 2
replace cs8statut_h = 30 if cs8pub_h == 3
replace cs8statut_h = 31 if cs8pub_h == 31
replace cs8statut_h = 40 if cs8pub_h == 4
replace cs8statut_h = 41 if cs8pub_h == 41
replace cs8statut_h = 50 if cs8pub_h == 5
replace cs8statut_h = 51 if cs8pub_h == 51
replace cs8statut_h = 60 if cs8_h == 6
replace cs8statut_h = 66 if (cs24_h  == "66"|cs24_h == "69")&wave !=2004
replace cs8statut_h = 66 if (cs_h  == "67"|cs_h == "68"|cs_h == "69")&wave ==2004
replace cs8statut_h = 80 if cs8pub_h == 8

gen cs8statut_f = .
replace cs8statut_f = 10 if cs8pub_f == 1
replace cs8statut_f = 20 if cs8pub_f == 2
replace cs8statut_f = 30 if cs8pub_f == 3
replace cs8statut_f = 31 if cs8pub_f == 31
replace cs8statut_f = 40 if cs8pub_f == 4
replace cs8statut_f = 41 if cs8pub_f == 41
replace cs8statut_f = 50 if cs8pub_f == 5
replace cs8statut_f = 51 if cs8pub_f == 51
replace cs8statut_f = 60 if cs8_f == 6
replace cs8statut_f = 66 if (cs24_f  == "66"|cs24_f == "69")&wave !=2004
replace cs8statut_f = 66 if (cs_f  == "67"|cs_f == "68"|cs_f == "69")&wave ==2004
replace cs8statut_f = 80 if cs8pub_f == 8


*** classif proposée par Angèle Jannot ***

destring cs42_h cs42_f cs_h cs_f, replace
replace cs42_h = cs_h if wave == 2004
gen cs9_aj_h = ""
replace cs9_aj_h = "A1" if inlist(cs42_h, 13,23,31) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,13,23, 31))
replace cs9_aj_h = "A2" if inlist(cs42_h, 37,38) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,37,38))
replace cs9_aj_h = "A3" if inlist(cs42_h, 33,34,35) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,33,34,35))
replace cs9_aj_h = "B1" if inlist(cs42_h, 11,12,21,22) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h, 11,12,21,22))
replace cs9_aj_h = "B2" if inlist(cs42_h, 46,47,48) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,46,47,48))
replace cs9_aj_h = "B3" if inlist(cs42_h, 42,43,44,45) | ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,42,43,44,45))
replace cs9_aj_h = "C1" if inlist(cs42_h, 53,54)| ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,53,54))
replace cs9_aj_h = "C2" if inlist(cs42_h, 52,55,62,63,64,65)| ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,52,55,62,63,64,65))
replace cs9_aj_h = "C3" if inlist(cs42_h, 56,67,68,69)| ((cs42_h >= 70 & cs42_h != .)& inlist(cs_ante_h,56,67,68,69))
replace cs9_aj_h = "D" if inlist(cs42_h, 81,84,85,86) & cs9_aj_h =="" 

replace cs42_f = cs_f if wave == 2004
gen cs9_aj_f = ""
replace cs9_aj_f = "A1" if inlist(cs42_f, 13,23,31) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,13,23,31))
replace cs9_aj_f = "A2" if inlist(cs42_f, 37,38) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,37,38))
replace cs9_aj_f = "A3" if inlist(cs42_f, 33,34,35) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,33,34,35))
replace cs9_aj_f = "B1" if inlist(cs42_f, 11,12,21,22) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f, 11,12,21,22))
replace cs9_aj_f = "B2" if inlist(cs42_f, 46,47,48) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,46,47,48))
replace cs9_aj_f = "B3" if inlist(cs42_f, 42,43,44,45) | ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,42,43,44,45))
replace cs9_aj_f = "C1" if inlist(cs42_f, 53,54)| ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,53,54))
replace cs9_aj_f = "C2" if inlist(cs42_f, 52,55,62,63,64,65)| ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,52,55,62,63,64,65))
replace cs9_aj_f = "C3" if inlist(cs42_f, 56,67,68,69)| ((cs42_f >= 70 & cs42_f != .)& inlist(cs_ante_f,56,67,68,69))
replace cs9_aj_f = "D" if inlist(cs42_f, 81,84,85,86) & cs9_aj_f =="" 

drop cs cs_h cs_f cs24* cs_ante* statut statut_h statut_f


* variables de diplomes *
gen diplome = . 
replace diplome = 10 if dip14 == 10 & wave == 2010
replace diplome = 10 if inlist(dipdet,11,12,15,16) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 12 if dip14 == 12 & wave == 2010
replace diplome = 12 if inlist(dipdet,13,14) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 20 if dip14 == 20 & wave == 2010
replace diplome = 20 if inlist(dipdet,21,22,23,24) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 30 if dip14 == 30 & wave == 2010
replace diplome = 30 if inlist(dipdet,35,36) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 31 if dip14 == 31 & wave == 2010
replace diplome = 31 if inlist(dipdet,31,32,33) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 33 if dip14 == 33 & wave == 2010
replace diplome = 33 if inlist(dipdet,34) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 41 if dip14 == 41 & wave == 2010
replace diplome = 41 if inlist(dipdet,41,42) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 42 if dip14 == 42 & wave == 2010
replace diplome = 42 if inlist(dipdet,43) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 43 if dip14 == 43 & wave == 2010
replace diplome = 43 if inlist(dipdet,44,45) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 50 if dip14 == 50 & wave == 2010
replace diplome = 50 if inlist(dipdet,51,52,53) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 60 if dip14 == 60 & wave == 2010
replace diplome = 60 if inlist(dipdet,60) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 70 if dip14 == 70 & wave == 2010
replace diplome = 70 if inlist(dipdet,70) & (wave == 2014|wave==2017|wave==2020)
replace diplome = 71 if dip14 == 71 & wave == 2010
replace diplome = 71 if (inlist(dipdet,71)|forter==2) & (wave == 2014|wave==2017|wave==2020)

replace diplome = 10 if inlist(dies,47) & (wave == 2004) & diplome == .
replace diplome = 12 if inlist(dies,48,49) & (wave == 2004) & diplome == .
replace diplome = 20 if inlist(dies,45,46) & (wave == 2004) & diplome == .
replace diplome = 30 if inlist(dies,41) & (wave == 2004) & diplome == .
replace diplome = 31 if inlist(dies,42,43) & (wave == 2004) & diplome == .
replace diplome = 33 if inlist(dies,44) & (wave == 2004) & diplome == .

replace diplome = 41 if inlist(dieg,17) & (wave == 2004) & diplome == .
replace diplome = 42 if inlist(diep,32,36) & (wave == 2004) & diplome == .
replace diplome = 43 if inlist(diep,34) & (wave == 2004) & diplome == .
replace diplome = 44 if inlist(diep,39) & (wave == 2004) & diplome == .

replace diplome = 50 if inlist(diep,23,25,27,29) & (wave == 2004) & diplome == .
replace diplome = 60 if inlist(dieg,15) & (wave == 2004) & diplome == .
replace diplome = 60 if inlist(dieg,16,18,19) & (wave == 2004) & diplome == .
replace diplome = 50 if inlist(diep,21) & (wave == 2004) & diplome == .

replace diplome = 70 if inlist(dieg,02) & (wave == 2004) & diplome == .


foreach p in h f{
gen diplome_`p' = . 
replace diplome_`p' = 10 if dip14_`p' == 10 & wave == 2010
replace diplome_`p' = 10 if inlist(dipdet_`p',11,12,15,16) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 12 if dip14_`p' == 12 & wave == 2010
replace diplome_`p' = 12 if inlist(dipdet_`p',13,14) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 20 if dip14_`p' == 20 & wave == 2010
replace diplome_`p' = 20 if inlist(dipdet_`p',21,22,23,24) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 30 if dip14_`p' == 30 & wave == 2010
replace diplome_`p' = 30 if inlist(dipdet_`p',35,36) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 31 if dip14_`p' == 31 & wave == 2010
replace diplome_`p' = 31 if inlist(dipdet_`p',31,32,33) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 33 if dip14_`p' == 33 & wave == 2010
replace diplome_`p' = 33 if inlist(dipdet_`p',34) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 41 if dip14_`p' == 41 & wave == 2010
replace diplome_`p' = 41 if inlist(dipdet_`p',41,42) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 42 if dip14_`p' == 42 & wave == 2010
replace diplome_`p' = 42 if inlist(dipdet_`p',43) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 43 if dip14_`p' == 43 & wave == 2010
replace diplome_`p' = 43 if inlist(dipdet_`p',44,45) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 50 if dip14_`p' == 50 & wave == 2010
replace diplome_`p' = 50 if inlist(dipdet_`p',51,52,53) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 60 if dip14_`p' == 60 & wave == 2010
replace diplome_`p' = 60 if inlist(dipdet_`p',60) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 70 if dip14_`p' == 70 & wave == 2010
replace diplome_`p' = 70 if inlist(dipdet_`p',70) & (wave == 2014|wave==2017|wave==2020)
replace diplome_`p' = 71 if dip14_`p' == 71 & wave == 2010
replace diplome_`p' = 71 if (inlist(dipdet_`p',71)|forter_`p'==2) & (wave == 2014|wave==2017|wave==2020)

replace diplome_`p' = 10 if inlist(dies_`p',47) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 12 if inlist(dies_`p',48,49) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 20 if inlist(dies_`p',45,46) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 30 if inlist(dies_`p',41) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 31 if inlist(dies_`p',42,43) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 33 if inlist(dies_`p',44) & (wave == 2004) & diplome_`p' == .

replace diplome_`p' = 41 if inlist(dieg_`p',17) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 42 if inlist(diep_`p',32,36) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 43 if inlist(diep_`p',34) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 43 if inlist(diep_`p',39) & (wave == 2004) & diplome_`p' == .

replace diplome_`p' = 50 if inlist(diep_`p',23,25,27,29) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 60 if inlist(dieg_`p',15) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 60 if inlist(dieg_`p',16,18,19) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 50 if inlist(diep_`p',21) & (wave == 2004) & diplome_`p' == .

replace diplome_`p' = 70 if inlist(dieg_`p',02) & (wave == 2004) & diplome_`p' == .
replace diplome_`p' = 71 if inlist(dieg_`p',00) & (wave == 2004) & diplome_`p' == .
*replace diplome_`p' = . if diplome_`p' == 71
replace diplome_`p' = 71 if diplome_`p' == . & age_`p' != .
}


drop diplo diplo_h diplo_f dip14* dip14_h dip14_f dipdet* dipdet_h dipdet_f dieg* diep* dies*
label define diplome 10 "Doctorat ou Master" 12 "Grande école (ingé ou comm)" 20 "Licence, matrise ou Bac+3" 30 "Deug ou Bac+2" 31 "DUT, BTS, ou assimilé" 33 "Paramédical Bac+2" 41 "Bac général" 42 "Bac techno" 43 "Bac pro, brevet pro" 50 "BAP, CEP ou assimilés" 60 "Brevet des collèges" 70 "Certificat d'études promaires" 71 "Aucun diplôme"  
label values diplome diplome
label values diplome_h diplome_f diplome

foreach p in h f{
	gen diplo_simple_`p' = .
	replace diplo_simple_`p' = 1 if diplome_`p' == 10 | diplome_`p' == 12
	replace diplo_simple_`p' = 2 if diplome_`p' == 20 | diplome_`p' == 30 | diplome_`p' == 31 | diplome_`p' == 33
	replace diplo_simple_`p' = 3 if diplome_`p' == 41 | diplome_`p' == 42  	
	replace diplo_simple_`p' = 4 if diplome_`p' == 43 | diplome_`p' == 50  	
	replace diplo_simple_`p' = 5 if diplome_`p' == 60 | diplome_`p' == 70 | diplome_`p' == 71	
}
label define diplo_simple 1 "Master, doctorat, grande école" 2 " Bac+2 ou bac+3" 3 "Bac général ou techno" 4 "Bac pro, BEP, CAP ou assimilés" 5 "Brevet, CEP, aucun diplôme"   
label values diplo_simple_h diplo_simple_f diplo_simple

recode z*_i* (.=0)
gen revenu_i = zsalaires_i + zchomage_i + zretraites_strictes_i + zpenalir_i + zrentes_i + zinvalidite_i if wave != 2004
replace revenu_i = zsali + zchoi + zrsti + zalri + zrtoi if wave == 2004    
gen revenu_non_compta = zragi + zrici + zrnci if wave == 2004
gen revenu_travail_i = zsalaires_i + zchomage_i if wave != 2004
replace revenu_travail_i = zsali + zchoi if wave == 2004  
gen revenu_horstrav_i = zretraites_strictes_i + zpenalir_i + zrentes_i + zinvalidite_i if wave != 2004
replace revenu_horstrav_i = zrsti + zalri + zrtoi if wave == 2004

foreach p in h f{
gen revenu_i_`p' = zsalaires_i_`p' + zchomage_i_`p' + zretraites_strictes_i_`p' + zpenalir_i_`p' + zrentes_i_`p' + zinvalidite_i_`p' if wave != 2004
replace revenu_i_`p' = zsali_`p' + zchoi_`p' + zrsti_`p' + zalri_`p' + zrtoi_`p' if wave == 2004  
gen revenu_non_compta_`p' = zragi_`p' + zrici_`p' + zrnci_`p' if wave == 2004

gen revenu_travail_i_`p' = zsalaires_i_`p' + zchomage_i_`p' if wave != 2004
replace revenu_travail_i_`p' = zsali_`p' + zchoi_`p' if wave == 2004  
gen revenu_horstrav_i_`p' = zretraites_strictes_i_`p' + zpenalir_i_`p' + zrentes_i_`p' + zinvalidite_i_`p' if wave != 2004
replace revenu_horstrav_i_`p' = zrsti_`p' + zalri_`p' + zrtoi_`p' if wave == 2004
}

drop zsalaires_i* zchomage_i* zretraites_strictes_i* zpenalir_i* zrentes_i* zinvalidite_i* zretraites_i* 
drop zsali* zchoi* zrsti* zalri* zrtoi* zrag* zric* zrnc* ztsai* zperi* zreti*
 

recode surface (999998=.)
recode surface (999999=.)


**** correction pour inflation (ref = 1er janvier 2018) ****
/*
CPI : https://www.insee.fr/en/statistiques/serie/001759970
1er janvier 2005 = 85.23
1er janvier 2010 = 93.59
1er janvier 2014 = 98.86
1er janvier 2018 = 101.75
1er janvier 2021 = 105.12
*/

recode revenu* W* (.=0)
foreach var of varlist W* revenu*{
replace `var' = `var'*101.75/85.23 if wave == 2004
replace `var' = `var'*101.75/93.59 if wave == 2010
replace `var' = `var'*101.75/98.86 if wave == 2014 
replace `var' = `var'*101.75/105.12 if wave == 2020 

}	

* drop les DOM (car pas inclus dans EP en 2004)
drop if zeat == "0"

* winsorize at 1 and 99
winsor2 W*, cuts(0.5 99.5) by(wave) replace
drop Wtot 
gen Wtot = Wimmo_net_enf + Wfin_epargne + Wfin_nonep
gen Wfin = Wfin_epargne  + Wfin_nonep


* create 2-year age groups
gen age2 = floor(age/2)*2


* change name of ident for 2010
replace ident = identmen if wave == 2010


* enfant 
gen enfant_h = enfant_couple | enfant_h_seul
gen enfant_f = enfant_couple | enfant_f_seul

recode cs8_h (0=.)


* composition sexuée des 2 premiers enfants
gen samesex =   (sexe_enf1 == sexe_enf2) 
gen samesex_f = (sexe_enf1 == sexe_enf2 & sexe_enf1 == "2") 
gen samesex_h = (sexe_enf1 == sexe_enf2 & sexe_enf1 == "1") 



*** REPONDERATION DES ENQUETES ***
* but : ne pas avoir une enquête sur-représentées 

*replace pond = pond_trans if wave == 2017
bys wave : egen m_pond = mean(pond)
gen pond2 = pond / m_pond

gen activite_enfant = (statutb != .)


gen nb_siblings = nb_enf_all-1
gen nb_siblings_dum_1 = (nb_siblings > 0)
gen nb_siblings_dum_2 = (nb_siblings > 1)
gen nb_siblings_dum_3 = (nb_siblings > 2)

gen nb_enf_all_g = nb_enf_all
recode nb_enf_all_g (3/17 = 3)

gen nb_enf_couple = nb_boys_couple + nb_girls_couple

gen has_asset = (has_compteCh|has_livA|has_livJeune|has_livImpot|has_EpLog|has_livJeune|has_other_asset)
gen fille = (sexe == "2")
gen age12p = (age>=12)

recode Wtot_net_f  Wtot_net_h (min/0 = 0)
gen Wtot_men = (Wtot_net_f + Wtot_net_h )/1000 

g Wfin_momshare = Wtot_net_f/(Wtot_net_f + Wtot_net_h)

g Wtot_menq = .
foreach k in 2004 2010 2014 2017 2020{
xtile Wtot_menq_`k' = Wtot_men if wave == `k' [aw=pond], nquantiles(10)
replace Wtot_menq = Wtot_menq_`k'  if wave == `k'
drop Wtot_menq_`k'
}


g Wtot_menq5 = .
foreach k in 2004 2010 2014 2017 2020{
xtile Wtot_menq5_`k' = Wtot_men if wave == `k' [aw=pond], nquantiles(5)
replace Wtot_menq5 = Wtot_menq5_`k'  if wave == `k'
drop Wtot_menq5_`k'
}


gen Wtot_men_sq = Wtot_men^2
gen revenu_men = (revenu_i_h+revenu_i_f)/1000

gen revenu_momshare = revenu_i_f/(revenu_i_h+revenu_i_f)

g revenu_menq = .
foreach k in 2004 2010 2014 2017 2020{
xtile revenu_menq_`k' = revenu_men if wave == `k' [aw=pond], nquantiles(10)
replace revenu_menq = revenu_menq_`k'  if wave == `k'
drop revenu_menq_`k'
}

g revenu_menq5 = .
foreach k in 2004 2010 2014 2017 2020{
xtile revenu_menq5_`k' = revenu_men if wave == `k' [aw=pond], nquantiles(5)
replace revenu_menq5 = revenu_menq5_`k'  if wave == `k'
drop revenu_menq5_`k'
}

g revenu_i_h_q5 = .
foreach k in 2004 2010 2014 2017 2020{
xtile revenu_i_h_q5_`k' = revenu_men if wave == `k' [aw=pond], nquantiles(5)
replace revenu_i_h_q5 = revenu_i_h_q5_`k'  if wave == `k'
drop revenu_i_h_q5_`k'
}

gen revenu_men_sq = revenu_men^2
gen rank_all_g = rank_all
recode rank_all_g (3/17 = 3)

gen nb_enf_all_g2 = nb_enf_all
recode nb_enf_all_g2 (5/17 = 5)
gen rank_all_g2 = rank_all
recode rank_all_g2 (5/17 = 5)


** mom share 
gen revenu_momshare_c = .
replace revenu_momshare_c = 0 if revenu_momshare == 0
replace revenu_momshare_c = 1 if revenu_momshare >0 & revenu_momshare<0.4
replace revenu_momshare_c = 2 if revenu_momshare >=0.4 & revenu_momshare<=0.6
replace revenu_momshare_c = 3 if revenu_momshare > 0.6 & revenu_momshare< 1
replace revenu_momshare_c = 4 if revenu_momshare == 1
replace revenu_momshare_c = 5 if revenu_i_h + revenu_i_f == 0

** mom share 
gen Wfin_momshare_c = .
replace Wfin_momshare_c = 0 if Wfin_momshare == 0
replace Wfin_momshare_c = 1 if Wfin_momshare >0 & Wfin_momshare<0.4
replace Wfin_momshare_c = 2 if Wfin_momshare >=0.4 & Wfin_momshare<=0.6
replace Wfin_momshare_c = 3 if Wfin_momshare > 0.6 & Wfin_momshare< 1
replace Wfin_momshare_c = 4 if Wfin_momshare == 1
replace Wfin_momshare_c = 5 if Wtot_men == 0



** age des parents
gen age_h_gr = age_h
recode age_h_gr (min/35=1) (36/40=2) (41/45=3) (46/max=4)
lab var age_h_gr "Father's age"
gen age_f_gr = age_f
recode age_f_gr (min/35=1) (36/40=2) (41/45=3) (46/max=4)
lab var age_f_gr "Mother's age"

lab def age_gr 1 "-35" 2 "36-40" 3 "41-45" 4 "46-"
lab val age_h_gr age_f_gr  age_gr


label def age2 0 "0-1" 2 "2-3" 4 "4-5" 6 "6-7" 8 "8-9" 10 "10-11" 12 "12-13" 14 "14-15" 16 "16-17"
label values age2 age2



* family type

gen fam_type = .
replace fam_type = 1 if enfant_couple == 1
replace fam_type = 2 if enfant_f_seul == 1 & age_h !=.
replace fam_type = 3 if enfant_h_seul == 1 & age_f !=.
replace fam_type = 4 if enfant_f_seul == 1 & age_h ==.
replace fam_type = 5 if enfant_h_seul == 1 & age_f ==.

label def fam_type 1 "Both parents" 2 "Mother and step-father" 3 "Father and step-mother" 4 "Lone mother" 5 "Lone father" 
label values fam_type fam_type 




gen diplo_simple_fam = diplo_simple_h if fam_type != 4  
replace diplo_simple_fam = diplo_simple_f if fam_type == 4
replace diplo_simple_fam = diplo_simple_f if fam_type != 4 & diplo_simple_fam==.

gen age_gr_fam = age_h_gr if fam_type != 4  
replace age_gr_fam = age_f_gr if fam_type == 4
replace age_gr_fam = age_f_gr if fam_type != 4 & age_gr_fam == .

gen statut_simple_fam = statut_simple_h if fam_type != 4  
replace statut_simple_fam = statut_simple_f if fam_type == 4
replace statut_simple_fam = statut_simple_f if fam_type != 4 & statut_simple_fam ==.

gen etranger_fam = (etranger_h == 1 | etranger_f == 1)



*** instruments ***

gen has_multi = (mult_birth_hh1 !=.)

gen sex_compo_girls = (sexe_enf1 == "2" & sexe_enf2 == "2")
gen sex_compo_boys = (sexe_enf1 == "1" & sexe_enf2 == "1")
gen sex_compo = (sex_compo_girls == 1 | sex_compo_boys == 1)


* dichotomize controls
tab age2, gen(agegr)
tab wave, gen(wave)
tab revenu_menq5, gen(revenu_menq5)
tab Wtot_menq5, gen(Wtot_menq5)
tab age_h_gr, gen(age_h_gr)
tab diplo_simple_h, gen(diplo_simple_h)
tab statut_simple_h, gen(statut_simple_h)
tab age_f_gr, gen(age_f_gr)
tab diplo_simple_f, gen(diplo_simple_f)
tab statut_simple_f, gen(statut_simple_f)
tab revenu_i_h_q5, gen(revenu_i_h_q5)
tab revenu_momshare_c, gen(revenu_momshare_c)
tab Wfin_momshare_c, gen(Wfin_momshare_c)
tab fam_type, gen(fam_type)

* Labellizations
lab var fille "Girl"
lab var sex_compo "Sex composition"
lab var nb_siblings_dum_1 "Additional sibling"
lab var nb_siblings_dum_2 "Additional sibling"
lab var nb_siblings_dum_3 "Additional sibling"
lab var rank_all "Rank of birth"

gen asinhW = asinh(Wfin)
gen Wfin_dum = 0
replace Wfin_dum = 1 if Wfin > 0

save $analysis_data/assets_allwaves, replace

exit 



/*
zsalaires_i  			zsali           long    %12.0g                salaires au sens strict 
zchomage_i   			zchoi           long    %12.0g                indemnit{s de chomage et de pr{retraite
zretraites_strictes_i 	zrsti           long    %12.0g                retraites au sens strict
zpenalir_i   			zalri           long    %12.0g                pensions alimentaires re\ues
zrentes_i    			zrtoi           long    %12.0g                rentes viag}res @ titre on{reux
zinvalidite_i

zragi           long    %12.0g                revenus agricoles
zrici           long    %12.0g                revenus industriels et commerciaux professionnels
zrnci           long    %12.0g                revenus non commerciaux professionnels

ztsai           long    %12.0g                revenus salariaux =sum(zsali,zchoi)
zperi           long    %12.0g                pensions, retraites, rentes =sum(zrsti,zalri,zrtoi)
zreti           long    %12.0g                pensions et retraites =sum(zrsti,zalri)
zretraites_i = zinvalidite_i_h + zretraites_strictes_i_h 
*/

/*
10 Diplôme de 3e cycle universitaire (DES, DEA, DESS, master), doctorat (y compris professions
de santé)
12 Diplôme d'une grande école (ingénieur, commerce …)
20 Diplôme de 2e cycle universitaire (licence, maîtrise …) ou ne sait pas quel diplôme de ce niveau
30 Diplôme de 1er cycle universitaire
31 BTS, DUT, DEUST ou équivalent ou ne sait pas quel diplôme de niveau bac + 2
33 Diplôme des professions sociales et de la santé de niveau bac + 2 (infirmière, …)
41 Baccalauréat général (séries A, B, C, D, E, ES, L, S), brevet supérieur, capacité en droit, DAEU
ou diplôme étranger de même niveau
42 Baccalauréat technologique (séries F, G, H, SMS, STI, STL, STT) ou ne sait pas quel diplôme
de ce niveau
43 Baccalauréat professionnel
44 Brevet professionnel ou de technicien de maîtrise, BEA, BEC, BEI, BEH, BSEC
50 Diplôme ou titre de niveau BEP ou CAP
60 Brevet des collèges, BEPC, brevet élémentaire ou diplôme étranger de même niveau
70 Certificat d'études primaires (CEP) ou diplôme étranger de même niveau
71 Aucun diplôme ou études initiales en cours
*/

* correspondances de variables 
*en 2010 educstat = etudes
*en 2010 forter = forminit

*entre 2010 et 2004

/*
etudes+forminit = etudi 
etudes educstat
pond => pond 
z* => z* => dans la table revenus 2004 */

/*
classif = posit 

foreach p in f h{
replace classif_`p' = 1 if wave == 2004 & posit_`p' == 1  
replace classif_`p' = 2 if wave == 2004 & posit_`p' == 2 
replace classif_`p' = 3 if wave == 2004 & posit_`p' == 5 
replace classif_`p' = 4 if wave == 2004 & posit_`p' == 6 
replace classif_`p' = 5 if wave == 2004 & (posit_`p' == 3|posit_`p' == 4)
replace classif_`p' = 6 if wave == 2004 & posit_`p' == 8
replace classif_`p' = 7 if wave == 2004 & posit_`p' == 7
replace classif_`p' = 8 if wave == 2004 & (posit_`p' == 9|posit_`p' == 10)
}
01 Manoeuvre ou ouvrier(e) spécialisé(e)
02 Ouvrier(e) qualifié(e) ou hautement qualifié(e), technicien(ne) d'atelier
03 Technicien(ne)
04 Personnel de catégorie B ou assimilé
05 Agent de maîtrise, maîtrise administrative ou commerciale, VRP (non cadre)
06 Personnel de catégorie A ou assimilé
07 Ingénieur, cadre (à l'exception des directeurs généraux ou de ses adjoints directs)
08 Personnel de catégorie C, D ou assimilé
09 Employé(e) de bureau, employé(e) de commerce, personnel de service
10 Directeur général, adjoint direct
*/

