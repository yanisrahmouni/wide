*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*                                                                              *
*                           CHILDREN'S WEALTH                                  *
*                                                                              *
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*                                                                              *
* 00_Main: master file, managing working paths, global settings and            *
* meant to be able to run all scripts at once.                                 *
*                                                                              *
*------------------------------------------------------------------------------*
* Inputs: French Wealth Survey datasets (2004, 2010, 2014, 2018, 2020)         *
* Outputs: Database with the wealth (real estate and financial assets,         *
* no business assets) directly owned by children.                              *
*------------------------------------------------------------------------------*
* Creation date: 2020                                                          *
* Update: 03/2024                                                              *
*------------------------------------------------------------------------------*
* Author(s): Marion Leturcq / Mathis Sansu                                     *
*------------------------------------------------------------------------------*


* Code running without stopping in Stata
set more off


*-------------------------*
*       PATHS             *
*-------------------------*

global pat04_revenus "XXXXXXXXXX"			// input not in Quetelet data

global pat04 "XXXXXXXXXX"			// input = HVP 2004
global pat10 "XXXXXXXXXX"			// input = HVP 2010
global pat14 "XXXXXXXXXX"			// input = HVP 2014
global pat18 "XXXXXXXXXX"			// input = HVP 2018
global pat20 "XXXXXXXXXX"			// input = HVP 2020

global temp  "XXXXXXXXXX"			// folder for outputs

global analysis_data  "XXXXXXXXXX"			// folder for outputs

global dofiles "XXXXXXXXXX"			// folder with code files


*-------------------------*
*   SCRIPTS' ROADMAP      *
*-------------------------*

* This is designed to be able to execute all scripts at once
* as well as to select specific scripts to run (by commenting).

do $dofiles/01_Construct_CW_Pat04         // output = $temp/assets_2004
do $dofiles/01_Construct_CW_Pat10         // output = $temp/assets_2010
do $dofiles/01_Construct_CW_Pat14         // output = $temp/assets_2014
do $dofiles/01_Construct_CW_Pat18         // output = $temp/assets_2017
do $dofiles/01_Construct_CW_Pat20         // output = $temp/assets_2020

* This script can be run only if the outputs from the previous scripts exist.
do $dofiles/02_Construct_CW_MergeAllWaves // output = $temp/assets_allwaves


exit
