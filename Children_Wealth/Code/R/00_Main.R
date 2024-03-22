#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                                                                              #
#                           CHILDREN'S WEALTH                                  #
#                                                                              #
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                                                                              #
# 00_Main: master file, managing working paths, global settings and            #
# meant to be able to run all scripts at once.                                 #
#                                                                              #
#------------------------------------------------------------------------------#
# Inputs: French Wealth Survey datasets (2004, 2010, 2014, 2018, 2020)         #
# Outputs: Database with the wealth (real estate and financial assets,         #
# no business assets) directly owned by children.                              #
#------------------------------------------------------------------------------#
# Creation date: 2020                                                          #
# Update: 03/2024                                                              #
#------------------------------------------------------------------------------#
# Author(s): Marion Leturcq / Mathis Sansu                                     #
#------------------------------------------------------------------------------#

#-------------------------#
#       SETTINGS          #
#-------------------------#

load_packages <- TRUE


#-------------------------#
#       PATHS             #
#-------------------------#

# MODIFY THAT HERE
pat04 <- "C://Users//mthsa//Desktop//Data//HVP//raw_dta//2004//EP2004_Nicolas"			    # input = HVP 2004 (MODIFY NICOLAS)
pat10 <- "C://Users//mthsa//Desktop//Data//HVP//raw_dta//2010//EP2010_Quetelet//Stata"		# input = HVP 2010
pat14 <- "C://Users//mthsa//Desktop//Data//HVP//raw_dta//2014//Stata"			              # input = HVP 2014
pat18 <- "C://Users//mthsa//Desktop//Data//HVP//raw_dta//2018//Stata"			              # input = HVP 2018
# MODIFY THAT HERE
pat20 <- "C://Users//mthsa//Desktop//Data//HVP//raw_dta//2020_temp//stata"			        # input = HVP 2020

temp <- "C://Users//mthsa//Desktop//Data//HVP//temp2"			                            # folder for outputs

dofiles <- "C://Users//mthsa//Nextcloud//WIDE//Children_Wealth//Code//R"			                    # folder with code files


#-------------------------#
#   SCRIPTS' ROADMAP      #
#-------------------------#
  
# This is designed to be able to execute all scripts at once
# as well as to select specific scripts to run (by commenting).

source(paste0(dofiles, "//01_Construct_CW_Pat04.R"))                            # output = $temp/assets_2004
source(paste0(dofiles, "//01_Construct_CW_Pat10.R"))                            # output = $temp/assets_2010
source(paste0(dofiles, "//01_Construct_CW_Pat14.R"))                            # output = $temp/assets_2014
source(paste0(dofiles, "//01_Construct_CW_Pat18.R"))                            # output = $temp/assets_2017
source(paste0(dofiles, "//01_Construct_CW_Pat20.R"))                            # output = $temp/assets_2020

# This script can be run only if the outputs from the previous scripts exist.
source(paste0(dofiles, "//02_Construct_CW_MergeAllWaves.R"))                    # output = $temp/assets_allwaves

