# DESCRIPTION ------------------------------------------------------------------
# Description: Converts French educational degrees into ISCED levels through 
# manual recoding (1998-2017)
#
# Inputs: metadata.csv, derived from the metadata.R script. 
# Output: ISCED_metadata.csv.

# I. PACKAGES ------------------------------------------------------------------
# If the following packages are not already installed on your RStudio, please use
# the -install.packages()- function. 
library("dplyr")

# II. SETTINGS -----------------------------------------------------------------
setwd("/Users/yanis/Desktop/WIDE/Harmonization") # TO EDIT 
Sys.setenv(LANG = "en")

# III. CONSTANTS ---------------------------------------------------------------
# [1] Pattern to match degree-related variables.
degree <- "Diplôme|Diplômes"

# III. MAIN SCRIPT -------------------------------------------------------------
# [1] Load file and filter for degree variables and categories.
metadata <- read.csv("./metadata.csv")
degree_metadata <- metadata[grepl(degree, metadata$desc, ignore.case = TRUE), ]
degree_metadata <- degree_metadata[!(
  grepl("Aucun diplôme", degree_metadata$label) | 
    grepl("Sans diplôme", degree_metadata$label) |
    (degree_metadata$category == "9" & degree_metadata$label == "Ne sait pas") |
    degree_metadata$first_id == "DIPLOREL"
), ]

# [2] Create empty columns for the ISCED number and the ISCED official label. 
# Sources: https://isced.uis.unesco.org/data-mapping/ (France - 2011 Excel file),
# https://uis.unesco.org/sites/default/files/documents/international-standard-classification-of-education-isced-2011-en.pdf
degree_metadata$ISCED_number <- NA
degree_metadata$ISCED_label <- NA
degree_metadata$ISCED_desc <- NA

# [3] Fill ISCED_number and ISCED_label based on the provided sources, for each
# degree variable of the Patrimoine survey. 
degree_metadata <- degree_metadata %>%
  mutate(
    # [3.1] SCOLAPR, SCOLACJ (Available in 2010, 2014 & 2017).
    ISCED_number = case_when(
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 0 ~ "010, 020, 030",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 1 ~ "100",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 2 ~ "244",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 3 ~ "344, 354",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 1 ~ "010",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 2 ~ "100",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 3 ~ "244",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 4 ~ "344, 354",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 0 ~ "Never attended an education programme., Some early childhood education., Some primary education (without level completion).",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 1 ~ "Primary, Including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion.",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 2 ~ "Lower secondary education, General, with direct access to upper Level completion, with direct access to upper secondary education",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% c(2014, 2017) & category == 3 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 1 ~ " Never attended an education programme",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 2 ~ "Primary, Including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion.",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 3 ~ "Lower secondary education, General, with direct access to upper Level completion, with direct access to upper secondary education",
      first_id %in% c("SCOLAPR", "SCOLACJ") & year %in% 2010 & category == 4 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.2] DIPDET, DIPLOMPR, DIPLOMCJ (Available in 2014 & 2017).
    ISCED_number = case_when(
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 0 ~ "999",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 11 ~ "840",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 12 ~ "750",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 13 ~ "750",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 14 ~ "750",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 15 ~ "750",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 16 ~ "740",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 17 ~ "740",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 21 ~ "640, 650",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 22 ~ "640",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 23 ~ "650",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 24 ~ "640, 650",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 31 ~ "550",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 32 ~ "550",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 33 ~ "550",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 34 ~ "550",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 35 ~ "540",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 36 ~ "540, 550",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 41 ~ "444",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 42 ~ "344",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 43 ~ "344",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 44 ~ "354",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 45 ~ "344, 353",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 51 ~ "353",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 52 ~ "353",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 53 ~ "353",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 60 ~ "244",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 70 ~ "100",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 71 ~ "010, 020, 030",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 0 ~ "Not elsewhere classified",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 11 ~ "Doctoral or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 12 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 13 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 14 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 15 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 16 ~ "Master’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 17 ~ "Master’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 21 ~ "Bachelor’s or equivalent level, Academic, Not further defined., Bachelor’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 22 ~ "Bachelor’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 23 ~ "Bachelor’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 24 ~ "Bachelor’s or equivalent level, Academic, Not further defined., Bachelor’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 31 ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 32 ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 33 ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 34 ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 35 ~ "Short-cycle tertiary education, General, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 36 ~ "Short-cycle tertiary education, General, Not further defined., Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 41 ~ "Post-secondary non-tertiary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 42 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 43 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 44 ~ "Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 45 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 51 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 52 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 53 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 60 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 70 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIPDET", "DIPLOMPR", "DIPLOMCJ") & year %in% c(2014, 2017) & category == 71 ~ "Never attended an education programme., Some early childhood education., Some primary education (without level completion)",
      TRUE ~ ISCED_label
    ),
    
    # [3.3] HODIP (Available in multiple years, various categories).
    ISCED_number = case_when(
      grepl("^HODIP", first_id) & grepl("^Supérieur à", label) ~ "640, 650",
      grepl("^HODIP", first_id) & grepl("CAP", label, ignore.case = TRUE) ~ "353",
      grepl("^HODIP", first_id) & grepl("^Bac\\+2", label) ~ "540, 550",
      grepl("^HODIP", first_id) & grepl("^Sans diplôme", label) ~ "254",
      grepl("^HODIP", first_id) & year == 1998 & grepl("Baccalauréat", label) ~ "344, 354",
      grepl("^HODIP", first_id) & year != 1998 & grepl("^Baccalauréat professionnel", label) ~ "354",
      grepl("^HODIP", first_id) & year != 1998 & grepl("^Baccalauréat général", label) ~ "344",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      grepl("^HODIP", first_id) & grepl("^Supérieur à", label) ~ "Bachelor's or equivalent level, or more.",
      grepl("^HODIP", first_id) & grepl("CAP", label, ignore.case = TRUE) ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      grepl("^HODIP", first_id) & grepl("^Bac\\+2", label) ~ "Short-cycle tertiary education, General, Not further defined., Short-cycle tertiary education, Vocational, Not further defined",
      grepl("^HODIP", first_id) & grepl("^Sans diplôme", label) ~ "No degree above lower secondary education.",
      grepl("^HODIP", first_id) & year == 1998 & grepl("Baccalauréat", label) ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      grepl("^HODIP", first_id) & year != 1998 & grepl("^Baccalauréat professionnel", label) ~ "Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      grepl("^HODIP", first_id) & year != 1998 & grepl("^Baccalauréat général", label) ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.4] DIPSUPE, DIPSUPEPR, DIPSUPECJ (Available in 2010).
    ISCED_number = case_when(
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 41 ~ "540",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category %in% c(42, 44) ~ "550",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 51 ~ "640",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 63 ~ "750",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 71 ~ "740, 750, 840",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 41 ~ "Short-cycle tertiary education, General, Not further defined",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category %in% c(42, 44) ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 51 ~ "Bachelor’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 63 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIPSUPE", "DIPSUPEPR", "DIPSUPECJ") & year == 2010 & category == 71 ~ "Master’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Professional, Not further defined., Doctoral or equivalent level, Academic, Not further defined",
      TRUE ~ ISCED_label
    ),
    
    # [3.5] DIPTECH, DIPTECHCJ, DIPTECHPR (Available in multiple years, various categories).
    ISCED_number = case_when(
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category %in% c(23, 24, 29, 33) ~ "353",
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category == 32 ~ "344",
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category == 34 ~ "354",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category %in% c(23, 24, 29, 33) ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category == 32 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPTECH", "DIPTECHCJ", "DIPTECHPR") & category == 34 ~ "Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.6] DIPGENE, DIPGENEPR, DIPGENECJ (Available in multiple years, various categories).
    ISCED_number = case_when(
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 02 ~ "100",
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 15 ~ "244",
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 17 ~ "344, 444",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 02 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 15 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIPGENE", "DIPGENEPR", "DIPGENECJ") & category == 17 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Post-secondary non-tertiary education, General, Level completion, with direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.7] DIEG, DIEGCJ, DIEGPR (Available in 1998 and 2004).
    ISCED_number = case_when(
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 02 ~ "100",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 15 ~ "244",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 16 ~ "344",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 17 ~ "344",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 18 ~ "343",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 19 ~ "344",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 02 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 15 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 16 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 17 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 18 ~ "Upper secondary education, General, Level completion, without direct access to tertiary education",
      first_id %in% c("DIEG", "DIEGCJ", "DIEGPR") & year %in% c(1998, 2004) & category == 19 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.8] DIEP, DIEPCJ, DIEPPR (Available in 1998 and 2004).
    ISCED_number = case_when(
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category %in% c(21, 23, 25, 27, 29) ~ "353",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 32 ~ "344",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 34 ~ "354",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 36 ~ "344, 354",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 39 ~ "353",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category %in% c(21, 23, 25, 27, 29) ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 32 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 34 ~ "Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 36 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      first_id %in% c("DIEP", "DIEPCJ", "DIEPPR") & year %in% c(1998, 2004) & category == 39 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      TRUE ~ ISCED_label
    ),
    
    # [3.9] DIES, DIESPR, DIESCJ (Available in 1998 and 2004).
    ISCED_number = case_when(
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 41 ~ "540",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category %in% c(42, 43, 44) ~ "550",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 45 ~ "650",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 46 ~ "640",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 47 ~ "740, 750, 840",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 48 ~ "750",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 49 ~ "750",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 41 ~ "Short-cycle tertiary education, General, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category %in% c(42, 43, 44) ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 45 ~ "Bachelor’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 46 ~ "Bachelor’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 47 ~ "Master’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Professional, Not further defined., Doctoral or equivalent level, Academic, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 48 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIES", "DIESPR", "DIESCJ") & year %in% c(1998, 2004) & category == 49 ~ "Master’s or equivalent level, Professional, Not further defined",
      TRUE ~ ISCED_label
    ),
    
    # [3.10] DIPLOPR, DIPLOCJ (Available in 1998 and 2004).
    ISCED_number = case_when(
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 1 ~ "100",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 2 ~ "353",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 3 ~ "244",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category %in% c(4, 5) ~ "344",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 6 ~ "540, 550, 640",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 7 ~ "740, 750, 840",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 1 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 2 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 3 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category %in% c(4, 5) ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 6 ~ "Short-cycle tertiary education, General, Not further defined., Short-cycle tertiary education, Vocational, Not further defined., Bachelor’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIPLOPR", "DIPLOCJ") & year == 1998 & category == 7 ~ "Master’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Professional, Not further defined., Doctoral or equivalent level, Academic, Not further defined",
      TRUE ~ ISCED_label
    ),
    
    # [3.11] DIPLO, DIPLOPR, DIPLOCJ (Available in 2004).
    ISCED_number = case_when(
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 1 ~ "100",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 2 ~ "353",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 3 ~ "244",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category %in% c(4, 5) ~ "344",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 6 ~ "540, 550",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 7 ~ "640, 740, 750, 840",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 1 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 2 ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 3 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category %in% c(4, 5) ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 6 ~ "Short-cycle tertiary education, General, Not further defined., Short-cycle tertiary education, Vocational, Not further defined.",
      first_id %in% c("DIPLO", "DIPLOPR", "DIPLOCJ") & year == 2004 & category == 7 ~ "Bachelor’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Professional, Not further defined., Doctoral or equivalent level, Academic, Not further defined",
      TRUE ~ ISCED_label
    ),
    
    # [3.12] DIP14, DIP14PR, DIP14CJ (Available in 2010).
    ISCED_number = case_when(
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 10 ~ "740, 750, 840",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 12 ~ "750",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 20 ~ "640",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 30 ~ "540",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category %in% c(31, 33) ~ "550",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 41 ~ "344, 444",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 42 ~ "344",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 43 ~ "354",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category %in% c(44, 50) ~ "353",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 60 ~ "244",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 70 ~ "100",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 71 ~ "010, 020, 030",
      TRUE ~ ISCED_number
    ),
    ISCED_label = case_when(
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 10 ~ "Master’s or equivalent level, Academic, Not further defined., Master’s or equivalent level, Professional, Not further defined., Doctoral or equivalent level, Academic, Not further defined",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 12 ~ "Master’s or equivalent level, Professional, Not further defined",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 20 ~ "Bachelor’s or equivalent level, Academic, Not further defined",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 30 ~ "Short-cycle tertiary education, General, Not further defined., Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category %in% c(31, 33) ~ "Short-cycle tertiary education, Vocational, Not further defined",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 41 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education., Post-secondary non-tertiary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 42 ~ "Upper secondary education, General, Level completion, with direct access to tertiary education",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 43 ~ "Upper secondary education, Vocational, Level completion, with direct access to tertiary education",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category %in% c(44, 50) ~ "Upper secondary education, Vocational, Level completion, without direct access to tertiary education",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 60 ~ "Lower secondary education, General, Level completion, with direct access to upper secondary education",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 70 ~ "Primary education, including recognised successful completion of a lower secondary programme insufficient for level completion or partial level completion",
      first_id %in% c("DIP14", "DIP14PR", "DIP14CJ") & year == 2010 & category == 71 ~ "Never attended an education programme., Some early childhood education., Some primary education (without level completion)",
      TRUE ~ ISCED_label
    )
  )

# [4] Some degree-related variables in 2010 are binary with Yes and No as possible
# answers. We create a variable for their description. 

missing_percentage <- sum(is.na(degree_metadata$ISCED_number)) / length(degree_metadata$ISCED_number) * 100
write.csv(degree_metadata, "/Users/yanis/Desktop/WIDE-R/Harmonization/degree_metadata.csv")