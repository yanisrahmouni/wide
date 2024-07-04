# DESCRIPTION ------------------------------------------------------------------
# This R script creates a function called -wide_metadata()- which is based on the 
# -pdftools- package. -wide_metadata()- extracts metadata from the Patrimoine pdf
# documentation files and turns it into a usable, category-level data frame. 
#
# Inputs: Raw .pdf files obtained from Quetelet.
# Output: metadata.R
#
# This script is not to be modified unless there is an "TO EDIT" mention. Tested
# on all waves between 1998 and 2017. 

# I. PACKAGES ------------------------------------------------------------------
# If the following packages are not already installed on your RStudio, please use
# the -install.packages()- function. 
packages <- c("pdftools", "dplyr", "tidyverse", "stringr", "data.table", "tidytext")
invisible(lapply(packages, library, character.only = TRUE))

# II. SETTINGS -----------------------------------------------------------------
setwd("D:/WIDE-R/inputs/raw") # TO EDIT 
Sys.setenv(LANG = "en")

# III. CONSTANTS ---------------------------------------------------------------
# [1] Defining the waves of interest.
waves <- c("1998", "2004", "2010", "2014", "2017", "2020") # TO EDIT

# [2] Defining .pdf file names. 
pdf_names <- list(
  pdf_1998 = "1998/Doc/Patrimoine98dcodes.pdf",
  pdf_2004 = "2004/Doc/dictionnaire des codes_long.pdf",
  pdf_2010 = "2010/Doc/DICO DES CODES _FPR_.pdf",
  pdf_2014 = "2014/Doc/dico_patrimoine_2014-15_final_v2.pdf",
  pdf_2017 = "2017/Doc/PATRIMOINE 2017_18 Dictionnaire des codes.pdf",
  pdf_2020 = "2020/Doc/Dico_HVP_2021_VF2.pdf"
)

# [3] Defining the beginning and ending pages of interest for each .pdf file.
page_limits <- list(
  wave_1998 = c(begin = 22, end = 308), 
  wave_2004 = c(begin = 29, end = 233), 
  wave_2010 = c(begin = 29, end = 277), 
  wave_2014 = c(begin = 39, end = 318),
  wave_2017 = c(begin = 45, end = 349)  
)

# [4] Defining the font characteristics of variable names for each .pdf file.
font_char <- list(
  wave_1998 = c(name = "HINGBK+Arial,Bold", size = "12"),
  wave_2004 = c(name = "Arial-BoldMT", size = "10.98"),
  wave_2010 = c(name = "Helvetica-Bold", size = "11.9999952"),
  wave_2014 = c(name = "Helvetica-Bold", size = "11.9999952"),
  wave_2017 = c(name = "BAAAAA+Arial-BoldMT", size = "12")
)

# [5] Defining the font characteristics of descriptions for each .pdf file. 
font_char_desc <- list(
  wave_1998 = c(name = "HINFMJ+Arial", size = "12"),
  wave_2004 = c(name = "ArialMT", size = "10.02"),
  wave_2010 = c(name = "Helvetica-Bold", size = "9.959996016"),
  wave_2014 = c(name = "Helvetica-Bold", size = "9.959996016"),
  wave_2017 = c(name = "BAAAAA+Arial-BoldMT", size = "10")
)

# [6] Defining the regex patterns to search for variable names for each .pdf file.
patterns <- list(
  wave_1998 = "^[A-Z][-A-Z0-9_]*[A-Z0-9]*\\*?$",
  wave_2004 = "^[A-Z][-A-Z0-9_]*[A-Z0-9]*\\*?$",
  wave_2010 = "^[A-Z][-A-Z0-9_]*(?:[A-Z0-9]*x+)?\\*?$",
  wave_2014 = "^[A-Z][-A-Z0-9_]*[A-Z0-9]*\\*?$",
  wave_2017 = "^[A-Z][-A-Z0-9_]*[A-Z0-9]*\\*?$"
)

# [6] Defining y-coordinates to delete unnecessary information below them. 
y_footnotes <- list(
  wave_1998 = 780,
  wave_2004 = 790,
  wave_2010 = 780,
  wave_2014 = 800,
  wave_2017 = 770
)

# [7] Defining x-coordinates range for category placement. 
x_categories <- list(
  wave_1998 = c(lower = 80, upper = 150), 
  wave_2004 = c(lower = 80, upper = 130),
  wave_2010 = c(lower = 60, upper = 120),
  wave_2014 = c(lower = 60, upper = 120),
  wave_2017 = c(lower = 55, upper = 125)
)

# [8 - YEAR-SPECIFIC] Defining the x-coordinate threshold to erase "Variable calculée"
x_calculee <- 512

# IV. FUNCTIONS ----------------------------------------------------------------
# [1] Defining -order_pdf_data()- function. This function orders the output of 
# pdftools::pdf_data() by page, y-coordinate, and x-coordinate. This is done to
# address incorrect processing from -pdf_data()-.
order_pdf_data <- function(pdf_data) {
  pdf_data <- pdf_data[order(pdf_data$page_number, pdf_data$y, pdf_data$x), ]
  pdf_data <- data.frame(pdf_data, row.names = NULL)
  pdf_data <- pdf_data %>%
    mutate(obs_number = row_number())
  
  return(pdf_data)
}

# [2] Defining -wide_scrape_text()- function. This function uses -pdftools::pdf_text()-
# to extract text from .pdf files and create a data frame with line-level details.
wide_scrape_text <- function(year) {
  
  #  [2.1] Fetching page limits.
  page_range <- page_limits[[paste0("wave_", year)]]
  begin_page <- page_range[1]
  end_page <- page_range[2]
  
  #  [2.2] Extracting text from the .pdf file
  pdf_text <- pdf_text(pdf_names[[paste0("pdf_", year)]])
  
  #  [2.3] Formatting and assigning pages.
  pdf_text <- paste(unlist(pdf_text), "page_break", sep = "")
  pdf_text <- as.data.frame(str_split(pdf_text, "\n", simplify = TRUE))
  pdf_text <- as.data.frame(t(pdf_text))
  pdf_text[] <- lapply(pdf_text, function(x) if (is.character(x)) trimws(x) else x)
  pdf_text <- stack(pdf_text)
  pdf_text <- subset(pdf_text, select = -ind)
  pdf_text <- pdf_text %>% 
    rename(V1 = values)
  pdf_text$page_number <- cumsum(grepl("^page_break", pdf_text$V1))+1
  pdf_text <- pdf_text %>%
    filter(page_number >= begin_page & page_number <= end_page)
  page_break <- which(grepl("^page_break", pdf_text$V1))
  page_break <- page_break - 1
  pdf_text <- as.data.frame(pdf_text[-page_break, ])
  pdf_text <- pdf_text[-nrow(pdf_text), ]
  pdf_text$V1 <- gsub('page_break', '', pdf_text$V1)
  pdf_text <- pdf_text %>% filter(V1 != "")
  
  #  [2.4 - YEAR-SPECIFIC] Identifying the table associated with each variable.
  if (year == 2004) {
    flag_dictionnaire <- which(grepl("Dictionnaire des codes", pdf_text$V1) | grepl("Table RISK", pdf_text$V1))
  } else {
    flag_dictionnaire <- which(grepl("Dictionnaire des codes", pdf_text$V1))
  }
  
  pages_table <- pdf_text$page_number[flag_dictionnaire]
  flag_tables <- which(grepl("Table", pdf_text$V1) & 
                         pdf_text$page_number %in% pages_table)
  if (year == 2010) {
    flag_tables <- flag_tables[grepl("^[0-9]", pdf_text$V1[flag_tables])]
  }
  
  pdf_text$table <- ""
  for (i in 1:length(flag_tables)) {
    table_end <- ifelse(i < length(flag_tables), flag_tables[i + 1] - 1, nrow(pdf_text))
    content <- pdf_text$V1[(flag_tables[i] + 1):table_end]
    pdf_text$table[(flag_tables[i] + 1):table_end] <- rep(pdf_text$V1[flag_tables[i]], length(content))
  }
  pdf_text$table <- gsub(".*Table ", "", pdf_text$table)
  pdf_text <- pdf_text %>% 
    filter(!page_number %in% pages_table)
  
  #  [2.5 - YEAR-SPECIFIC] Correcting malformed expressions by adding spaces (e.g., 0à10 to 0 à 10)
  pattern <- "(\\d+)à(\\d*)|(?<=\\d)à|à(?=\\d)"
  replacement <- "\\1 à \\2"
  pdf_text$V1 <- gsub(pattern, replacement, pdf_text$V1, perl = TRUE)
  
  
  if (year == 2017) {
    poste <- which(grepl("postesListe", pdf_text$V1))
    replacement1 <- "postes                                                    Liste"
    
    if (length(poste) > 0) {
      for (i in seq_along(poste)) {
        pdf_text$V1[poste[i]] <- gsub("postesListe", replacement1, pdf_text$V1[poste[i]])
      }
    }
    
    pdf_text_poste <- pdf_text %>%
      mutate(New_V1 = strsplit(as.character(V1), ""))
  }
  
  
  #  [2.6 - YEAR-SPECIFIC] Splitting lines to separate variable names from format.
  pdf_text_split <- pdf_text %>%
    mutate(New_V1 = strsplit(as.character(V1), "\\s{30,} ")) %>%
    unnest(New_V1)
  
  condition <- grepl("^Variable numérique", pdf_text_split$New_V1)
  pdf_text_split <- pdf_text_split %>%
    mutate(
      New_V1 = ifelse(condition, 
                      strsplit(as.character(New_V1), "\\s{2,}"), 
                      New_V1)
    ) %>%
    unnest(New_V1)
  
  pdf_text <- pdf_text_split %>%
    select(New_V1, table, page_number)
  
  names(pdf_text)[names(pdf_text) == "New_V1"] <- "V1"
  
  #  [2.7 - YEAR-SPECIFIC] Removing unnecessary information from the data frame.
  if (year == 2004 | year == 1998) {
    calculee <- which(grepl("^Variable calculée$", pdf_text$V1))
    pdf_text <- pdf_text[-calculee, , drop = FALSE]
    
    if (year == 1998) {
      unit_measure <- which(grepl("^En ", pdf_text$V1))
      pdf_text <- pdf_text[-unit_measure, , drop = FALSE]
      
      subtitle_menage <- which(grepl("^Table MENAGE$", pdf_text$V1) | grepl("^Table MENAGE -", pdf_text$V1))
      pdf_text <- pdf_text[-subtitle_menage, , drop = FALSE]
    }
  }

  #  [2.8] Tokenizing pdf_text in order to merge it with -pdf_data()-'s output.
  pdf_text <- pdf_text %>%
    mutate(id = V1) %>%
    unnest_tokens(V2, V1, to_lower = FALSE, token = "regex", pattern = "\\s+")

  #  [2.9 - YEAR-SPECIFIC] Correcting typos from the pdf file.
  if (year == 2010) {
    pdf_text$id[pdf_text$id == "RMALDMTCD"
                  & pdf_text$page_number == 188] <- "RMALMTCD"
    
    pdf_text$V2[pdf_text$V2 == "RMALDMTCD"
                & pdf_text$page_number == 188] <- "RMALMTCD"
    
    pdf_text$id[pdf_text$id == "ZCGSCRDS"
                  & pdf_text$page_number == 150] <- "ZCSGCRDS"
    
    pdf_text$V2[pdf_text$V2 == "ZCGSCRDS"
                & pdf_text$page_number == 150] <- "ZCSGCRDS"
  }
  
  if (year == 2017) {
    pdf_text$id[pdf_text$id == "LEE" & pdf_text$page_number == 223][2] <- "LEE_DECL"
    pdf_text$V2[pdf_text$V2 == "LEE" & pdf_text$page_number == 223 & pdf_text$id == "LEE_DECL"] <- "LEE_DECL"
    
    pdf_text$id[pdf_text$id == "PATRI_BRUT_CORR*"
                  & pdf_text$page_number == 116] <- "PATRI_BRUT_CORR"
    
    pdf_text$id[pdf_text$id == "PATPROFHENT_CORR*"
                  & pdf_text$page_number == 113] <- "PATPROFHENT_CORR"
    
    pdf_text$id[pdf_text$id == "PATRI_BRUT_HORSRESTE_CORR*"
                  & pdf_text$page_number == 116] <- "PATRI_BRUT_HORSRESTE_CORR"
    
    pdf_text$id[pdf_text$id == "PATRI_NET_CORR*"
                  & pdf_text$page_number == 117] <- "PATRI_NET_CORR"
    
    pdf_text$id[pdf_text$id == "PATRI_NET_HORSRESTE_CORR*"
                  & pdf_text$page_number == 117] <- "PATRI_NET_HORSRESTE_CORR"
    
    pdf_text$V2[pdf_text$V2 == "PATRI_BRUT_CORR*"
                & pdf_text$page_number == 116] <- "PATRI_BRUT_CORR"
    
    pdf_text$V2[pdf_text$V2 == "PATPROFHENT_CORR*"
                & pdf_text$page_number == 113] <- "PATPROFHENT_CORR"
    
    pdf_text$V2[pdf_text$V2 == "PATRI_BRUT_HORSRESTE_CORR*"
                & pdf_text$page_number == 116] <- "PATRI_BRUT_HORSRESTE_CORR"
    
    pdf_text$V2[pdf_text$V2 == "PATRI_NET_CORR*"
                & pdf_text$page_number == 117] <- "PATRI_NET_CORR"
    
    pdf_text$V2[pdf_text$V2 == "PATRI_NET_HORSRESTE_CORR*"
                & pdf_text$page_number == 117] <- "PATRI_NET_HORSRESTE_CORR"

    lonedots <- c()
    lonedots_indices <- grep("^\\.$", pdf_text$V2)
    for (i in lonedots_indices) {
      if (substr(pdf_text$V2[i - 1], nchar(pdf_text$V2[i - 1]),
                 nchar(pdf_text$V2[i - 1])) == ".") {
        lonedots <- c(lonedots, i)
      } else {
        pdf_text$V2[i - 1] <- paste0(pdf_text$V2[i - 1], pdf_text$V2[i])
        lonedots <- c(lonedots, i)
      }
    }
    pdf_text <- pdf_text[-lonedots, ]
  }

  row.names(pdf_text) <- NULL
  
  # Output: 
  return(pdf_text)
}

# [3] Defining -wide_scrape_data()-, a function that uses -pdftools::pdf_data()-
# to obtain a word-level dataframe containing information about fonts, size, co-
# ordinates, etc.
wide_scrape_data <- function(year) {
  
  #  [3.1] Fetching page limits.
  page_range <- page_limits[[paste0("wave_", year)]]
  begin_page <- page_range[1]
  end_page <- page_range[2]
  
  #  [3.2] Extracting text from PDF. 
  pdf_data <- pdf_data(pdf_names[[paste0("pdf_", year)]], font_info = TRUE)
  
  #  [3.3] Formatting and assigning pages.
  pdf_data <- bind_rows(lapply(seq_along(pdf_data), function(i) 
    mutate(pdf_data[[i]], page_number = i)))
  pdf_data <- pdf_data %>%
    filter(page_number >= begin_page & page_number <= end_page)
  pdf_data$font_size <- as.character(pdf_data$font_size)
  
  #  [3.4] Identifying the table associated with each variable. 
  if (year == 2004) {
    flag_dictionnaire <- which((grepl("Dictionnaire", pdf_data$text)) 
                               | (grepl("RISK", pdf_data$text) 
                                & pdf_data$font_size == 12.00 
                                & pdf_data$font_name == "Arial-BoldMT"))
  } else {
    flag_dictionnaire <- which(grepl("Dictionnaire", pdf_data$text))
  }
  
  pages_table <- pdf_data$page_number[flag_dictionnaire]
  flag_tables <- which(grepl("Table", pdf_data$text) 
                       & pdf_data$page_number %in% pages_table)
  
  if (year == 2010) {
    flag_tables <- flag_tables[!(pdf_data$text[flag_tables + 1] == "MS_RISQUE")]
  }
  
  tables_list <- list()
  for (i in flag_tables) {
    tables_list[[length(tables_list) + 1]] <- paste(pdf_data$text[i:(i+1)], collapse = " ")
  }
  
  pdf_data$table <- NA
  for (i in 1:(length(flag_tables) - 1)) {
    rows_to_fill <- (flag_tables[i] + 1):(flag_tables[i+1] - 1)
    pdf_data$table[rows_to_fill] <- tables_list[[i]]
  }
  
  rows_to_fill <- (flag_tables[length(flag_tables)] + 1):nrow(pdf_data)
  pdf_data$table[rows_to_fill] <- tables_list[[length(flag_tables)]]
  pdf_data$table <- gsub(".*Table ", "", pdf_data$table)
  pdf_data <- pdf_data %>%
    filter(!page_number %in% pages_table)
  
  #  [3.5 - YEAR-SPECIFIC] Removing unnecessary information between metadata.
  y_footnote <- y_footnotes[[paste0("wave_", year)]]
  footnotes <- which(pdf_data$y > y_footnote)
  pdf_data <- pdf_data[-footnotes, ]
  
  pattern_to_replace1 <- "(\\d+)à(\\d+)"
  pattern_replacement1 <- "\\1 à \\2"
  pdf_data$text <- gsub(pattern_to_replace1, pattern_replacement1, pdf_data$text)
  pattern_to_replace2 <- "(\\d+)à"
  pattern_replacement2 <- "\\1 à"
  pdf_data$text <- gsub(pattern_to_replace2, pattern_replacement2, pdf_data$text)
  pattern_to_replace3 <- "à(\\d+)"
  pattern_replacement3 <- "à \\1"
  pdf_data$text <- gsub(pattern_to_replace3, pattern_replacement3, pdf_data$text)
  
  index_0à <- which(pdf_data$text == "0 à")
  if (length(index_0à) > 0) {
    text_parts <- strsplit(pdf_data$text[index_0à], " ")[[1]]
    new_row1 <- pdf_data[index_0à, ]
    new_row1$text <- text_parts[1]
    new_row2 <- pdf_data[index_0à, ]
    new_row2$text <- text_parts[2]
    
    pdf_data <- rbind(pdf_data[1:(index_0à-1), ], new_row1, new_row2, pdf_data[(index_0à+1):nrow(pdf_data), ])
  }
  
  if (year == 2017) {
    pattern_to_replace4 <- "postesListe"
    pattern_replacement4 <- "postes Liste"
    pdf_data$text <- gsub(pattern_to_replace4, pattern_replacement4, pdf_data$text)
    
    index_poste <- which(pdf_data$text == "postes Liste")
    if (length(index_poste) > 0) {
      new_rows <- list()
      for (i in seq_along(index_poste)) {
        text_parts2 <- strsplit(pdf_data$text[index_poste[i]], " ")[[1]]
        new_row3 <- pdf_data[index_poste[i], ]
        new_row3$text <- text_parts2[1]
        new_row4 <- pdf_data[index_poste[i], ]
        new_row4$text <- text_parts2[2]
        new_rows[[length(new_rows) + 1]] <- new_row3
        new_rows[[length(new_rows) + 1]] <- new_row4
      }
      
      pdf_data <- pdf_data[-index_poste, ]
      pdf_data <- rbind(pdf_data, do.call(rbind, new_rows))
      rownames(pdf_data) <- NULL
      pdf_data <- pdf_data[order(row.names(pdf_data)), ]
    }
  
    indices_liste <- which(pdf_data$text == "Liste" & pdf_data$page_number == 204 & pdf_data$x == 229)
    indices_des <- which(pdf_data$text == "des" & pdf_data$page_number == 204 & pdf_data$x == 283)
    indices_codes <- which(pdf_data$text == "codes" & pdf_data$page_number == 204 & pdf_data$x == 302)
    
    pdf_data$y[indices_liste] <- pdf_data$y[indices_liste] + 2
    pdf_data$y[indices_des] <- pdf_data$y[indices_des] + 2
    pdf_data$y[indices_codes] <- pdf_data$y[indices_codes] + 2
    
    pdf_data$font_name[indices_liste] <- "BAAAAA+Arial-BoldMT"
    pdf_data$font_name[indices_des] <- "BAAAAA+Arial-BoldMT"
    pdf_data$font_name[indices_codes] <- "BAAAAA+Arial-BoldMT"

   pdf_data$x[pdf_data$text == "Liste"
              & pdf_data$page_number == 204
              & pdf_data$x == 229] <- 56

   pdf_data$x[pdf_data$text == "des"
              & pdf_data$page_number == 204
              & pdf_data$x == 283] <- 82

   pdf_data$x[pdf_data$text == "codes"
              & pdf_data$page_number == 204
              & pdf_data$x == 302] <- 102
   
   pdf_data$text[pdf_data$text == "=."
                 & pdf_data$page_number == 264] <- "="
  }
  
  
  if (year == 2004 | year == 1998) {
    variable_calculee1 <- which(pdf_data$text == "calculée" & pdf_data$x == x_calculee)
    variable_calculee2 <- variable_calculee1-1
    variable_calculee <- c(variable_calculee1, variable_calculee2)
    pdf_data <- pdf_data[-variable_calculee, ]
    
    if (year == 1998) {
      subtitle_menage <- which((pdf_data$text == "Table" & lead(pdf_data$text == "MENAGE")
                                & pdf_data$font_name == "HINGBK+Arial,Bold"))
      subtitle_menage_y <- pdf_data$y[subtitle_menage]
      subtitle_menage_page <- pdf_data$page_number[subtitle_menage]
      pdf_data <- pdf_data %>%
        filter(!(y %in% subtitle_menage_y & page_number %in% subtitle_menage_page))
      
      units <- which(pdf_data$font_name == "HINFMJ+Arial" 
                     & pdf_data$font_size == "10.02" 
                     & pdf_data$x > 465 
                     & (pdf_data$text == "En" 
                        | pdf_data$text == "francs" 
                        | pdf_data$text == "hectares"
                        | pdf_data$text == "m2"
                        | pdf_data$text == "pourcentage")
      )
      pdf_data <- pdf_data[-units, ]
      
    } else if (year == 2004) {
      numerique1 <- which(pdf_data$text == "numérique" & pdf_data$x == 368)
      numerique2 <- numerique1-1
      numerique <- c(numerique1, numerique2)
      pdf_data$y[numerique] <- pdf_data$y[numerique] + 2
    }
  }
  
  if (year == 2010) {
    pdf_data$text[pdf_data$text == "RMALDMTCD"
                  & pdf_data$page_number == 188] <- "RMALMTCD"
    pdf_data$text[pdf_data$text == "ZCGSCRDS"
                  & pdf_data$page_number == 150] <- "ZCSGCRDS"
  }

  if (year == 2017) {
    range_start <- 35819
    range_end <- 35883
    subset_to_order <- pdf_data[range_start:range_end, ]
    ordered_subset <- subset_to_order[order(subset_to_order$y, subset_to_order$x), ]
    pdf_data[range_start:range_end, ] <- ordered_subset
    row.names(pdf_data) <- NULL
    pdf_data <- pdf_data %>% mutate(obs_number = row_number())
    
    pdf_data$text[pdf_data$text == "LEE" 
                  & pdf_data$page_number == 223 
                  & pdf_data$font_size == "12"][2] <- "LEE_DECL"
    
    pdf_data$text[pdf_data$text == "PATRI_BRUT_CORR*"
                  & pdf_data$page_number == 116
                  & pdf_data$font_size == "12"] <- "PATRI_BRUT_CORR"
    
    pdf_data$text[pdf_data$text == "PATPROFHENT_CORR*"
                  & pdf_data$page_number == 113
                  & pdf_data$font_size == "12"] <- "PATPROFHENT_CORR"
    
    pdf_data$text[pdf_data$text == "PATRI_BRUT_HORSRESTE_CORR*"
                  & pdf_data$page_number == 116
                  & pdf_data$font_size == "12"] <- "PATRI_BRUT_HORSRESTE_CORR"
    
    pdf_data$text[pdf_data$text == "PATRI_NET_CORR*"
                  & pdf_data$page_number == 117
                  & pdf_data$font_size == "12"] <- "PATRI_NET_CORR"
    
    pdf_data$text[pdf_data$text == "PATRI_NET_HORSRESTE_CORR*"
                  & pdf_data$page_number == 117
                  & pdf_data$font_size == "12"] <- "PATRI_NET_HORSRESTE_CORR"
    
    pdf_data$font_size[pdf_data$text == "MTDONVC" 
                       & pdf_data$page_number == 326] <- "12"
    pdf_data$font_size[pdf_data$text == "VEND" 
                       & pdf_data$page_number == 272 
                       & pdf_data$font_name == "BAAAAA+Arial-BoldMT"] <- "12"
    
    categories_to_change <- which(pdf_data$font_size == 10 
                                  & pdf_data$text == 0 
                                  & pdf_data$x < 60 
                                  & pdf_data$table == "Table RETRAITE")
    
    for (index in categories_to_change) {
      if (pdf_data$text[index+1] == "Non") {
        pdf_data$y[index] <- pdf_data$y[index+1]
      }
    }
    
    lonedots <- c()
    lonedots_data_indices <- grep("^\\.$", pdf_data$text)
    
    for (i in lonedots_data_indices) {
      if (substr(pdf_data$text[i - 1], nchar(pdf_data$text[i - 1]), nchar(pdf_data$text[i - 1])) == ".") {
        lonedots <- c(lonedots, i)
      } else {
        pdf_data$text[i - 1] <- paste0(pdf_data$text[i - 1], pdf_data$text[i])
        lonedots <- c(lonedots, i)
      }
    }
    
    pdf_data <- pdf_data[-lonedots, ]
    
    rows_RANG_PANEL <- which(pdf_data$font_size == "5.8")
    if (length(rows_RANG_PANEL) > 0) {
      pdf_data$text[rows_RANG_PANEL - 1] <- paste0(   pdf_data$text[rows_RANG_PANEL - 1], 
                                                      pdf_data$text[rows_RANG_PANEL])
      pdf_data <- pdf_data[-rows_RANG_PANEL, ]
    }
    
    pdf_data <- pdf_data[order(pdf_data$page_number, pdf_data$y, pdf_data$x), ]
    pdf_data <- pdf_data %>%
      mutate(
        start_row = ifelse(grepl("codes", text) 
                           & font_size == "10" 
                           & font_name == "BAAAAA+Arial-BoldMT", row_number()+1, NA),
        end_row = ifelse(
          grepl("^[A-Z][-A-Z0-9_]*[A-Z0-9]*\\*?$", text) & 
            font_size == "12" & 
            font_name == "BAAAAA+Arial-BoldMT", 
          ifelse(text == "POND_RETRAITE", nrow(pdf_data), row_number()-1), 
          NA
        )
      ) %>%
      fill(start_row) %>%
      group_by(start_row) %>%
      mutate(
        end_row = ifelse(any(!is.na(end_row)), min(end_row, na.rm = TRUE), NA)
      ) %>%
      ungroup() %>%
      fill(start_row, .direction = "down") %>%
      fill(end_row, .direction = "down") %>%
      mutate(
        font_name = ifelse(row_number() >= start_row & row_number() <= end_row, "CATEGORIES", font_name),
        font_size = ifelse(row_number() >= start_row & row_number() <= end_row, "10", font_size)
      )
    
    repeat {
      pdf_data <- pdf_data %>%
        mutate(
          next_y = lead(y, default = last(y)),
          harmonize_y = ifelse((font_size == "10" | font_size == "11") & (y == as.numeric(next_y) + 1 | y == as.numeric(next_y) - 1), TRUE, FALSE),
          y = ifelse(harmonize_y, next_y, y)
        )
      
      if (!any(pdf_data$harmonize_y)) {
        break
      }
    }
  }
  
  #  [3.6 - YEAR-SPECIFIC] Re-ordering and numbering pdf_data.
  pdf_data <- order_pdf_data(pdf_data)
  
  # Output :
  return(pdf_data)
}

# [4] Defining -search_variables()-, a function that uses the pdf_data output obtained
# in function [3] to look for variables based on a specific pattern of font names
# and font size. 
search_variables <- function(pdf_data, year) {
  
  #  [4.1] Fetching the correct font name and font size.
  font_specs <- font_char[[paste0("wave_", year)]]
  cfont_name <- font_specs[1]
  cfont_size <- font_specs[2]
  pattern <- patterns[[paste0("wave_", year)]]
  
  #  [4.2] Obtaining the list of variable names. 
  variable_names <- pdf_data %>%
    filter(!is.na(table)) %>%
    filter(grepl(pattern, text) & font_size == cfont_size
           & font_name == cfont_name) %>%
    group_by(table) %>%
    mutate(
      name = text,
      obs_numbers = obs_number
    ) %>%
    select(table, name, obs_numbers)
  
  # Output : 
  return(variable_names)
}

# [5] Defining -merge_pdf()-, a function that merges the tokenized pdf_text output
# with the pdf_data one in order to get information on both the line and the metadata.
merge_pdf <- function(pdf_data, pdf_text, variable_names, year) {
  tables <- unique(variable_names$table)
  subset_list <- list()
  mismatched_subset_list <- list()
  
  for (table in tables) {
    cat("Processing table:", table, "\n")
    list_indices <- variable_names$obs_numbers[variable_names$table == table]
    list_variables <- variable_names$name[variable_names$table == table]
    
    for (i in seq_along(list_variables)) {
      start_indices <- which(pdf_text$id == list_variables[i] & pdf_text$table == table)
      
      if (i < length(list_variables)) {
        end_indices <- which(pdf_text$id == list_variables[i + 1] & pdf_text$table == table) - 1
      } else {
        row_end <- which(variable_names$name == list_variables[i] & variable_names$table == table)+1
        row_end <- variable_names$name[row_end]
        end_indices <- which(pdf_text$id == row_end) - 1
        if (is.na(row_end) & list_variables[i] == tail(list_variables, 1)) {
          end_indices <- nrow(pdf_text)
        }
      }
      
      if (i > 1) {
        previous_obs_index <- start_indices - 1
        previous_obs <- pdf_text$id[previous_obs_index]
        previous_observation_no_comma <- !grepl(",$", previous_obs)
        start_indices <- start_indices[previous_observation_no_comma]
      }
      
      for (j in seq_along(start_indices)) {
        start_index <- start_indices[j]
        if (length(end_indices) > 0) {
          end_index <- end_indices[which(end_indices > start_index)][1]
        } else {
          end_index <- end_indices[which(end_indices > start_index)][1]
        }
        
        if (!is.na(start_index) && !is.na(end_index) && start_index < end_index) {
          subset_text <- pdf_text[start_index:end_index, ]
          subset_text$obs_number <- 1:nrow(subset_text)
          
          index_start <- list_indices[i]
          if (i < length(variable_names)) {
            index_end <- list_indices[i+1]-1
          } else {
            row_end <- which(variable_names$obs_numbers == index_start)+1
            index_end <- variable_names$obs_numbers[row_end]-1
            if (row_end > nrow(variable_names) & list_variables[i] == tail(list_variables, 1)) {
              index_end <- nrow(pdf_data)
            }
          }
          subset_data <- pdf_data[index_start:index_end, ]
          
          if (year != 2017) {
            if (length(unique(subset_data$page)) == 1) {
              subset_data <- subset_data[order(subset_data$y, subset_data$x), ]
            } else {
              second_unique_value <- unique(subset_data$page)[2]
              subset_data$y[subset_data$page == second_unique_value] <- as.numeric(paste0(subset_data$page[subset_data$page == second_unique_value],
                                                                                          subset_data$y[subset_data$page == second_unique_value]))
              subset_data <- subset_data[order(subset_data$y, subset_data$x), ]
            }
          }
          
          subset_data$obs_number <- 1:nrow(subset_data)
          
          if (nrow(subset_data) != nrow(subset_text)) {
            cat("Warning: subset_data and subset_text have different numbers of rows for variable", list_variables[i], "\n")
            mismatched_subset_list[[length(mismatched_subset_list) + 1]] <- list(variable = list_variables[i], 
                                                                                 subset_data = subset_data, 
                                                                                 subset_text = subset_text)
          } else {
            subset_list[[length(subset_list) + 1]] <- list(variable = list_variables[i], 
                                                           subset_data = subset_data, 
                                                           subset_text = subset_text)
          }
        }
      }
    }
  }
  
  merged_subset_list <- list()
  for (i in seq_along(subset_list)) {
    current_element <- subset_list[[i]]
    merged_df <- merge(current_element$subset_text, 
                       current_element$subset_data, 
                       by.x = c("V2", "obs_number"), 
                       by.y = c("text", "obs_number"), 
                       all = TRUE, sort = FALSE)
    merged_subset_list[[i]] <- merged_df
  }
  
  add_first_id_column <- function(df) {
    df$first_id <- df$id[1]
    return(df)
  }
  merged_subset_list <- lapply(merged_subset_list, add_first_id_column)
  
  if (length(mismatched_subset_list) == 0) {
    message("No mismatched variables. We can proceed to the next step.")
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      x_coordinates <- x_categories[[paste0("wave_", year)]]
      lower_xco <- x_coordinates[1]
      upper_xco <- x_coordinates[2]
      
      df <- df %>%
        filter(font_name != "Arial-ItalicMT" & font_name != "HJBCNO+TimesNewRoman,Italic") %>%
        mutate(
          category = ifelse(grepl("\\s{2,}", id) & grepl("\\d+", V2) & x > lower_xco & x < upper_xco, row_number(), 0)
        ) 
      return(df)
    })
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      x_coordinates <- x_categories[[paste0("wave_", year)]]
      lower_xco <- x_coordinates[1]
      upper_xco <- x_coordinates[2]
      df <- df %>%
        group_by(id) %>%
        mutate(
          category = ifelse(category == 0 & any(category != 0), 
                            first(category[category != 0]), 
                            category)
        ) %>%
        ungroup() %>%
        mutate(
          category = ifelse(category == 0 
                            & lag(category) != 0 
                            & x > upper_xco 
                            & lag(x) > upper_xco, 
                            lag(category), 
                            category)
        ) %>%
        group_by(id) %>%
        mutate(
          category = ifelse(category == 0 
                            & any(category != 0), 
                            first(category[category != 0]), 
                            category)
        ) %>%
        ungroup()
      
      return(df)
    })
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      df <- df %>%
        mutate(
          line_break = (lag(font_size) == font_size) & 
            (lag(font_name) == font_name) & 
            ((abs(lag(y) - y) > 15) | (lag(x) < 450 & x < 100 & lag(x) > x)),
          group_var = consecutive_id(table.x, font_name, font_size, category, line_break)
        )
      return(df)
    })
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      df <- df %>%
        group_by(group_var) %>%
        mutate(concatenated_ids = paste(unique(id), collapse = " ")) %>%
        ungroup()
      
      return(df)
    })
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      x_coordinates <- x_categories[[paste0("wave_", year)]]
      lower_xco <- x_coordinates[1]
      upper_xco <- x_coordinates[2]
      df$format <- NA
      if (year == 2017) {
        if (any(grepl("^Liste des codes$|^Variable texte$", df$concatenated_ids))) {
          df$format <- "Categorical"
        } else if (any(grepl("^Variable numérique$", df$concatenated_ids))) {
          df$format <- "Numerical"
        } else {
          df$format <- "Not mentionned"
        }
      } else if (year == 2014) {
        if (any(grepl("^Caractère$", df$concatenated_ids))) {
          df$format <- "Categorical"
        } else if (any(grepl("^Numérique$", df$concatenated_ids))) {
          df$format <- "Numerical"
        } else {
          df$format <- "Not mentionned"
        }
      } else if (year == 2004) {
        if (any(grepl("^Variable numérique$", df$id))) {
          df$format <- "Numerical"
        } else {
          df$format <- "Categorical"
        }
      } else if (year == 2010 | year == 1998) {
        if (any(grepl("^à$", df$V2) & df$x > lower_xco & df$x < upper_xco)) {
          df$format <- "Numerical"
        } else {
          df$format <- "Categorical"
        }
      }
      
      return(df)
    })
    
    merged_subset_list <- lapply(merged_subset_list, function(df) {
      if (year == 2017) {
        format_2017_1 <- which(grepl("^Variable numérique$", df$concatenated_ids))
        format_2017_2 <- which(grepl("^Liste des codes$", df$concatenated_ids))
        format_2017_3 <- which(grepl("^Variable texte$", df$concatenated_ids))
        format_2017 <- c(format_2017_1, format_2017_2, format_2017_3)
        if (length(format_2017) > 0) {
          df <- df[-format_2017, ]
        }
      } else if (year == 2014) {
        format_2014_1 <- which(grepl("^Caractère$", df$concatenated_ids))
        format_2014_2 <- which(grepl("^Numérique$", df$concatenated_ids))
        format_2014 <- c(format_2014_1, format_2014_2)
        if (length(format_2014) > 0) {
          df <- df[-format_2014, ]
        }
      } else if (year == 2004) {
        format_2004_1 <- which(grepl("^Variable numérique$", df$id))
        format_2004_2 <- which(grepl("Variable numérique", df$concatenated_ids))
        if (length(format_2004_1) > 0) {
          df <- df[-format_2004_1, ]
          df$concatenated_ids <- gsub('Variable numérique ', '', df$concatenated_ids)
        }
      }
      
      return(df)
    })
    
    add_desc_column <- function(df) {
      font_desc <- font_char_desc[[paste0("wave_", year)]]
      desc_font_name <- font_desc[1]
      desc_font_size <- font_desc[2]
      any_match <- any(df$font_size == desc_font_size & df$font_name == desc_font_name)
      if (any_match) {
        df$desc <- unique(df$concatenated_ids)[2]
      } else {
        df$desc <- NA
      }
      return(df)
    }
    merged_subset_list <- lapply(merged_subset_list, add_desc_column)
    
    merged_df <- do.call(rbind, merged_subset_list)
    vars_to_remove <- c("table.y", "page")
    merged_df <- merged_df[, setdiff(names(merged_df), vars_to_remove)]
    
    # Output: 
    return(merged_df)
    
  } else {
    message("Some variables still need treatment. Returning mismatched variables.")
    return(mismatched_subset_list)
  }
}

# [6] Defining -categorize_pdf()-, a function to categorize variables by survey
# and provide category information.
categorize_pdf <- function(merged_df, year) {
  categorized_df <- merged_df %>%
    group_by(table.x, first_id) %>%
    mutate(below = (concatenated_ids != desc & concatenated_ids != first_id),
           keep_condition = ifelse(format == "Categorical" & all(!below), TRUE, format != "Categorical")) %>%
    slice(if (keep_condition[1]) 1 else which(below)) %>%
    distinct(concatenated_ids, .keep_all = TRUE) %>%
    ungroup() %>%
    select(table.x, first_id, concatenated_ids, desc, format) %>%
    mutate(category = ifelse(format == "Categorical",
                             str_trim(str_extract(concatenated_ids, "^(.*?)\\s{2,}")),
                             NA),
           label = ifelse(format == "Categorical",
                          str_trim(str_extract(concatenated_ids, "\\s{2,}(.*)$")),
                          NA)) %>%
    group_by(table.x, first_id) %>%
    filter(if (all(is.na(category) & is.na(label))) row_number() == 1 else TRUE) %>%
    filter(!(format == "Categorical" & is.na(category) & is.na(label) & n() > 1)) %>% # Additional filter for NA category or label
    ungroup() %>%
    select(-concatenated_ids) %>%
    select(table.x, first_id, desc, category, label, format)
  
  treat_xx_variables <- function(df) {
    if (year == 2017) {
      xx_variables <- which((grepl("XX$", df$first_id) | grepl("X$", df$first_id) | grepl("XX_DECL$", df$first_id)) & grepl("\\(.*?[xX] (?:=|de) (\\d+).*?\\)", df$desc))
      df$interval <- NA
      for (i in xx_variables) {
        interval <- str_extract(df$desc[i], "\\((.*?\\d+)\\)")  # Extract text within parentheses
        if (!is.na(interval)) {
          df$interval[i] <- interval
        }
      }
      
      xxid <- with(
        df,
        lapply(
          regmatches(interval, gregexpr("\\d+", interval)),
          \(v) do.call(seq, as.list(as.numeric(v)))
        )
      )
      
      df <- transform(
        `row.names<-`(df[rep(seq.int(nrow(df)), lengths(xxid)), ], NULL),
        first_id = ifelse(is.na(interval),
                          first_id,
                          ifelse(grepl("XX", first_id),
                                 ifelse(grepl("XX_DECL", first_id),
                                        paste0(sub("XX_DECL", "", first_id), sprintf("%i_DECL", unlist(xxid))),
                                        paste0(sub("XX", "", first_id), sprintf("%i", unlist(xxid)))),
                                 first_id)
        )
      )
      
      df <- df[order(df$table.x, df$first_id), ]
      df <- subset(df, select = -interval)
      
      return(df)
      
      
    } else if (year == 2014) {
      
      xx_variables <- which((grepl("XX$", df$first_id) | grepl("X$", df$first_id)) & grepl("\\(.*?[xX] (?:=|de) (\\d+).*?\\)", df$desc))
      df$interval <- NA
      for (i in xx_variables) {
        interval <- str_extract(df$desc[i], "\\((.*?\\d+)\\)")  # Extract text within parentheses
        if (!is.na(interval)) {
          df$interval[i] <- interval
        }
      }
      
      xxid <- with(
        df,
        lapply(
          regmatches(interval, gregexpr("\\d+", interval)),
          \(v) do.call(seq, as.list(as.numeric(v)))
        )
      )
      
      df <- transform(
        `row.names<-`(df[rep(seq.int(nrow(df)), lengths(xxid)), ], NULL),
        first_id = ifelse(is.na(interval),
                          first_id,
                          ifelse(grepl("XX$", first_id),
                                 paste0(sub("XX$", "", first_id), sprintf("%i", unlist(xxid))),
                                 paste0(sub("X$", "", first_id), sprintf("%i", unlist(xxid))))
        )
      )
      
      df <- df[order(df$table.x,df$first_id), ]
      df <- subset(df, select = -interval)
      
      return(df)
      
      
    } else if (year == 2010) {
      xx_variables <- which(grepl("xx", df$first_id))
      df$interval <- NA
      for (i in xx_variables) {
        interval <- str_extract(df$desc[i], "\\((.*?)\\)")  # Extract text within parentheses
        if (!is.na(interval)) {
          df$interval[i] <- interval
        }
      }
      
      xxid <- with(
        df,
        lapply(
          regmatches(interval, gregexpr("\\d+", interval)),
          \(v) do.call(seq, as.list(as.numeric(v)))
        )
      )
      
      df <- transform(
        `row.names<-`(df[rep(seq.int(nrow(df)), lengths(xxid)), ], NULL),
        first_id = ifelse(is.na(interval), first_id, paste0(sub("xx", "", first_id), sprintf("%i", unlist(xxid))))
      )
      
      df <- df[order(df$table.x,df$first_id), ]
      df <- subset(df, select = -interval)
      
      return(df)
    }
  }
  
  if (year == 2010 | year == 2014 | year == 2017) {
  categorized_df <- treat_xx_variables(categorized_df)
  }
  
  categorized_df$category <- as.numeric(categorized_df$category)
  
  # Output : 
  return(categorized_df)
}

# [7] Defining -wide_metadata()-, a function embedding all the other to create 
# the final dataset, combined_categorized_df. 
wide_metadata <- function(...) {
  years <- list(...)
  invalid_years <- years[!years %in% as.numeric(waves)]
  
  if (length(invalid_years) > 0) {
    unsupported_years_message <- paste(invalid_years, collapse = ", ")
    if (length(invalid_years) == 1) {
      stop(paste("Error: Year", unsupported_years_message, "is not supported by the function. Please use one of these years:", 
                 paste(supported_years, collapse = ", ")))
    } else {
      stop(paste("Error: Years", unsupported_years_message, "are not supported by the function. Please use one of these years:", 
                 paste(supported_years, collapse = ", ")))
    }
  }
  
  categorized_dfs <- list()
  for (year in years) {
    pdf_data <- wide_scrape_data(year)
    pdf_text <- wide_scrape_text(year)
    variables <- search_variables(pdf_data, year)
    merged_pdf <- merge_pdf(pdf_data, pdf_text, variables, year)
    categorized_df <- categorize_pdf(merged_pdf, year)
    
    categorized_df$year <- year
    categorized_dfs[[as.character(year)]] <- categorized_df
  }
  
  combined_categorized_df <- do.call(rbind, categorized_dfs)
  rownames(combined_categorized_df) <- NULL
  
  # Final output: 
  return(combined_categorized_df)
}

# [8 - OPTIONAL] Knowing how many variables in total : 
count_variables <- function(dataframe) {
  unique_combinations <- dataframe %>%
    distinct(dataframe$year, dataframe$table.x, dataframe$first_id)
  
  number_of_unique_combinations <- nrow(unique_combinations)
  
  return(number_of_unique_combinations)
}

convert_to_lowercase <- function(dataframe, column_name) {
  dataframe[[column_name]] <- tolower(dataframe[[column_name]])
  return(dataframe)
}

# [9 - OPTIONAL] Finding years for variable : 
find_years <- function(data, variable_name) {
  filtered_data <- data %>%
    filter(first_id == variable_name)
  
  categories_per_year <- filtered_data %>%
    group_by(year) %>%
    summarise(num_categories = n())
  
  unique_years <- categories_per_year$year
  num_categories <- categories_per_year$num_categories
  
  if(length(unique_years) > 1) {
    changes <- sapply(2:length(unique_years), function(i) {
      paste("from", num_categories[i-1], "to", num_categories[i], "categories")
    })
    change_messages <- paste0(unique_years[-1], ": ", changes)
    
    message <- paste("Variable", variable_name, "appears in years", paste(unique_years, collapse = ", "), ".",
                     "There are changes in the number of categories between years as follows:", 
                     paste(change_messages, collapse = "; "), ".")
  } else {
    message <- paste("Variable", variable_name, "does not appear in more than one year.")
  }
  
  return(message)
}


# V. MAIN SCRIPT ---------------------------------------------------------------
metadata <- wide_metadata(1998, 2004, 2010, 2014, 2017)
write.csv(metadata, "D:/WIDE/Harmonization/metadata.csv", row.names = FALSE)