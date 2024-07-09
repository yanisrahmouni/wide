# DESCRIPTION ------------------------------------------------------------------
# This R script creates a function called make_path that standardizes the process
# of unzipping files and organizing data for researchers. The primary objective 
# is to avoid inconsistencies in file paths across different users, ensuring a 
# uniform structure. The only requirement is that all zip files must be downloaded
# into a single main folder.

# I. PACKAGES ------------------------------------------------------------------
if(!require("utils")) install.packages("utils", dependencies=TRUE)
if(!require("tools")) install.packages("tools", dependencies=TRUE)

# Define the function
make_path <- function(main_folder) {
  # Dictionary mapping numbers to years
  number_to_year <- list(
    "0124" = "1986",
    "0125" = "1992",
    "0079" = "1998",
    "0250" = "2004",
    "0686" = "2010",
    "1150" = "2014",
    "1303" = "2014R",
    "1418" = "2017",
    "1625" = "2020"
  )
  
  # Get a list of all zip files in the main folder
  zip_files <- list.files(main_folder, pattern = "\\.zip$", full.names = TRUE)
  
  # Iterate over each zip file
  for (zip_file in zip_files) {
    # Extract the number from the file name
    file_name <- basename(zip_file)
    file_name <- iconv(file_name, from = "UTF-8", to = "ASCII//TRANSLIT")
    file_number <- sub("lil-(\\d+).*", "\\1", file_name)
    
    # Check if the number is in the dictionary
    if (file_number %in% names(number_to_year)) {
      # Get the corresponding year
      year <- number_to_year[[file_number]]
      year_folder <- file.path(main_folder, year)
      
      # Create the year folder if it doesn't exist
      if (!dir.exists(year_folder)) {
        dir.create(year_folder)
      }
      
      # Move the zip file to the year folder
      file.rename(zip_file, file.path(year_folder, file_name))
      
      # Unzip the file in the year folder
      unzip(file.path(year_folder, file_name), exdir = year_folder)
      
      # Determine the type of file (.csv or .dta)
      if (grepl("\\.csv\\.zip$", file_name)) {
        subfolder <- "Csv"
      } else if (grepl("\\.dta\\.zip$", file_name)) {
        subfolder <- "Dta"
      } else {
        next  # Skip if the file type is unknown
      }
      
      # Define paths for Doc and subfolder
      doc_folder <- file.path(year_folder, "Doc")
      data_folder <- file.path(year_folder, subfolder)
      
      # Create subfolders if they don't exist
      if (!dir.exists(doc_folder)) {
        dir.create(doc_folder)
      }
      if (!dir.exists(data_folder)) {
        dir.create(data_folder)
      }
      
      # Move contents to the appropriate subfolders
      extracted_files <- list.files(year_folder, full.names = TRUE)
      for (extracted_file in extracted_files) {
        if (basename(extracted_file) == "Doc") {
          # Move Doc contents
          file.copy(list.files(extracted_file, full.names = TRUE), doc_folder, overwrite = TRUE)
          unlink(extracted_file, recursive = TRUE)
        } else if (grepl(paste0("\\.", tolower(subfolder), "$"), extracted_file)) {
          # Move Csv or Dta contents
          file.rename(extracted_file, file.path(data_folder, basename(extracted_file)))
        }
      }
    }
  }
}

# Example usage
# Make sure to replace 'your/main/folder/path' with the actual path to your main folder
# make_path("your/main/folder/path")


test <- make_path("D:/Archive WIDE-R - Inutilisé/Archive données/Quetelet/quetelet_archive")