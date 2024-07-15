# DESCRIPTION ------------------------------------------------------------------
# This R script creates a function called make_path that standardizes the process
# of unzipping files and organizing data for researchers. The primary objective 
# is to avoid inconsistencies in file paths across different users, ensuring a 
# uniform structure. The only requirement is that all zip files must be downloaded
# into a single main folder.

# I. PACKAGES ------------------------------------------------------------------
if(!require("utils")) install.packages("utils", dependencies=TRUE)
if(!require("tools")) install.packages("tools", dependencies=TRUE)
if(!require("zip")) install.packages("zip", dependencies=TRUE)

Sys.setenv(LANG = "en")

# Define the function
make_path <- function(main_folder, meth_2010 = "2010") {
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
      zip::unzip(file.path(year_folder, file_name), overwrite = TRUE, exdir = year_folder)
      
      # Special handling for the year 2010
      if (year == "2010") {
        stata_folder <- file.path(year_folder, "Stata")
        R_folder <- file.path(year_folder, "Csv")
        meth_2004_folder_stata <- file.path(stata_folder, "Methodologie04")
        meth_2010_folder_stata <- file.path(stata_folder, "Methodologie10")
        meth_2004_folder_R <- file.path(R_folder, "Methodologie04")
        meth_2010_folder_R <- file.path(R_folder, "Methodologie10")
        
        # Function to move contents and remove the folder
        move_contents_and_remove <- function(source_folder, target_folder) {
          files <- list.files(source_folder, full.names = TRUE)
          file.rename(files, file.path(target_folder, basename(files)))
          unlink(source_folder, recursive = TRUE)
        }
        
        # Move the contents and remove the folder accordingly for Stata
        if (meth_2010 == "2004" && dir.exists(meth_2010_folder_stata)) {
          move_contents_and_remove(meth_2010_folder_stata, stata_folder)
          move_contents_and_remove(meth_2004_folder_stata, stata_folder)
        } else if (meth_2010 == "2010" && dir.exists(meth_2004_folder_stata)) {
          move_contents_and_remove(meth_2004_folder_stata, stata_folder)
          move_contents_and_remove(meth_2010_folder_stata, stata_folder)
        }
        
        # Move the contents and remove the folder accordingly for R
        if (meth_2010 == "2004" && dir.exists(meth_2010_folder_R)) {
          move_contents_and_remove(meth_2010_folder_R, R_folder)
          move_contents_and_remove(meth_2004_folder_R, R_folder)
        } else if (meth_2010 == "2010" && dir.exists(meth_2004_folder_R)) {
          move_contents_and_remove(meth_2004_folder_R, R_folder)
          move_contents_and_remove(meth_2010_folder_R, R_folder)
        }
      }
    }
  }
}

# Example usage
# Make sure to replace 'your/main/folder/path' with the actual path to your main folder
# make_path("your/main/folder/path", meth_2010 = "2004")

test <- make_path("D:/Archive WIDE-R - Inutilisé/Archive données/Quetelet/quetelet_archive")
