# Load necessary libraries
library(dplyr)
library(stringdist)
library(tokenizers)

# Step 1: Load your data
metadata <- read.csv("/Users/yanis/Desktop/WIDE-R/Harmonization/metadata.csv")

# Step 2: Preprocess the Data
metadata_clean <- metadata %>%
  mutate(
    desc = tolower(desc),                   # Convert descriptions to lowercase
    desc = gsub("[[:punct:]]", "", desc),   # Remove punctuation
    desc = trimws(desc),                    # Trim whitespace
    category = as.character(category)       # Convert category to character for comparison
  )

# Step 3: Remove Stop Words
# List of French stop words (you can expand this list)
stop_words <- c("pour", "et", "de", "la", "le", "les", "des", "du", "l'", "d'")
metadata_clean$desc_clean <- sapply(metadata_clean$desc, function(desc) {
  words <- unlist(strsplit(desc, " "))
  words <- words[!words %in% stop_words]
  paste(words, collapse = " ")
})

# Step 4: Ensure One first_id Per Year Before Distance Calculation
metadata_unique <- metadata_clean %>%
  group_by(year, desc_clean) %>%
  filter(!duplicated(first_id)) %>%
  ungroup()

# Step 5: Calculate String Distance with Cosine Similarity
desc_dist <- stringdistmatrix(metadata_unique$desc_clean, metadata_unique$desc_clean, method = "cosine")

# Step 6: Apply Hierarchical Clustering with a Stricter Threshold
threshold <- 0.1  # Adjust this threshold as needed
groupings <- hclust(as.dist(desc_dist)) %>%
  cutree(h = threshold)

# Step 7: Add Groupings to the Data
metadata_unique <- metadata_unique %>%
  mutate(desc_group = groupings)

# Step 8: Summarize Grouped Data
final_groups <- metadata_unique %>%
  group_by(desc_group) %>%
  summarise(
    first_ids = paste(unique(first_id), collapse = ", "),
    desc_variations = paste(unique(desc), collapse = " | "),
    category_variations = paste(unique(category), collapse = ", "),
    label_variations = paste(unique(label), collapse = " | "),
    year_range = paste(min(year), max(year), sep = "-"),
    n_years = n_distinct(year)  # Count distinct years
  ) %>%
  filter(n_years == length(unique(metadata$year)))  # Ensure all years are represented in the group

# Print the final grouped metadata
print(final_groups)

