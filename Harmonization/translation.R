library(polyglotr)
library(shiny)

metadata <- read.csv("D:/WIDE/Harmonization/metadata.csv")

list_of_variable <- metadata %>%
  distinct(year, table.x, first_id, .keep_all = TRUE) %>%
  select(first_id, year, table.x, desc)

# Translate the description
list_of_variable <- list_of_variable %>%
  mutate(desc_en = google_translate(desc, target = "en"))

list_of_variable$desc_en <- as.character(list_of_variable$desc_en)

write.csv(list_of_variable, "D:/WIDE/Harmonization/list_of_variable.csv", row.names = FALSE)

# Define UI for the Shiny app (use a different name for the UI function)
ui_shiny_app <- fluidPage(
  titlePanel("Variable Description Translator"),
  sidebarLayout(
    sidebarPanel(
      textInput("variable_name", "Enter Variable Name:", "")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

# Define server logic for the Shiny app (use a different name for the server function)
server_shiny_app <- function(input, output) {
  data <- read.csv("D:/WIDE/Harmonization/list_of_variable.csv", stringsAsFactors = FALSE)
  
  output$results <- renderTable({
    req(input$variable_name)
    filtered_data <- data %>%
      filter(first_id == input$variable_name)
    filtered_data
  })
}

# Run the Shiny app (runApp needs to be adjusted if already defined)
shinyApp(ui = ui_shiny_app, server = server_shiny_app)