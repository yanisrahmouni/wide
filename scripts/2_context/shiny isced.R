library(shiny)
library(dplyr)

# Load the data
data <- read.csv("/Users/yanis/Desktop/WIDE-R/Harmonization/degree_metadata.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Mapping Table by Year and Variable"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = unique(data$year)),
      selectInput("variable", "Select Variable", choices = NULL)
    ),
    
    mainPanel(
      tableOutput("mappingTable")
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  
  # Update the variable selection based on the selected year
  observe({
    filtered_data <- data %>% filter(year == input$year)
    updateSelectInput(session, "variable", choices = unique(filtered_data$first_id))
  })
  
  # Generate the mapping table
  output$mappingTable <- renderTable({
    req(input$year, input$variable)
    
    # Filter the data based on the year and variable
    filtered_data <- data %>%
      filter(year == input$year, first_id == input$variable) %>%
      select(category, label, ISCED_number, ISCED_label)
    
    # Return the filtered data as a table
    filtered_data
  })
}

# Run the Shiny App
shinyApp(ui, server)