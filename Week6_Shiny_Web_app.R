
# Load dependencies
library(shiny) # Web Application Framework for R
library(shinydashboard) 
library(ggplot2)
library(magrittr)
library(dplyr)
library(dashboardthemes)
library(knitr)

library(palmerpenguins) 
library(rsconnect)
library(kableExtra)

#rsconnect::setAccountInfo(name='',
#                          token='',
#                          secret='')

#rsconnect::deployApp("C:/Users/SURFACE PRO/Desktop/Palmer Penguins")


#forgetDeployment(appPath = getwd())

data <- penguins_raw

# Define UI ---------------------------------------------------------------
# Define UI for application that draws a scatter plot

ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Palmer Penguins"),
  # Sidebar with check box
  dashboardSidebar(
    checkboxGroupInput(
      "penguinType",
      h4("Select a penguin species"),
      choices = list(
        "Adelie" = "Adelie",
        "Chinstrap" = "Chinstrap",
        "Gentoo" = "Gentoo"
      ),
      selected = "Adelie"
    )
  ),
  # Show a plot
  dashboardBody(
  box(
    solidHeader = TRUE,
    fluidRow(plotOutput("penguins",height = 250))),
  box(
    solidHeader = TRUE,
    fluidRow(plotOutput("penguins_boxplot",height = 250))),
  box(tableOutput("penguins_table")), 
  box(tableOutput("penguins_islands")),
  
           
  ### changing theme
  shinyDashboardThemes(
  theme = "grey_light"),
    ) #dashboard body
    
)

# Define server  ----------------------------------------------------------

# Define server logic to draw a scatter plot
server <- function(input, output) {
  
  output$penguins <- renderPlot({
    
    # Prepare the data and add colors
    penguins %<>%
      dplyr::filter(species %in% input$penguinType)
    
    # Use ggplot2 to generate a scatter plot where the flipper length is 
    # plotted against weight
    penguins %>%
      ggplot(aes(
        x = bill_depth_mm,
        y = bill_length_mm,
        color = species)) +
      geom_jitter() +
      labs(title ="Bill Depth & Length for penguin species",
           x = "bill depth",
           y = "bill length")
    
  })
  
   #try to output another visualization, boxplot
  output$penguins_boxplot <- renderPlot({
    # Prepare the data and add colors
    penguins %<>%
      dplyr::filter(species %in% input$penguinType)
    
    penguins %>%
      ggplot(aes(x = body_mass_g, color = species)) + 
      geom_boxplot() +
      coord_flip()+
      labs(title = "Box plot for Body mass",
           x = "body mass")
    
    })
  

   output$penguins_table <- function(){
     penguins_raw %>%
       count(Species) %>%
       kbl() %>%
       kable_minimal()
     
     
   }
   
   output$penguins_islands <- function(){penguines1 %>%
       count(Island, Species)%>%
       kbl()%>%
       kable_paper("hover", full_width = F)}
  
  
}


# Bring everything together -----------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
