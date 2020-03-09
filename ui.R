library(shiny)

BRCA <- read.csv('BRCA_subset_median.csv')
surv_object <- Surv(time = BRCA$DSS.time, event = BRCA$DSS)

proteins <- as.vector(colnames(BRCA[4:6]))

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.min.css",
                  
                  pageWithSidebar(
                      
                      # Application title.
                      headerPanel("Breast Cancer Survival Analysis"),
                      
                      sidebarPanel(
                          selectInput(inputId = "Protein_ID",
                                      'Select Protein ID',
                                      names(BRCA)[names(BRCA) != "patient" & names(BRCA) != "DSS" & names(BRCA) != "DSS.time"]
                          )
                      ), 
                      
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Survival Plot", plotOutput("plot_survival"), h2(textOutput("caption"))),
                              tabPanel("Data Summary", verbatimTextOutput("summary")),
                              tabPanel("Data", tableOutput("raw_data")),
                              id = "tabs"
                          ) 
                      )
                  )))
