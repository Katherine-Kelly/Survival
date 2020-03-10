library(shiny)

BRCA <- read.csv('BRCA_survival_median.csv')
surv_object <- Surv(time = BRCA$DSS.time, event = BRCA$DSS)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.min.css",
                  
                  pageWithSidebar(
                      
                      # Application title.
                      headerPanel("Breast Cancer Survival Analysis"),
                      
                      sidebarPanel(
                          selectInput(inputId = "Protein_ID",
                                      'Select Protein ID',
                                      names(BRCA[16:236])
                          ),
                          selectInput(inputId = "subtype",
                                      'Select cancer subtype (optional)',
                                      na.omit(BRCA$subtype))
                      ), 
                      
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Survival Plot", plotOutput("plot_survival"), h2(textOutput("caption"), h4(textOutput("info")), h4(textOutput("moreinfo")))),
                              tabPanel("Data Summary", verbatimTextOutput("summary")),
                              tabPanel("Data", tableOutput("raw_data")),
                              id = "tabs"
                          ) 
                      )
                  )))

