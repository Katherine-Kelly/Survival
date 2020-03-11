library(shiny)

BRCA <- read.csv('BRCA_survival_median.csv')
surv_object <- Surv(time = BRCA$DSS.time, event = BRCA$DSS)


# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.min.css",
             navbarPage("Breast Cancer Survival Analysis",
                        tabPanel("Plot",
                                 pageWithSidebar(
                                   
                                   # Application title.
                                   headerPanel(""),
                                   
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
                                       tabPanel("KM Survival Plot", plotOutput("plot_survival"), h2(textOutput("caption"), h4(textOutput("moreinfo")))),
                                       tabPanel("COXPH", plotOutput("coxph"), h2(textOutput("coxph_caption"))),
                                       tabPanel("Summary Statistics", fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),plotOutput("subtypesummary"), plotOutput("agesummary"), plotOutput("gradesummary"))),
                                       id = "tabs"
                                                
                                       ) 
                                     )))
                        ))
))
