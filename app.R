library(shiny)

BRCA <- read.csv('BRCA_survival_median.csv')
# Define UI
ui <- fluidPage(theme = "bootstrap.min.css",
    
    titlePanel("Breast Cancer Survival Analysis"),
    
    sidebarLayout(
        
        sidebarPanel(
           # selectInput(inputId = "Protein_ID",
            #            'Select Protein ID',
             #           names(BRCA[16:236]), selected="SMAD3"
            #),
            #just testing this out
            textInput(inputId = "Protein_ID",
                      'Enter Protein ID'),
            

            fileInput("file1", "Upload CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("My Data", tableOutput("contents")),
                tabPanel("My Survival Plot", plotOutput('survplot'), h2(textOutput("caption"))),
                tabPanel("Help", h4(textOutput('help')), tableOutput("sampletable"))
            )
            
            
        )
        
    )
)

# Define server logic
server <- function(input, output) {
    
    myData <- reactive ({
        req(input$file1)
        inFile <- input$file1
            df <- read.csv(inFile$datapath)
        df
    })
    
    runSur <- reactive({
        req(input$Protein_ID)
        survfit(as.formula(paste("Surv(DSS.time,DSS) ~ ",paste(input$Protein_ID))),data=myData())
        
    })
    
    output$contents <- renderTable({
        myData()
        
    })
    
    output$survplot <- renderPlot ({
        plot(runSur(), col=c('red', 'blue'), xlab='Time (weeks)', ylab='Probability of Survival', lwd=2) 
        legend(7000,1, legend=c("High expression", "Low expression"), col=c("red", "blue"),lty=1, lwd=4)
    
        
    })
    
    output$caption <- renderText({
        req(input$Protein_ID)
        paste("Kaplan Meier Survival Graph of", input$Protein_ID, sep="\n")
    })
    output$help <- renderText({
        paste("How do I upload a file?",
              "Files should be uploaded in .CSV format with 
              genes/conditions/outcomes represented by columns, 
              and individuals/patients represented by rows, as in the sample table below:",
              "How do I interpret my plot?", sep="\n")
    })
    output$sampletable <- renderTable({
        sample <- read.csv("BRCA_subset_median.csv")
        head(sample)
    })
    

} 

shinyApp(ui, server)
