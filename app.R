library(shiny)

# Define UI
ui <- fluidPage(
    
    titlePanel("Breast Cancer Survival Analysis"),
    
    sidebarLayout(
        
        sidebarPanel(

            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
        ),
        
        mainPanel(
            tableOutput("contents"),
            plotOutput('survplot')
            
        )
        
    )
)

# Define server logic
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        req(input$file1)
        df <- read.csv(input$file1$datapath)
            return(df)
        
    })
    output$survplot <- renderPlot ({
        req(input$file1)
        df <- read.csv(input$file1$datapath)
        surv_object <- Surv(df$DSS.time, df$DSS)

        fit <- survfit(surv_object ~ X4EBP1, data = df)
        ggsurvplot(fit, data = df, pval = TRUE)
        #hist(df[,3])
    })

} 

shinyApp(ui, server)
