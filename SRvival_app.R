library(shiny)
library(survival)
library(survminer)

BRCA <- read.csv('BRCA_survival_median.csv')

ui <- fluidPage(
    
    # Input() and Output() object IDs from UI will be called in Server as input$*** and output$***
    
    navbarPage(
        "SRvival: A Survival Analysis Toolbox",
        tabPanel("TCGA Data",
                 sidebarLayout(
                     
                     sidebarPanel(width = 3,
                         selectInput(inputId = "Protein_ID",
                                     'Select Protein ID',
                                     names(BRCA[16:236])
                         ), 
                         selectInput(inputId = "endpoint",
                                     'Select Survival Endpoint',
                                     c("Overall Survival (OS)"="OS", "Disease Specific Survival (DSS)"="DSS", "Disease-Free Interval (DFI)"="DFI", "Progression-Free Interval (PFI)"="PFI")
                         ), 
                         selectInput(inputId = "subtype",
                                     'Select Subtype',
                                     c("All"="All", "Basal"="Basal", "LumB"="LumB", "LumA"="LumA", "Her2"="Her2", "Normal"="Normal"), selected = "All")
                     ), 
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("KM Survival Plot", plotOutput("plot_survival")),
                             tabPanel("Model Summary", verbatimTextOutput("diff"), tags$head(tags$style("#diff{color:darkblue; font-size:12px; background: ghostwhite;}")))
                             
                              
                         )))),
        tabPanel("Summary Statistics",            
                 tabsetPanel(
                     tabPanel("Survival Summary", fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("subtypeKM"), plotOutput("stageKM")))),
                     tabPanel("COX Proportional Hazard Models", fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"), plotOutput("ageCox"), plotOutput("subtypeCox"), plotOutput("stageCox")))),
                              tabPanel("Distribution of Data", fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),plotOutput("subtypesummary"), plotOutput("agesummary"), plotOutput("gradesummary"))), fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("stacked1"), plotOutput("stacked2"))),
                     )
                 )
                 
        ),
        tabPanel("Custom Data",
                 sidebarLayout(   
                     sidebarPanel(width = 3,
                         textInput(inputId = "custom_variable",
                                   'Enter Variable Name'),
   
                         fileInput("file1", "Upload CSV File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("My Data", tableOutput("contents")),
                             tabPanel("My Survival Plot", plotOutput('plot_survival2')),
                             tabPanel("My Model Summary", verbatimTextOutput("diff2"), tags$head(tags$style("#diff2{color:darkblue; font-size:12px; background: ghostwhite;}")))
                         )
                    )
                 )),
        
        # Page of text describing the analysis, interpretation and usage
        tabPanel("Help", 
                 headerPanel(""),
                 mainPanel(
                     p(strong("Overview")),
                     br(),
                     p("SRvival is a user-friendly survival analysis toolbox created using the R Shiny platform which allows customised evaluation of prognostic markers. Users can investigate associations between protein expression and patient survival in breast cancer and its molecular subtypes, and can also upload their own datasets for analysis. SRvival allows separate analysis across four survival endpoints - overall survival, disease-free survival, progression-free interval and disease-free interval. Data can be stratified into LuminalA, LuminalB, HER2+, Basal and Normal-like subtypes if desired."), 
                     p("SRvival plots are based on data from the publically available", strong("TCGA Breast dataset"), ", which contains survival and protein expression information for", strong("221 proteins"), "in", strong("874 patients."),"Protein expression was quantified using reverse phase protein arrays. Data is stratified into high and low expression groups based on median expression for each protein. Kaplan Meier survival models and log rank p-values are calculated using the R package,", em("survival.")),
                     br(),
                     p(strong("How do I upload a file?")),
                     p("Files in .csv format can be uploaded in the", strong("Custom Data"), "section. Each predictor variable (the genes or conditions of interest) should be represented in a column, and each individual represented by a row. Time and endpoint columns should be so named, as in the sample table below:"),
                     br(),
                     tableOutput("sampletable"),
                     br(),
                     p("You can browse your data table under the", strong("My Data"), "tab. Entering a variable name in the selection box will automatically generate a Kaplan Meier plot based on your sample data. Check for a difference in your survival curves under the", strong("My Summary"), "tab, where you will find the chi-squared statistic and log-rank p-value for your survival model."),
                     br(),
                     p(strong("How do I interpret the survival plot?"),
                     br(),
                     p("The Kaplan Meier method describes the probability of survival at a specified timepoint. At time = 0, the probability of survival is 1, and decreases as time progresses. A steeper slope of the survival function corresponds to a poorer prognosis. The effect of gene expression (or any other predictor variable) on patient prognosis can be evaluated by visualising the differences in survival curves for high (red) and low (blue)  expression. The log rank p-value for difference in survival curves can be found under the", strong("Model Summary"), "tab. A log rank p-value < 0.05 indicates that the protein of interest is significantly associated with patient survival in the selected cohort."),
                     )
                 ),
                 )
    )
)    
    
          #########################
          ##   Start of Server   ##
          #########################


server <- function(input, output) {
    
    # Run survival function depending on user selected protein, subtype and endpoint
    runSurSubtype <- reactive({
        if (input$endpoint=="OS") {
            object <- as.formula(paste("Surv(OS.time,OS) ~" , paste(input$Protein_ID)))
        } else if (input$endpoint=="DSS") {
            object <- as.formula(paste("Surv(DSS.time,DSS) ~" , paste(input$Protein_ID)))   
        } else if (input$endpoint=="PFI") {
            object <- as.formula(paste("Surv(PFI.time,PFI) ~" , paste(input$Protein_ID)))   
        } else {
            object <- as.formula(paste("Surv(DFI.time,DFI) ~" , paste(input$Protein_ID))) 
        }
        return(object)    
    })
    
    # Plot Kaplan Meier survival function
    output$plot_survival <- renderPlot({
        subtype <- subset(BRCA, subtype==paste(input$subtype))
        if (input$subtype=="All") {
            plot(survfit(runSurSubtype(), data=BRCA), col=c('red', 'blue'), xlab='Time (days)', ylab='Probability of Survival', lwd=2, main=c(paste("Kaplan Meier Survival Plot of",paste(input$Protein_ID), "in Breast Cancer"))) 
            legend(0,0.2, legend=c("High expression", "Low expression"), col=c("red", "blue"),lty=1, lwd=4)
            
        } else {
            plot(survfit(runSurSubtype(), data=subtype), col=c('red', 'blue'), xlab='Time (days)', ylab='Probability of Survival', lwd=2, main=c(paste("Kaplan Meier Survival Plot of",paste(input$Protein_ID), "in", paste(input$subtype), "Breast Cancer"))) 
            legend(0,0.2, legend=c("High expression", "Low expression"), col=c("red", "blue"),lty=1, lwd=4)
        }
        
    })
    
    # Test for difference between survival curves
    diff <- reactive ({
        subtype <- subset(BRCA, subtype==paste(input$subtype))
        if (input$subtype=="All") {
            diff <- survdiff(runSurSubtype(), data=BRCA)
        } else {
            diff <- survdiff(runSurSubtype(), data=subtype)
        } 
        return(diff)
    })
    
    output$diff <- renderPrint({
        diff() 
    })
    
    #################
    # Summary Plots #
    #################
    
    
    # Barplots by subtype, age group and tumour stage
    output$subtypesummary <- renderPlot({
        plot(BRCA$subtype, col='steelblue', border=F, main= "Distribution of data by subtype")
    })
    output$agesummary <- renderPlot({
        plot(BRCA$age, col='steelblue', border=F, main= "Distribution of data by age")
    })
    output$gradesummary <- renderPlot({
        plot(BRCA$ajcc_pathologic_tumor_stage, col='steelblue', border=F, main= "Distribution of data by tumor stage")
    })
    
    
    # Stacked barplots of subtype by age group and tumour stage
    output$stacked1 <- renderPlot({
        plot(BRCA$subtype, BRCA$age, col=c("black", "darkblue", "dodgerblue3", "cornflowerblue", "lightskyblue", "lightblue", "white"), main="Subtype distribution by age group", xlab="Subtype", ylab="Age group")
    })
    output$stacked2 <- renderPlot({
        plot(BRCA$subtype, BRCA$ajcc_pathologic_tumor_stage, col=c("darkblue", "cornflowerblue", "lightblue", "white"), main="Subtype distribution by tumour stage", xlab="Subtype", ylab="AJCC Pathological Tumour Stage")
    })
    
    
    # KM survival plots by subtype and stage
    output$subtypeKM <- renderPlot({
        surv_object <- Surv(time = BRCA$OS.time, event = BRCA$OS)
        BRCA$subtype <- factor(BRCA$subtype, levels = c("Basal", "LumA", "LumB", "Her2"))
        fit <- survfit(surv_object ~ subtype, data=BRCA)
        plot(fit, col=c("blue", "purple", "turquoise", "seagreen"), xlab='Time (days)', ylab='Probability of Survival', lwd=2, main=c(paste("Kaplan Meier Plot of Suvival by Subtype"))) 
        legend(0,0.4, legend=c("Basal", "LumA", "LumB", "Her2"), col=c("blue", "purple", "turquoise", "seagreen"),lty=1, lwd=4)
    })
    output$stageKM <- renderPlot({
        surv_object <- Surv(time = BRCA$OS.time, event = BRCA$OS)
        fit <- survfit(surv_object ~ ajcc_pathologic_tumor_stage, data=BRCA)
        plot(fit, col=c("blue", "purple", "turquoise", "seagreen"), xlab='Time (days)', ylab='Probability of Survival', lwd=2, main=c(paste("Kaplan Meier Plot of Suvival by Stage"))) 
        legend(0,0.3, legend=c("Stage I", "Stage II", "Stage III", "Stage IV"), col=c("blue", "purple", "turquoise", "seagreen"),lty=1, lwd=4)
    })
    
    
    # Forest plots illustrate survival by subtype, stage, age group, as analysed by cox proportional hazard models
    output$subtypeCox <- renderPlot({
        BRCA$subtype <- factor(BRCA$subtype, levels = c("Normal", "Basal", "LumA", "LumB", "Her2"))
        fit.coxph <- coxph(Surv(time = BRCA$OS.time, event = BRCA$OS) ~ subtype, data = BRCA)
        ggforest(fit.coxph, main="Hazard Ratio by Subtype", data=BRCA)   
    })
    output$ageCox <- renderPlot({
        fit.coxph <- coxph(Surv(time = BRCA$OS.time, event = BRCA$OS) ~ age, data = BRCA)
        ggforest(fit.coxph,main="Hazard Ratio by Age Group", data=BRCA)
    })
    output$stageCox <- renderPlot({
        fit.coxph <- coxph(Surv(time = BRCA$OS.time, event = BRCA$OS) ~ ajcc_pathologic_tumor_stage, data = BRCA)
        ggforest(fit.coxph, main="Hazard Ratio by Tumour Stage", data=BRCA)
    })
        
    
    ###################
    #   Custom Data   #
    ###################
    
    
    # Define dataframe from input file
    myData <- reactive ({
        req(input$file1)
        inFile <- input$file1
        df <- read.csv(inFile$datapath)
        df
    })
    
    # Run survival function on input data
    runSur2 <- reactive({
        req(input$custom_variable)
        survfit(as.formula(paste("Surv(time,endpoint) ~ ",paste(input$custom_variable))),data=myData())
        
    })
    
    # Plot survival function
    output$plot_survival2 <- renderPlot ({
        plot(runSur2(), col=c('red', 'blue'), xlab='Time (days)', ylab='Probability of Survival', lwd=2, main = c(paste("Kaplan Meier Survival Plot of",paste(input$custom_variable))))
        legend(0,0.2, legend=c("High expression", "Low expression"), col=c("red", "blue"),lty=1, lwd=4)
        
    })
    
    # Print input data as a table
    output$contents <- renderTable({
        myData()
        
    })
    
    # Test for difference in survival curves
    diff2 <- reactive ({
        req(input$custom_variable)
        diff <- survdiff(as.formula(paste("Surv(time,endpoint) ~" , input$custom_variable)), data = myData())
        return(diff)
    })
    
    output$diff2 <- renderPrint ({
        diff2()
    })
    
    
    # Print sample data table structure for "Help" section
    output$sampletable <- renderTable({
        sample <- read.csv("BRCA_subset_median.csv")
        head(sample)
    })  
}



# Run the application 
shinyApp(ui = ui, server = server)
