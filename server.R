library(shiny)
library(survival)
library(survminer)
library(ggplot2)

BRCA <- read.csv('BRCA_survival_median.csv')
surv_object <- Surv(time = BRCA$DSS.time, event = BRCA$DSS)

shinyServer(function(input, output) {
    
    # Caption will show below the graph; based on which protein you select
    output$caption <- renderText({
        paste("Kaplan Meier Survival Graph of", input$Protein_ID, sep="\n")
    })
    
    output$info <- renderText({
        paste("Red line indicates high expression, and blue line indicates low expression of", input$Protein_ID, ".")
    })
    
    output$moreinfo <- renderText({
        paste("Data is separated based on median expression from 873 patients in the TCGA Breast dataset.")
    })
    
    # Running the survival function
    runSur <- reactive({
        survfit(as.formula(paste("Surv(DSS.time,DSS) ~ ",paste(input$Protein_ID))),data=BRCA)
        
    })
    
    output$plot_survival <- renderPlot({
        plot(runSur(), col=c('red', 'blue'), xlab='Time (weeks)', ylab='Probability of Survival', lwd=2) 
        legend(10,10, legend=c("High", "Low"),
               col=c("red", "blue"))
    
    })
    
    
    summary <- reactive({
        summary(runSur())
    })
    
    output$summary <- renderPrint({
        surv_object <- Surv(time = BRCA$OS.time, event = BRCA$OS)
        fit <- survfit(surv_object ~ input$Protein_ID, data=BRCA)
        summary(fit)
    })
    

    
    output$raw_data <- renderTable({
        BRCA
    }, include.rownames = FALSE)
    
})

