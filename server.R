library(shiny)
library(survival)
library(survminer)
library(ggplot2)

BRCA <- read.csv('BRCA_survival_median.csv')


shinyServer(function(input, output) {

    # Run survival function depending on user selected protein
    runSur <- reactive({
        fit <- survfit(as.formula(paste("Surv(DSS.time,DSS) ~" , paste(input$Protein_ID))), data = BRCA)
        return(fit)
    })
   
    output$plot_survival <- renderPlot({
        plot(runSur(), col=c('red', 'blue'), xlab='Time (weeks)', ylab='Probability of Survival', lwd=2) 
        legend(7000,1, legend=c("High expression", "Low expression"), col=c("red", "blue"),lty=1, lwd=4)
    })
    
    runCoxph <- reactive({
        fit.coxph <- coxph(as.formula(paste("Surv(DSS.time,DSS) ~" , paste(input$Protein_ID))), data = BRCA)
        return(fit.coxph)       
    })
    
    output$coxph <- renderPlot({
        ggforest(runCoxph(), main="")
    })
    
    output$coxph_caption <- renderText({
        paste("Forest Plot for Cox Proportional Hazards Model for", input$Protein_ID, "in breast cancer.", sep="\n")
    })
    
    output$survdiff <- renderTable({
        diff()
    })
    
    # Caption will show below the graph; based on which protein you select
    output$caption <- renderText({
        paste("Kaplan Meier Survival Graph of", input$Protein_ID, "in breast cancer.", sep="\n")
    })
    
    output$moreinfo <- renderText({
        paste("Data is separated based on median expression from 873 patients in the TCGA Breast dataset.")
    })
    
    
    summary <- reactive({
        summary(runSur())
    })
    
    
    output$subtypesummary <- renderPlot({
        plot(BRCA$subtype, col='pink', border=F, main= "Distribution of data by subtype")

    })
    output$agesummary <- renderPlot({
        plot(BRCA$age, col='pink', border=F, main= "Distribution of data by age")
        
    })
    output$gradesummary <- renderPlot({
        plot(BRCA$ajcc_pathologic_tumor_stage, col='pink', border=F, main= "Distribution of data by tumor stage")
        
    })
    
    
})

