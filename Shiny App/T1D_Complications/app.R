#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rms)
DPN_mod<-readRDS("data/T1Dcomp_DPNmod_logit.rds")
DAN_mod<-readRDS("data/T1Dcomp_DANmod_logit.rds")
DRET_mod<-readRDS("data/T1Dcomp_DRETmod_logit.rds")
DNEP_mod<-readRDS("data/T1Dcomp_DNEPmod_logit.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculate You Risk of Type 1 Diabetes Related Complications"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("Age",
                        "Current Age (years):",
                        min = 1,
                        max = 100,
                        value = 30),
            numericInput("Dx_Age",
                         "Age at diagnosis of T1D (years):",
                         min = 1,
                         max = 100,
                         value = 15),
            numericInput("SBP",
                         "Average Systolic Blood Pressure from last 3 visits (mmHg):",
                         min = 1,
                         max = 300,
                         value = 140),
            numericInput("HbA1C",
                         "Average HbA1C from last 3 visits (%):",
                         min = 1,
                         max = 30,
                         value = 6),
            # radioButtons("PeriNeuro",
            #              "Peripheral Neuropathy?",
            #              c("Yes"="Yes",
            #                "No"="No"),
            #              selected = "No"),
            submitButton("Submit"),
            uiOutput("git"),
            textOutput("funding")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #img(src = "T1D_DPN_nomogram.png",height = 300, width = 600),
           textOutput("DPNRisk"),
           textOutput("DANRisk"),
           textOutput("DRETRisk"),
           textOutput("DNEPRisk"),           
           textOutput("DPNPotRisk_HTN"),
           textOutput("DPNPotRisk_HbA1c")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$DPNRisk <- renderText(
        paste("Your Risk of Diabetic Peripheral Neuropathy is: ",
                                    round(predict(DPN_mod,
                                                  newdata=data.frame(Dr_Age=input$Age,
                                                                     Dx_Age=input$Dx_Age,
                                                              AvgA1c=input$HbA1C,
                                                              avg.Systolic=input$SBP),
                                                  na.action = na.pass,
                                                  type = "fitted")*100,4),
                                    "%"
                                    ))

    
    output$DANRisk <- renderText(
        paste("Your Risk of Diabetic Autonomic Neuropathy is: ",
                                       round(predict(DAN_mod,
                                                     newdata=data.frame(Dr_Age=input$Age,
                                                                        Dx_Age=input$Dx_Age,
                                                               AvgA1c=input$HbA1C,
                                                               avg.Systolic=input$SBP),
                                                     type = "fitted")*100,4),
                            "%"))
    
    output$DRETRisk <- renderText(
        paste("Your Risk of Diabetic Retinopathy is: ",
                                       round(predict(DRET_mod,
                                                     newdata=data.frame(Dr_Age=input$Age,
                                                                        Dx_Age=input$Dx_Age,
                                                                        Dur=input$Age-input$Dx_Age,
                                                               AvgA1c=input$HbA1C,
                                                               avg.Systolic=input$SBP),
                                                     type = "fitted")*100,4),
                                       "%"
    ))
    
    output$DNEPRisk <- renderText(
        paste("Your Risk of Diabetic Nephropathy is: ",
                                       round(predict(DNEP_mod,
                                                     newdata=data.frame(Dr_Age=input$Age,
                                                                        Dx_Age=input$Dx_Age,
                                                               AvgA1c=input$HbA1C,
                                                               avg.Systolic=input$SBP),
                                                     type = "fitted")*100,4),
                                       "%"
    ))
    
    output$DPNPotRisk_HTN <- renderText(if(input$SBP<=140){"Your blood pressure is great! Keep up the good work!!!"}else if(input$SBP>140) {
        paste("If you decreased your Systolic Blood Pressure by 20 mmHg then your risk of Diabetic Complications in order would be: ",
              round(predict(DPN_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C,
                                               avg.Systolic=input$SBP-20),
                            na.action = na.pass,
                            type = "fitted")*100,4),
              "%,",
              round(predict(DAN_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C,
                                               avg.Systolic=input$SBP-20),
                            type = "fitted")*100,4),
              "%,",
              round(predict(DRET_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               Dur=input$Age-input$Dx_Age,
                                               AvgA1c=input$HbA1C,
                                               avg.Systolic=input$SBP-20),
                            type = "fitted")*100,4),
              "%, and",
              round(predict(DNEP_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C,
                                               avg.Systolic=input$SBP-20),
                            type = "fitted")*100,4),
              "%")
            
        })
    output$DPNPotRisk_HbA1c <- renderText(if(input$HbA1C<=7){"Your blood sugars are great! Keep up the good work!!!"}else if(input$HbA1C>7) {
        paste("If you decreased your HbA1C by 2% then your risk of Diabetic Complications in order would be: ",
              round(predict(DPN_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C-2,
                                               avg.Systolic=input$SBP),
                            na.action = na.pass,
                            type = "fitted")*100,4),
              "%,",
              round(predict(DAN_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C-2,
                                               avg.Systolic=input$SBP),
                            type = "fitted")*100,4),
              "%,",
              round(predict(DRET_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               Dur=input$Age-input$Dx_Age,
                                               AvgA1c=input$HbA1C-2,
                                               avg.Systolic=input$SBP),
                            type = "fitted")*100,4),
              "%, and",
              round(predict(DNEP_mod,
                            newdata=data.frame(Dr_Age=input$Age,
                                               Dx_Age=input$Dx_Age,
                                               AvgA1c=input$HbA1C-2,
                                               avg.Systolic=input$SBP),
                            type = "fitted")*100,4),
              "%")
        })
    
    #add AUC
    #add how theyre doing compared to others same ago
    url<-a("Github link",href="https://github.com/pmtran5884/T1D_Complications")
    output$git <- renderUI({tagList("The manuscript describing the methods behind this prediction tool is still in progress. Please see the github link for details:", url)})
    output$funding <- renderText(paste("
                                         
                                         This work was supported by grants from the National Institutes of Health (R21HD050196, R33HD050196, and 2RO1HD37800) and JDRF (1-2004-661) to JXS. PMHT was supported by NIH fellowship (F30DK12146101A1)."))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
