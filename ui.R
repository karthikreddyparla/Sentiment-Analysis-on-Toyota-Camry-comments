library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)
library(DT)
library(shinythemes)
header <- headerPanel("KarthikReddyParla_Natural_Language_Processing")
header[[2]]$attribs$id = "header"
ui <-(fluidPage(theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                header,
                  mainPanel(
                    tabsetPanel(type = 'tabs',
                                
                                tabPanel("Train Data",   DT::dataTableOutput('RawData') ),
                                tabPanel("Test Data",     DT::dataTableOutput('testData')),
                                 tabPanel("Model Summary", verbatimTextOutput('caption1'),verbatimTextOutput(outputId = "modelSummary")),
                                tabPanel("predicted model results", textOutput(outputId = "predictedmodel")),
                                tabPanel("Accuracy and Confusion martx",  verbatimTextOutput('caption'),tableOutput('table')),
                                tabPanel("ratingAverage", tableOutput("ratingTagAverage1"),tableOutput("ratingAverage1"),tableOutput("ratingAverage2") ),
                                tabPanel("TFIDF",plotOutput("Plot"))
                    )
                    
                  # )
                )
)
)