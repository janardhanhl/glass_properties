
library(reticulate)
library(keras)
library(tensorflow)
library(vroom)
library(shiny)

dl.fit.RI <- load_model_tf("modeldl4")
dl.fit.Abbe <- load_model_tf("modeldlabbe")
RI_template <- read.csv("RI_template.csv")

# Define UI for application that draws a histogram



ui <- fluidPage(
    tabsetPanel(
      tabPanel("RI",   # Application title
               titlePanel("Deep Learning Regression"),
               titlePanel("Estimation of Refractive Index (RI)"),
               h2 ("Please upload the Composition File "),
               hr (),
               hr (),
               h1 (" "),
               #side panel ----
               sidebarPanel(
                 fluidRow(column(12, 
                                 downloadButton("Downloadritemp", label ="Download Template", class = "btn-s  "  )
                 )),
                 h5(" "),
                 fileInput("uploadri", "select a .csv file", accept = ".csv"),
                 h1(" "),
                 fluidRow(
                   column (6,
                           actionButton("predictri", "predict RI", class = "btn-lg btn-success" ),
                   )
                 ),
                 h2 (" "),
                 fluidRow(
                   column (6, 
                           downloadButton("Downloadripred", label ="Download Prediction", class = "btn-lg btn-success"  )
                   )
                 )),
               # Main panel ----
               mainPanel(
                 h5(" 1. Download the template file 2. upload the glass composition in atomic fractions "),
                 h5("3. Predict the glass RI and Abbe number 4. Download the predicted utput"),
                 hr(),
                 p(strong("uploaded file properties")),
                 tableOutput("files1"),
                 hr(),
                 p(strong("Top entries in input file")),
                 hr(),
                 tableOutput("head1"),
                 hr(),
                 tableOutput("files2"),
                 tableOutput("head2"),
                 hr(),
                 h5(p(strong("Prediction"))),
                 hr(),
                 dataTableOutput("predictedRI")
                 
               )),
      tabPanel("Abbe", 
               # Application title
               titlePanel("Deep Learning Regression"),
               titlePanel("Estimation of Abbe number"),
               h2 ("Please upload the Composition File "),
               hr (),
               hr (),
               h1 (" "),
               sidebarPanel(
                 fluidRow(column(12, 
                                 downloadButton("Downloadabtemp", label ="Download Template", class = "btn-s  "  )
                 )),
                 h5(" "),
                 fileInput("uploadabbe", "select a .csv file", accept = ".csv"),
                 h1(" "),
                 fluidRow(
                   column (6,
                           actionButton("predictabbe", "predict Abbe", class = "btn-lg btn-success" ),
                   )
                 ),
                 h2 (" "),
                 fluidRow(
                   column (6, 
                           downloadButton("Download_pred_abbe", label ="Download Prediction", class = "btn-lg btn-success"  )
                   )
                 )),
               mainPanel(
                 h5(" 1. Download the template file 2. upload the glass composition in atomic fractions "),
                 h5("3. Predict the glass RI and Abbe number 4. Download the predicted utput"),
                 hr(),
                 p(strong("uploaded file properties")),
                 tableOutput("files3"),
                 hr(),
                 p(strong("Top entries in input file")),
                 hr(),
                 tableOutput("head3"),
                 hr(),
                 tableOutput("files4"),
                 tableOutput("head4"),
                 hr(),
                 h5(p(strong("Prediction"))),
                 hr(),
                 dataTableOutput("predictedabbe")
                 
               ))
      
    ),
  
)
 
#-------------------------------------------------------------------------------
#--------------------------- Server Functions ----------------------------------
#-------------------------------------------------------------------------------


server <- function(input, output, session) {
    unlink("input_data_RI.csv")  
    unlink("input_abbe_data.csv") 
   output$Downloadritemp <- downloadHandler(
    filename = function(){
      paste("RI_Template",".csv")
    },
    content = function(file){
      write.csv(RI_template,file)
    }
  )
  
  dataRI <- reactive({
    req(input$uploadri)
    ext <- tools::file_ext(input$uploadri$name)
    switch (ext,
      csv = vroom::vroom(input$uploadri$datapath, delim = ","),
      validate("invalid file; Please upload a .csv file") )
  })

      
  
 # output$predictedRI <- renderDataTable(head(RI_data)) #renderDataTable( 
   #predicted.RI <- predict(rf.fit.RI, newdata=RI_data[]) )
#  Predicted.Abbe <- Predict(rf.fit.Abbe, data())
  observe({
    if (is.null(input$uploadri)) return()
    file.copy(input$uploadri$datapath, "input_data_RI.csv", recursive = TRUE)
  })
 
  predictionRI <- reactive(
    {input$predictri
      data1=read.csv("input_data_RI.csv")
      datari=model.matrix( ~. -1, data=data1)
    temp=cbind(data1,data.frame(predict(dl.fit.RI,datari)))
    temp
    }) # giving data frame is important else output will not be rendered
  
  
  output$files1 <- renderTable(input$uploadri )
  output$head1 <- renderTable(head(dataRI()))
  output$predictedRI <- renderDataTable(predictionRI())
  
   # downloading the results file
  output$Downloadripred <- downloadHandler(
    filename = function(){
      paste("predictions_RI",".csv")
    },
    content = function(file){
      write.csv(predictionRI(),file)
    }
  )
##################################################################################  
## Abbe Server functions##########################################################
##################################################################################
  
  output$Downloadabtemp <- downloadHandler(
    filename = function(){
      paste("Abbe_Template",".csv")
    },
    content = function(file){
      write.csv(Abbe_template,file)
    }
  )
  
  dataabbe <- reactive({
    req(input$uploadabbe)
    ext <- tools::file_ext(input$uploadabbe$name)
    switch (ext,
            csv = vroom::vroom(input$uploadabbe$datapath, delim = ","),
            validate("invalid file; Please upload a .csv file") )
  })
  
  
  
  # output$predictedRI <- renderDataTable(head(RI_data)) #renderDataTable( 
  #predicted.RI <- predict(rf.fit.RI, newdata=RI_data[]) )
  #  Predicted.Abbe <- Predict(rf.fit.Abbe, data())
  observe({
    if (is.null(input$uploadabbe)) return()
    file.copy(input$uploadabbe$datapath, "input_abbe_data.csv", recursive = TRUE)
  })
  
  predictionabbe <- reactive(
    {input$predictabbe
      data2=read.csv("input_abbe_data.csv")
      dataab=model.matrix( ~. -1, data=data2)
      temp2=cbind(data2,data.frame(predict(dl.fit.Abbe,dataab)))
      temp2
    }) # giving data frame is important else output will not be rendered
  
  
  output$files3 <- renderTable(input$uploadabbe)
  output$head3 <- renderTable(head(dataabbe()))
  output$predictedabbe <- renderDataTable(predictionabbe())
  
  # downloading the results file
  output$Download_pred_abbe <- downloadHandler(
    filename = function(){
      paste("predictions",".csv")
    },
    content = function(file){
      write.csv(predictionabbe(),file)
    }
  )
  
}



shinyApp(ui, server)

