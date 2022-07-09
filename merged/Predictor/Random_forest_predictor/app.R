
library(randomForest, quietly =TRUE)
library(vroom)
library(shiny)

rf.fit.RI <- readRDS("rf.fit.rds")
rf.fit.Abbe <- readRDS("rf.fit.abbe.rds")
# Define UI for application that draws a histogram



ui <- fluidPage(
  
  # Application title
  titlePanel("Random Forest Regression"),
  titlePanel("Estimation of Refractive Index (RI) and Abbe number"),
  h2 ("Please upload the Composition File "),
  h6 ("___________________________________________________________________________________"),
  h6 ("___________________________________________________________________________________"),
  h1 (" "),
  sidebarPanel(
  fluidRow(column(12,
    fileInput("upload", "select a .csv file", accept = ".csv"),
  )),
  h1(" ")),
  mainPanel(
  tableOutput("files1"),
  tableOutput("head1"),
  tableOutput("files2"),
  tableOutput("head2"),
  
  fluidRow(
  column (6,
  actionButton("predict", "predict RI & Abbe", class = "btn-lg btn-success" ),
  )
),
h2 (" "),
fluidRow(
  column (6, 
          downloadButton("Download1", label ="Download Prediction", class = "btn-lg btn-success"  )
  )
),

dataTableOutput("predictedRI")


))
 
#----------------------------------------------------------------------
#--------------------------- Server Functions -------------------------
#----------------------------------------------------------------------


server <- function(input, output, session) {
    unlink("input_data.csv")  
  
  dataIN <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch (ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      validate("invalid file; Please upload a .csv file") )
  })

      
  
 # output$predictedRI <- renderDataTable(head(RI_data)) #renderDataTable( 
   #predicted.RI <- predict(rf.fit.RI, newdata=RI_data[]) )
#  Predicted.Abbe <- Predict(rf.fit.Abbe, data())
  observe({
    if (is.null(input$upload)) return()
    file.copy(input$upload$datapath, "input_data.csv", recursive = TRUE)
  })
 
  prediction <- reactive(
    {input$predict
      data=read.csv("input_data.csv")
    temp=cbind(data,data.frame(predict(rf.fit.RI,data),data.frame(predict(rf.fit.Abbe,data))))
    temp
    }) # giving data frame is important else output will not be rendered
  
  
  output$files1 <- renderTable(input$upload)
  output$head1 <- renderTable(head(dataIN()))
  output$predictedRI <- renderDataTable(prediction())
  
   # downloading the results file
  output$Download1 <- downloadHandler(
    filename = function(){
      paste("predictions",".csv")
    },
    content = function(file){
      write.csv(prediction(),file)
    }
  )
}



shinyApp(ui, server)

