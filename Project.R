modelFileName <- "neuralNetwork-Model.RData"
library(shiny)
library(EBImage)
library(nnet)
load(modelFileName)
# UI for project
ui <- fluidPage(
  fileInput(inputId = "image","Choose a Image",multiple=FALSE,accept=c('image/png','image/jpeg')),
  textOutput("digit"),
  imageOutput("myImage")
)
server <- function(input, output) {
  
  output$digit <- renderText({
    inFile <- input$image
    if (is.null(inFile))
      return(NULL)
    
    old = inFile$datapath
    new = file.path(dirname(inFile$datapath),inFile$name)
    
    file.rename(from = old , to = new)
    inFile$datapath <- new
    
    Image <- readImage(inFile$datapath)
    nof=numberOfFrames(Image, type = c('total', 'render'))
    
    if(nof==1)
    {
      image=255*imageData(Image[1:28,1:28])
    }else 
      if(nof==3)
      {
        r=255*imageData(Image[1:28,1:28,1])
        g=255*imageData(Image[1:28,1:28,2])
        b=255*imageData(Image[1:28,1:28,3])
        
        image=0.21*r+0.72*g+0.07*b
        
        image=round(image)
      }
    image=t(image)
    
     
    
    image=as.vector(t(image))
    write.csv(t(as.matrix(image)),'E:/Tech MAhindra Project/DigitRecognition-master/threepx.csv',row.names = FALSE)
    testFileName  <-'E:/Tech MAhindra Project/DigitRecognition-master/threepx.csv'
    newTestDataset <- read.csv(testFileName)    # Read the datafile
    
    testreduced <- newTestDataset/255 
    
    testreduced <- as.matrix(testreduced) %*% pcaX$rotation[,1:45]
    
    
    NewPredicted <- predict(model,testreduced,type="class")
    paste("Predicted Value is",NewPredicted)
      })

  output$myImage <- renderImage({
    inFile <- input$image
    if (is.null(inFile))
      return(NULL)
  
    outfile <- tempfile(fileext='.jpg')
   
    old=inFile$datapath
    new = file.path(dirname(inFile$datapath),inFile$name)
    
    file.rename(from = old , to = new)
    inFile$datapath <- new
    
    list(src = inFile$datapath,
         contentType = 'image/jpeg',
         width =200,
         height=200,
         alt = "Predicted Image")
  }, deleteFile = TRUE)
}
 shinyApp(ui = ui, server = server)
 
