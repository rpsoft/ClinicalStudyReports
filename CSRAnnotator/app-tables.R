#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

results <- readRDS("../sentences-gsk.rds")
annotation_file <- "./annotations-gsk.rds"


totalDocs <- (results %>% select(doc) %>% distinct %>% dim)[1]
docids <- (results %>% select(doc) %>% distinct )

if(file.exists(annotation_file)){
  sg_results <- readRDS(annotation_file)
} else {
  sg_results <- data.table(docid=docids,sg="dk")
}

getResultsSentences <- function (index, results){
  docList <- (results %>% select(doc) %>% distinct )
  currentDocID <- docList[index] %>% toString()
  sentencesForDoc <- (results %>% filter (doc == currentDocID))
  sentencesContent <- sentencesForDoc %>% select(sentences) %>% as_vector() %>% paste("", collapse =" [...] ")
  return(sentencesContent)
}

getCurrentDocId <- function ( index, results){
  docList <- (results %>% select(doc) %>% distinct )
  # browser()
  currentDocID <- docList[index,] %>% toString()
  return(currentDocID)
}


# Define UI for application that draws a histogram
ui <- fluidPage( theme = "mystyle.css",
   
   titlePanel("CSR Tables Annotator"),

   
   mainPanel(
     actionButton("prev", "Previous"),
     actionButton("next", "Next"),
     uiOutput("index"),
     uiOutput("docid"),
     
     radioButtons(inputId = "hasSubroup",label = "Has sub groups?", choiceValues = c("dk","n","y"), choiceNames = c("Don't know","No","Yes"), selected = "dk"),
     
     uiOutput("sentences")
     
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  index <- reactiveVal(0) 
  sg_results <- reactiveVal( sg_results )
  
  
  # First RUN
  output$docid <- renderUI({div(getCurrentDocId(index(),results),class="title")})
  output$index <- renderUI({div(paste(index()," / ",(sg_results() %>% dim())[1] ), class="title")})    
  
  output$sentences <- renderUI({
    div(getResultsSentences(index(),results), class="sentences", id="sentences")
  })
  
  observe({
    docid <- getCurrentDocId(index(),results)  
    updateRadioButtons(session, "hasSubroup", selected= (sg_results() %>% filter(docid.doc == docid))[2]   )
  })
  
  observeEvent(input$hasSubroup, {
      
      docid <- getCurrentDocId(index(),results)  
      
      sg_results( sg_results() %>% mutate(sg = ifelse(docid.doc == docid, input$hasSubroup, sg)))
      
      write_rds(sg_results(),annotation_file)
    
  })
  
  
  observeEvent(input$prev, {
    # Change the following line for more examples
   
    
    index( ifelse(index() -1 < 0 , 1, index()-1 ) )
    
    output$docid <- renderUI({div(getCurrentDocId(index(),results),class="title")})    
    
    output$sentences <- renderUI({
      div(getResultsSentences(index(),results), class="sentences", id="sentences")
    })
    
    # print(sg_results)
  })
  
  observeEvent(input$"next", {
    # Change the following line for more examples
    
    index( ifelse(index() +1 < 0 , 1, index()+1 ) )
    
    output$docid <- renderUI({div(getCurrentDocId(index(),results),class="title")})    
    
    output$sentences <- renderUI({
      div(getResultsSentences(index(),results), class="sentences", id="sentences")
    })
    
    
    
  })
  
  # 
  # 
  # output$sentences <- renderUI({
  #   div(getResultsSentences(index,results), class="sentences", id="sentences")
  # })
  # 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

