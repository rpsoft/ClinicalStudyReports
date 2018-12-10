#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

tables <- readRDS("./testTables.rds")

colnames(tables) <- c("filename", "doc", "tables", "page")



allCSR_sg <- readRDS("./all_sgs.rds")


tables <- tables %>% filter(tables != "NULL")
tables <- tables %>% filter(doc %in% allCSR_sg$docid.doc)

annotation_file <- "./annotation_tables.rds"

totalTables <- (tables %>% select(doc) %>% dim)[1]
docids <- (tables %>% select(doc) %>% distinct )

if(file.exists(annotation_file)){
  sg_results <- readRDS(annotation_file)
} else {
  sg_results <- tables %>% mutate(sg = "dk") 
}

getTable <- function (index, results){
  return(tables[index,]$tables[[1]])
}

getCurrentDocId <- function ( index, results){
  currentDocID <- tables[index,]$doc %>% toString()
  return(currentDocID)
}

getCurrentDocPage <- function ( index, results){
  page <- tables[index,]$page %>% toString()
  return(page)
}


# Define UI for application that draws a histogram
ui <- fluidPage( theme = "mystyle.css",
   
   titlePanel("CSR Tables Annotator"),

   
   mainPanel(

     uiOutput("index"),
     uiOutput("docid"),
     uiOutput("doc_page"),
     
     fixedPanel( top = 0, right = 0,
       radioButtons(inputId = "hasSubroup",label = "About sub groups?", choiceValues = c("dk","n","y"), choiceNames = c("Don't know","No","Yes"), selected = "dk"),
       actionButton("prev", "Previous"),
       actionButton("next", "Next")
     ),
     
     # uiOutput("tableHolder"),
     
     tableOutput("tableHolder")
     
     
     
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  index <- reactiveVal(1) 
  sg_results <- reactiveVal( sg_results )
  
  # First RUN
  output$docid <- renderUI({div(getCurrentDocId(index(),results),class="title")})
  output$doc_page <- renderUI({div(getCurrentDocPage(index(),results),class="title")})
  
  output$index <- renderUI({div(paste(index()," / ",totalTables  ), class="title")})    
  
  output$tableHolder <- renderTable(getTable(index(),tables), class="tableHolder", id="tableHolder")  
  
  observe({
    docid <- getCurrentDocId(index(),tables)
    Page <- getCurrentDocPage(index(),tables)  
    
    updateRadioButtons(session, "hasSubroup", selected= (sg_results() %>% filter(doc == docid & page == Page))$sg %>% as.character()   )
  })
  
  observeEvent(input$hasSubroup, {
      
      docid <- getCurrentDocId(index(),tables)
      Page <- getCurrentDocPage(index(),tables)  
      
      sg_results( sg_results() %>% mutate(sg = ifelse(doc == docid & page == Page, input$hasSubroup, sg)))
      
      write_rds(sg_results(), annotation_file)
    
  })
  
  
  observeEvent(input$prev, {
    index( ifelse(index() -1 <= 0 , 1, index()-1 ) )

    output$docid <- renderUI({div(getCurrentDocId(index(),tables),class="title")})

   #  output$sentences <- renderUI({
   #    div(getResultsSentences(index(),results), class="sentences", id="sentences")
   #  })

  })
  
  observeEvent(input$"next", {
    
    index( ifelse(index() +1 > totalTables , index(), index()+1 ) )

    output$docid <- renderUI({div(getCurrentDocId(index(),tables),class="title")})

    # output$sentences <- renderUI({
    #   div(getResultsSentences(index(),results), class="sentences", id="sentences")
    # })
    
    
    
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

