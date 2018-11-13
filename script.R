# install.packages("tm")
library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyr)
library("tabulizer")
library(data.table)

# download.file("http://theses.gla.ac.uk/8666/1/2018Rodriguez-PerezPhD.pdf", "./mypaper.pdf")



getPDFTermStats <- function(filename) {

  text <- pdf_text(filename)
  
  dfs = data_frame("ngram" = NA,"n" = NA,"first" = NA,"second"= NA, "page" = NA) %>% filter(FALSE)
  
  for (page in 1:length(text)) {
        
        text_df <- data_frame( text = text[page])
        
        text_df %>%
          unnest_tokens(word, text) %>% 
          anti_join(stop_words) -> extracted_terms
        
        text_df <- data_frame( text = paste(extracted_terms$word, collapse = ' ') )
        
        text_freqs <- text_df %>% unnest_tokens(ngram, text, token = "ngrams", n = 2, n_min = 2) %>% count(ngram, sort=TRUE)
        
        text_freqs %>% separate(ngram, 
                                c("first", "second"),
                                extra='drop') %>% select(first,second) -> splitted
        
        text_freqs <- text_freqs %>% cbind(splitted) %>% as.data.table 
        text_freqs[,page:=page]
        
        dfs <- dfs %>% rbind(text_freqs)
  
  }
  
  return(dfs)
}


document <- d
tokens <- c("patient")

getDocumentParagraphsForTokens <- function(document, tokens, around = TRUE){
  

  searchPattern <- paste0(tokens, "|", collapse = "")
  searchPattern <- substr(searchPattern,1,str_length(searchPattern)-1)
  
  
  text <- pdf_text(document)
  
  text %>% str_split("\n") -> a
  
  a %>% unlist %>% str_trim() %>% str_to_lower() -> b
  
  grepl( str_to_lower(searchPattern) , b ) -> c
  
  if(around) {
    shift(c, n=1, fill=0, type="lag") | shift(c, n=1, fill=0, type="lead") | c -> matches_and_leads
  } else {
    c -> matches_and_leads
  }
  
  b[matches_and_leads]
  
}



getTablesPerPage <- function(document){
  
    npages <- get_n_pages(document)
    
    tables <- list()
    
    for (page in 1:npages) {
      
      possible_table <- extract_tables(document, pages = page,  output = "data.frame") %>% print
      
      print(possible_table)
      
      tryCatch({
        tables[page] <- ifelse(possible_table %>% is_empty(), NA, possible_table)
      }, error = function(e) {
        browser()
        tables[page] <- NA
      })
      
    }

    return (tables)
}


getPagesWithTerms <- function(terms, tstats){
  
  tstats %>% filter( str_detect(ngram,"sub") ) %>% select(page) %>% distinct
  
}


document <- "./CLAF237A2301-2276.pdf"
tstats <- getPDFTermStats(document)

doctables <- getTablesPerPage(document)



docs <- paste0("/home/suso/allpdfs/allpdfs/",list.files("/home/suso/allpdfs/allpdfs"))

# docs <- c("./CLAF237A2301-2276.pdf","./CLAF237A2301-2276.pdf")



for( d in 1:length(docs) ){
 
  
  print(paste0(d,"/",length(docs)))
  
  d <- docs[d]
  
  tryCatch(
    {
      res <- getDocumentParagraphsForTokens(d, c("subgroup","sub-group"))
      
      # res <- getDocumentParagraphsForTokens(d, c("patient","population"), FALSE)
      
      if ( length(res) > 0 ){
        if (!exists("result")){
            result <- data.table(doc=d,sentences=res)
        } else {
            result <- result %>% rbind(data.table(doc=d,sentences=res))
        }
      }
  
    },
    error= function(cond){
      print(paste0(d, " : ", cond))
    },
    warning= function(cond){
      # print(d)
    }
    
    )
  
}



