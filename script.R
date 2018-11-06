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

document <- "./JournalChapter1.pdf"

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

tstats <- getPDFTermStats(document)




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


doctables <- getTablesPerPage(document)


# 
# text_df %>%
#   unnest_tokens(word, text, token = "ngrams", n = 3, n_min = 3) %>% 
#   anti_join(stop_words) %>% count(word, sort=TRUE)


f <- system.file(".","mypaper.pdf", package = "tabulizer")
out1 <- extract_tables(f)
str(out1)

# 
# f <- system.file("examples", "data.pdf", package = "tabulizer")
# out1 <- extract_tables("./mypaper.pdf")
# 
# out1[[1]][,1]

