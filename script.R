# install.packages("tm")
# install.packages("pdftools")
library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyr)
library("tabulizer")
library(data.table)
library(tm)
# library("rpoppler")

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


# document <- "/home/suso/allpdfs/testpdfs/CACZ885I2202-4735.pdf"
# tokens <- c("subgroup","sub-group","interaction","stratif*","restrict*","hetero*","homo*")

# tokens <- c("hetero*","homo*")

getDocumentParagraphsForTokens <- function(document, tokens, around = TRUE){
  
  
  searchPattern <- paste0(tokens, "|", collapse = "")
  searchPattern <- substr(searchPattern,1,str_length(searchPattern)-1)
  
  searchPattern <- str_replace_all(searchPattern, "\\*", "[A-z]*")
  
  
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
  
  npages <- 30
  
  tables <- list()
  
  for (page in 1:npages) {
    
    print(paste0(page,"/",npages))
    possible_table <- extract_tables(document, pages = page,  output = "data.frame")
    # browser()
    # print(possible_table)
    
    tryCatch({
      tables[page] <- ifelse(possible_table %>% is_empty(), NA, possible_table)
    }, error = function(e) {
      # browser()
      tables[page] <- NA
    })
    
  }
  
  return (tables)
}

#new version to parallelise the table extraction. ## file access racing conditions make it fail.
# getTablesPerPage_par <- function(document,cores){
#   
#   library(doParallel)
#   registerDoParallel(cores=cores)
#   
#   npages <- get_n_pages(document)
#   
getTablesPerPage <- function(document){
  
  npages <- get_n_pages(document)
  
 # npages <- 10
  
  tables <- list()
  
  for (page in 1:npages) {
    
    print(paste0(page,"/",npages))
    tryCatch({
      possible_table <- extract_tables(document, pages = page,  output = "data.frame")
      tables[page] <- ifelse(possible_table %>% is_empty(), NA, possible_table)
    }, error = function(e) {
      # browser()
      tables[page] <- NA
    })
    
  }
  
  return (tables)
}

#   npages <- 5
#   
#   tables <- foreach(page=1:npages, .combine=rbind) %dopar% {
#     
#     # for (page in 1:npages) {
#     
#     possible_table <- extract_tables(document, pages = page,  output = "data.frame") %>% print
#     
#     # print(possible_table)
#     
#     tryCatch({
#       # return(ifelse(is_empty(possible_table), list(), possible_table))
#       return(list(list(1,2,3)))
#     }, error = function(e) {
#       # browser()
#       return(list(list(1,2,3,4)))
#     })
#     
#   }
#   
#   return (tables)
# }

# getTablesPerPage_par(d,4) -> test

# getTablesPerPage(d) -> test


# #not preserving the page number.
# getTables <- function(document){
#   # 
#   # npages <- get_n_pages(document)
#   # 
#   # tables <- list()
#   # 
#   # for (page in 1:npages) {
#   #   
#   #   possible_table <- extract_tables(document, output = "data.frame") %>% print
#   #   
#   #   # print(possible_table)
#   #   
#   #   tryCatch({
#   #     tables[page] <- ifelse(possible_table %>% is_empty(), NA, possible_table)
#   #   }, error = function(e) {
#   #     browser()
#   #     tables[page] <- NA
#   #   })
#   #   
#   # }
#   
#   return (extract_tables(document, output = "data.frame") %>% print)
# }



getPagesWithTerms <- function(terms, tstats){
  
  tstats %>% filter( str_detect(ngram,"sub") ) %>% select(page) %>% distinct
  
}


# document <- "./CLAF237A2301-2276.pdf"
# tstats <- getPDFTermStats(document)
# doctables <- getTablesPerPage(document)


# Given tokens do a seach on sentences of all PDFS located within the folder. // folder is a string that needs to be terminated with "/"
findTokensInSentences <- function (folder, tokens){
  
  fileNames <- list.files(folder)
  docs <- paste0(folder,fileNames)
  # browser()
  for( d in 1:length(docs) ){
    f <- str_replace_all(fileNames[d],".pdf","")
    
    print(paste0(d,"/",length(docs), " : ", docs[d]))
    
    d <- docs[d]
    
    tryCatch(
      {
        res <- getDocumentParagraphsForTokens(d, tokens, TRUE)
        
        # res <- getDocumentParagraphsForTokens(d, c("patient","population"), FALSE)
        
        if ( length(res) > 0 ){
          if (!exists("result")){
            result <- data.table(filename=d, doc=f,sentences=res)
          } else {
            result <- result %>% rbind(data.table(filename=d,doc=f,sentences=res))
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
  
  return(result)
  
}



#Parallel version



findTokensInSentences_par <- function (folder, tokens, cores = 4){
  library(doParallel)
  registerDoParallel(cores=cores)
  
  fileNames <- list.files(folder)
  docs <- paste0(folder,fileNames)
  
  
  result <- foreach(d=1:length(docs), .combine=rbind) %dopar% {
    
    # for( d in 1:length(docs) ){
    f <- str_replace_all(fileNames[d],".pdf","")
    
    # print(paste0(d,"/",length(docs), " : ", docs[d]))
    
    d <- docs[d]
    
    tryCatch(
      {
        res <- getDocumentParagraphsForTokens(d, tokens, TRUE)
        
        # res <- getDocumentParagraphsForTokens(d, c("patient","population"), FALSE)
        
        if ( length(res) > 0 ){
          return(data.table(filename=d,doc=f,sentences=res))
        } else {
          return(data.table(filename=d,doc=f,sentences=NA))
        }
        
      },
      error= function(cond){
        return(data.table(filename=d,doc=f,sentences=NA))
      },
      warning= function(cond){
        # print(d)
      }
      
    )
    
  }
  
  return(result)
  
}


## Get all tables for all documents 
extractAllTables_par <- function (folder, cores = 4){
  library(doParallel)
  registerDoParallel(cores=cores)
  
  fileNames <- list.files(folder)
  docs <- paste0(folder,fileNames)
  
  
  result <- foreach(d=1:length(docs), .combine=rbind) %dopar% {
    
    f <- str_replace_all(fileNames[d],".pdf","")
    
    d <- docs[d]
    
    tryCatch(
      {
        res <- getTablesPerPage(d)
        
        tabs <- data.table(filename=d,doc=f,tables=res,page=1:(res %>% length))
        tabs <- tabs %>% filter (! is.na(tables) ) 
        
        return(1)
        
      },
      error= function(cond){
        return(NA)
      },
      warning= function(cond){
        return(NA)
      }
      
    )
    
  }
  
  return(result)
  
}

extractAllTables <- function (folder){
 
  fileNames <- list.files(folder)
  docs <- paste0(folder,fileNames)
  
  allTables <- data.table(filename=NA,doc=NA,tables=NA,page=NA) %>% filter(!is.na(filename))
  
  
  for(d in 1:length(docs)){
    print(paste0("processing file: ",docs[d], "  ", d, " / ",length(docs) ))
    f <- str_replace_all(fileNames[d],".pdf","")
    d <- docs[d]
    
    tryCatch({
      res <- getTablesPerPage(d)
      tabs <- data.table(filename=d,doc=f,tables=res,page=1:(res %>% length))
      tabs <- tabs %>% filter (! is.na(tables) ) 
      allTables <- allTables %>% rbind(tabs)
    },
    error= function(cond){
      return(paste0(d," failed, probably empty"))
    },
    warning= function(cond){
      return(paste0(d," warning, probably empty"))
    })
  
    
  }
  
  return(allTables)
}

# results <- findTokensInSentences("/home/suso/allpdfs/allpdfs/",c("subgroup","sub-group","interaction","stratif*","restrict*","hetero*","homo*"))

# These are novo nordisk and novartis CSRs
# results_par <- findTokensInSentences_par("/home/suso/allpdfs/allpdfs/",c("subgroup","sub-group","interaction","stratif*","restrict*","hetero*","homo*"), 8) ## Beast mode. Using parallelisation. 
# results_par <- results_par %>% filter(! (sentences %>% is.na())) -> results
# saveRDS(results, "sentences-novo-nova.rds")

# 
# # These are GSKs CSRs
# results_par <- findTokensInSentences_par("/home/suso/ihw/Decoded/",c("subgroup","sub-group","interaction","stratif*","restrict*","hetero*","homo*"), 8) ## Beast mode. Using parallelisation. 
# results_par <- results_par %>% filter(! (sentences %>% is.na())) -> results
# 
# 
# results_tables <- extractAllTables("/home/suso/allpdfs/allpdfs/")

results_tables_gsk <- extractAllTables("/home/suso/ihw/Decoded/")


# p <- extract_text("/home/suso/ihw/Decoded/102871.pdf.decoded.pdf",pages = 24)


# 
# saveRDS(results, "sentences-gsk.rds")
# results_tables
# all_aact_tables <- readRDS("~/ihw/csr/all_aact_tables.Rds")
# 
# 
# all_aact_tables %>%  View

# make_thumbnails(file = "/home/suso/allpdfs/allpdfs/CACZ885I2202-4735.pdf", pages = 9, resolution = 120) -> thumbnail

