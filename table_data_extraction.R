library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyr)
library("tabulizer")
library(data.table)
library(tm)

# filename <- "/home/suso/allpdfs/allpdfs/CACZ885I2202-4735.pdf" 
# 
# text_tabulizer <- extract_text(filename,pages = 9)
#  
# table_tabulizer <- extract_tables(filename, pages = 9, method = "lattice")
# 
# text <- pdf_text(filename)
# 
# text[8]
# 

xlsFolder <- "/home/suso/ihw/csr/CSRAnnotator/xls_tables/"

xlsFiles <- list.files(xlsFolder)


library( readxl )

require(xlsx)
# example of reading xlsx sheets

allTables <- data_frame("doc_path" = "","doc" = "","table" = list(), "page"=1) %>% filter(FALSE) 

for(f in xlsFiles){
  
  filePath <- paste0(xlsFolder,f)
  
  print(filePath)
  
  l <- length( excel_sheets( filePath ) )
  print (l)
  for( possible_page in 1:l){
  
    possible_table <- read.xlsx(filePath, possible_page )
      
    if ( !is.null(possible_table) ){
      
      # print(possible_table)
      new_table <- data_frame("doc_path" = filePath,"doc" =  str_replace_all(f,".xlsx","") ,"table" = list(possible_table), "page"= possible_page)
      allTables <- allTables %>% rbind(new_table)
      
    }
    
  }

}

saveRDS(allTables, "testTables.rds")

