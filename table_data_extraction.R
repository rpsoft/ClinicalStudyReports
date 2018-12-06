library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyr)
library("tabulizer")
library(data.table)
library(tm)

filename <- "/home/suso/allpdfs/allpdfs/CACZ885I2202-4735.pdf" 

text_tabulizer <- extract_text(filename,pages = 9)
 
table_tabulizer <- extract_tables(filename, pages = 9, method = "lattice")

text <- pdf_text(filename)

text[8]