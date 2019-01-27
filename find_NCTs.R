
library(stringi)


list.files("../Decoded") -> csr_files

csr_files %>% as.data.frame() -> csr_files

colnames(csr_files) = c("filename")

csr_files <- csr_files %>% mutate( SIN = str_replace(filename, ".pdf.decoded.pdf",""))

csr_files <- csr_files %>% mutate( SIN = str_replace(SIN, "^gsk-",""))



All_Sponsor_Funder_Studies <- read_csv("All-Sponsor-Funder-Studies.csv")

colnames(All_Sponsor_Funder_Studies)[4] <- "SIN"

csr_files %>% left_join(All_Sponsor_Funder_Studies) -> first_matched

first_matched %>% filter(is.na(`Trial Registry Identification Number(s)`) ) %>% 
  mutate (SIN = stri_replace_last_fixed(SIN, '-', '/')) %>% select(filename,SIN) %>% left_join(All_Sponsor_Funder_Studies) -> second_matched

# second_matched %>% filter(is.na(`Trial Registry Identification Number(s)`) ) %>% select(SIN)

GSK_Studies  <- All_Sponsor_Funder_Studies %>% 
  filter(Sponsor == "GSK") %>% mutate(full_SIN = SIN)
  

GSK_Studies <- GSK_Studies %>% mutate(SIN = gsub("[^0-9.]", "", SIN) )





first_matched %>% mutate(full_SIN = SIN) %>% filter(!is.na(`Trial Registry Identification Number(s)`) ) -> first_matched
second_matched %>% mutate(full_SIN = SIN) %>% filter(!is.na(`Trial Registry Identification Number(s)`) ) -> second_matched

csr_files %>% inner_join(GSK_Studies) %>% rbind(second_matched) %>% rbind(first_matched) -> matched_GSK_CSRs

matched_GSK_CSRs %>% View

csr_files %>% filter( !(filename %in% matched_GSK_CSRs$filename) ) -> unmatched_CSRs



found_CSRs <- matched_GSK_CSRs %>% filter(NA)

for(s in unmatched_CSRs$SIN){
  found <- GSK_Studies %>% filter(str_detect(SIN, s ))
  if ( found %>% nrow > 0){
    found <- found %>% cbind(s)
    found_CSRs <- found_CSRs %>% rbind(found)
  }
}



csr_files %>% inner_join((found_CSRs %>% mutate(SIN = s) %>% select(-s) ) ) -> last_CSRs_to_match

last_CSRs_to_match %>% rbind(matched_GSK_CSRs) %>% View

last_CSRs_to_match %>% rbind(matched_GSK_CSRs) -> GSK_final_matched

GSK_final_matched %>% select(filename) %>% distinct()


checkingfiles <- list.files("../Decoded") %>% as.data.frame()

colnames(checkingfiles) = c("filename")



checkingfiles %>% left_join(GSK_final_matched) %>% View
checkingfiles %>% anti_join(GSK_final_matched) %>% View


checkingfiles %>% left_join(GSK_final_matched) %>% select(SIN,full_SIN,`Trial Registry Identification Number(s)`,filename) %>% distinct %>% write_csv("GSK_matches.csv")

###### NOW we check for Novo Nordisk and Novartis

novo_novartis_wanted <- read_rds("novo_novartis_wanted.rds")

novo_novartis_wanted %>% select(nct_id) %>% write_csv("novo_novartis_wanted_nctids.csv")

novo_novartis_wanted %>% filter(nct_id == "NCT00171496")

novo_novartis_wanted %>% View()

files_wanted <- list.files("/home/suso/ihw/csr/allpdfs/wanted")

files_wanted <- files_wanted %>% as_data_frame()

colnames(files_wanted)[1] <- "filename"

files_wanted %>% View()


all_aact_tables$id_information %>% select(nct_id,id_value) %>% distinct() -> docIds

docIds %>% View

found <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("filename", "d"))
v <- 0
for ( d in docIds$id_value ){
  
  if ( d %>% str_length() > 4){
    find <- files_wanted %>% filter(str_detect(filename,d))
  
    if( find %>% nrow() == 1 ){
      # print(paste0("here",d))
      # print(find)
      
      found <- found %>% rbind (find %>% cbind(d))
      v <- v+1
    }
    
  }
  
}

files_wanted %>% anti_join(found %>% select(filename)) -> not_found

colnames(found)[2] <- "id_value"

found %>% inner_join(docIds) %>% View()

#### Used the lines before as partial work. Finished the matching manually. And generated the final file below: 

all_matched_filenames_nctids_gsk_novo_novartis <- read_csv("all_matched_filenames_nctids_gsk_novo_novartis.csv")


all_matched_filenames_nctids_gsk_novo_novartis

all_annotations <- `annotations-gsk` %>% rbind(`Results-novo-novartis`)

all_matched_filenames_nctids_gsk_novo_novartis <- all_matched_filenames_nctids_gsk_novo_novartis %>% 
  mutate( docid.doc = str_replace_all(filename,".pdf","") )

all_annotations %>% inner_join(all_matched_filenames_nctids_gsk_novo_novartis) %>% View()


joined_sg <- all_matched_filenames_nctids_gsk_novo_novartis %>% 
  mutate( docid.doc = str_replace_all(filename,".pdf","") ) %>% 
  anti_join(`annotations-gsk` %>% 
  rbind(`Results-novo-novartis`))


joined_sg %>% select(nct_id)


## /home/suso/ihw/csr/allpdfs/done/nct_id_not_found <- these may be a challenge
