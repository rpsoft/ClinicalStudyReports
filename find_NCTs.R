
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

