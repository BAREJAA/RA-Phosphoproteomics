# Note - 
# import uniprot dataframe (downloaded on 09/13/2024)
uniprot_df <- read_delim(
  "metadata/uniprotkb_reviewed_true_AND_model_organ_2024_09_13.tsv",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>% 
  dplyr::mutate(protein_symbol = word(`Gene Names`)) %>% 
  dplyr::mutate(`Protein names` = str_trim(str_extract(`Protein names`, "[^\\(]+"))) 

#write_csv(uniprot_df, "data/uniprot_df.csv")
# This data frame contains protein symbol and name information
# This is used for to create the text underneath the volcano plot
