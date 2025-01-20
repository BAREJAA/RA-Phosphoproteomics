library(shiny)
library(shinyauthr)
library(bslib)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(readr)
library(tibble)
library(plotly)
library(stringr)
library(DT)

# import differential abundance results
ksea_input_liberal <- read_csv("data/ksea_input_liberal_010625.csv") %>% 
  tidyr::unite("phosphosite", 
               c(Gene, Residue.Both),
               sep = "-",
               remove = FALSE) %>% 
  dplyr::mutate(neg_log_p = -log10(p),
                log2FoldChange = log2(FC))


# import kinase-substrate links
Kinase_Substrate_Links_liberal <- read_csv("data/Kinase-Substrate Links_liberal_010625.csv") %>% 
  tidyr::unite("phosphosite", 
               c(Substrate.Gene, Substrate.Mod),
               sep = "-",
               remove = FALSE)

# import kinase-substrate scores
KSEA_Kinase_Scores_with_padj_liberal <- read_csv("data/KSEA_Kinase_Scores_with_padj_liberal_010625.csv")

# import uniprot dataframe (downloaded on 09/13/2024)
uniprot_df <- read_csv("data/uniprot_df.csv")
