library(readr)
library(dplyr)

get_data <- function(){
  # check if data folder exists
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  files_to_download <- c("eukaryotes.txt", "prokaryotes.txt", "viruses.txt")
  
  for (file in files_to_download) {
    file_path <- file.path("data", file)
    
    if (!file.exists(file_path)) {
      # URL for downloading the files (replace with actual URLs)
      download_url <- switch(file,
                             "eukaryotes.txt" = "https://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/eukaryotes.txt",
                             "prokaryotes.txt" = "https://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/prokaryotes.txt",
                             "viruses.txt" = "https://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/viruses.txt"
      )
      
      # download the file
      download.file(download_url, destfile = file_path, method = "auto")
      cat(paste("Downloaded", file, "\n"))
    } else {
      cat(file, "already exists.\n")
    }
  }
}

get_balanced <- function(df){
  
  # get minimal number of observation among groups
  min_obs <- df %>% na.omit() %>% 
    group_by(Domen) %>%
    summarize(total = n()) %>% 
    pull(total) %>% min()
  
  # slice the groups based on min observations
  mini_df <- df %>% 
    na.omit() %>% 
    group_by(Domen) %>%
    slice_sample(n = min_obs, replace = FALSE) %>%
    ungroup()
  
  return(mini_df)
}

get_yearly_summary <- function(df) {
  df$Year <- lubridate::year(df$Date)
  year_sum <- df %>% group_by(Year, Domen) %>% 
    summarize(Genomes = n(), .groups = "drop") %>%
    #mutate(Genomes = cumsum(Genomes)) %>% 
    pivot_wider(names_from = Domen, values_from = Genomes)
  year_sum[is.na(year_sum)] <- 0
  year_sum <- year_sum %>% 
    mutate(virus = cumsum(virus)) %>% 
    mutate(eukaryote = cumsum(eukaryote)) %>% 
    mutate(prokaryote = cumsum(prokaryote)) %>% 
    pivot_longer(cols = c('virus', 'prokaryote', 'eukaryote'),
                 names_to = 'Domen', values_to = 'Genomes_cumsum')
  
  return(year_sum)
}

get_all_genomes <- function(){
  get_data()
  
  vir <- read_tsv("data/viruses.txt", na = c("-"),
                  col_select = c("Group","Size (Kb)", "Genes", "Release Date", "Status")) %>% 
    filter(Status == "Complete Genome") %>%
    select(-Status) %>% 
    rename(Size = "Size (Kb)", Date = "Release Date") %>% 
    mutate(Size = Size/1000)
  vir <- vir %>% mutate(Domen = rep('virus', nrow(vir)))
  
  euk <- read_tsv("data/eukaryotes.txt", na = c("-"),
                  col_select = c("Group", "Size (Mb)", "Genes", "Release Date", "Status")) %>%
    filter(Status == "Complete Genome") %>% 
    select(-Status) %>% 
    rename(Size = "Size (Mb)", Date = "Release Date")

  euk <- euk %>% 
    mutate(Domen = rep('eukaryote', nrow(euk)))
  
  prok <- read_tsv("data/prokaryotes.txt", na = c("-"),
           col_select = c("Group", "Size (Mb)", "Genes", "Release Date", "Status")) %>%
    filter(Status == "Complete Genome") %>% 
    select(-Status) %>% 
    rename(Size = "Size (Mb)", Date = "Release Date")
  
  prok <- prok %>%
    mutate(Domen = rep('prokaryote', nrow(prok)))
  
  df <- vir %>% rbind(euk) %>% rbind(prok)
}
