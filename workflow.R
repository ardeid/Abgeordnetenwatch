### Funktionen und Packages laden
library(tidyverse)
### Gescheiterter Versuch mit future mehrere Parlamente gleichzeitig zu scrapen.
#pacman::p_load(future, furrr)



setwd("C:/Users/arned/Documents/03 R/Projekts/Abgeordnetenwatch")


#scraper.r beinhaltet die aw_scraper() Funktion
source("scraper.r")
parlamente <- tryCatch(
  expr = {readRDS("parlamente.rds")},
  error = function(e){
           source("parlamentescan.r")
           return(parlamente)
           }
         )
setwd("C:/Users/arned/Documents/03 R/Projekts/Abgeordnetenwatch/parl")
files <- list.files()
if (any(str_detect(files, "parl_"))) {
  done <- files[str_detect(files, ".rds") & str_detect(files, "parl_")] %>% str_remove("parl_") %>% str_remove(".rds")
  
  done <- str_replace(done, "#", "/")
  
  parlamente <- parlamente[!str_detect(parlamente, str_flatten(done, collapse = "|"))]
}


### Using map and sample to scrape the wanted amount of parlaments at once.
map(sample(parlamente, 1), aw_scraper, save = TRUE, return = FALSE, fragenscrapen = TRUE)



### Testing the datasets
#setting the Folder the datasets are in
setwd("C:/Users/arned/Documents/03 R/Projekts/Abgeordnetenwatch/parl")
#getting the file names
files <- list.files()
#initiating a results df
results <- data.frame()

#Testing loop
for (i in 1:length(files)) {
  df <- readRDS(file = files[i])
  
  #getting the parlarment name
  parlname <- unique(df$parlbez) 
  #Testing for errors scraping the questions
  quest_error <- any(!str_detect(df$Uhrzeit, "\\d{10}\\.\\d?"), na.rm = TRUE)
  
  #Testing for errors scraping the profiles
  prof_error <- any(!str_detect(df$systime, "\\d{10}\\.\\d?"), na.rm = TRUE) 
  
  filename <- files[i]
  
  #constructing the cases in the results df
  case <- c(parlname, quest_error, prof_error, filename)
  results <- rbind(results, case)
  #memory management
  rm(case, parlname, quest_error, prof_error, filename)
  
  ### End sequenz
  if (i == length(files)) {
    names(results) <- c("parlbez", "quest_error", "prof_error", "filename")
    #changeing classes
    results$quest_error <- as.logical(results$quest_error)
    results$prof_error <- as.logical(results$prof_error)
    #outputting results
    if (any(results$quest_error)) {
      cat("Question Errors in Datasets:", 
             results$parlbez[results$quest_error == TRUE], sep = "\n")
    }else{cat("Question Errors in Datasets: \n-")}
    if (any(results$prof_error)) {
      cat("\n","Profile Errors in Datasets:", 
             results$parlbez[results$prof_error == TRUE], sep = "\n")
    }else{cat("Profile Errors in Datasets: \n-")}
  }
  #Memory Management
  rm(i, df)
}
#Memory Management 
rm(files)

error_dfs <- rbind(results[results$quest_error,], results[results$prof_error,])
### i = No. df that you want to investigate
i <- 1
### Reading in the Error Datasets
error_df <- readRDS(paste0(getwd(), "/",error_dfs[i]))
errors <- data.frame()
for (i in 1:nrow(error_dfs)) {
  
  if (error_dfs[i,]$quest_error) {
    error <- error_df %>% filter(!str_detect(error_df$Uhrzeit, "\\d{10}\\.\\d?"), na.rm = TRUE)
  }
  if(error_dfs[i,]$prof_error){
    error <- error_df %>% filter(!str_detect(error_df$Uhrzeit, "\\d{10}\\.\\d?"), na.rm = TRUE)
  }
}
view(errors)




