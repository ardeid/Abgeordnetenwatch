#------------------------------------
# This function searches on Wikipedia for the missing birth year of Politicians
#------------------------------------

# loading packages
if (!require(tidyselect)) install.packages("tidyselect")
library(tidyselect)
if (!require(rvest)) install.packages("rvest")
library(rvest)
if (!require(readr)) install.packages("readr")
library(readr)


############# Alter Nachtragen ################
age_scraper <- function(df, verbose = FALSE) {
  # Fälle raussuchen
  if (condition) {
    tmp <- df |>
      filter(is.na(df$geb)) %>%
      select(name, AbgeordnetenID)

    #### Wikipedia Scrapen ####

    # df initieren
    df_alter <- data.frame()
    error_csv <- data.frame()

    # Suchfeld einstellen:
    search <- html_form(read_html("https://www.wikipedia.de/"))

    ### Scrapeing loop
    for (i in 1:length(tmp$name)) {
      # Absicherung
      tryCatch(
        {
          # Suchanfrage konstruieren
          search_form <- html_form_set(search[[1]], q = tmp$name[i])

          # Suchanfrage abschicken
          resp <- html_form_submit(search_form)

          # Respons in HTML umformen, um mit den HTML Werkzeugen zu arbeiten.
          html <- read_html(resp)

          # Feld finden, in dem das Alter angegeben wird
          info <- html %>%
            html_elements(xpath = "//div[@class='mw-content-ltr mw-parser-output']/p") %>%
            html_text() %>%
            str_extract(pattern = "\\(\\*[^()]*\\)")

          ### Im DF df_alter speichern
          # Konstruieren eines Falls
          case <- c(tmp$AbgeordnetenID[i], tmp$name[i], info[1])
          # Einfügen des Falls ins DF
          df_alter <- rbind(df_alter, case)
        },

        # Defining error function
        error = function(e) {
          if (verbose) {
            message("Not Found: ", tmp$name[i])
          }
          case <- c(AbgeordnetenID[i], tmp$name[i])
          error_csv <- -rbind(error_csv, case)
        }
      )
    }

    # colnames ins df einfügen
    names(df_alter) <- c("Name", "geb_lang", "AbgeordnetenID")

    # Geburtsjahre aus den gescrapten strings extrahieren
    df_alter <- df_alter |> mutate(geb = str_extract(geb_lang, pattern = "\\d{4}"))

    # Na´s Rausfiltern
    df_alter <- df_alter %>% filter(!is.na(df_alter$geb))

    # Neue Daten in Haupt df einpflegen
    for (i in 1:length(df_alter$geb)) {
      df$geb[df$AbgeordnetenID == df_alter$AbgeordnetenID[i]] <- df_alter$geb[i]
    }


    # Weiterhin fehlende Fälle finden
    tmp <- df |>
      filter(is.na(df$geb)) %>%
      select(name, AbgeordnetenID)

    # CSV Erstellen und von Hand nachtragen
    # write.csv(tmp, file = file.path("rds", "age_nachtrag.csv"), row.names = FALSE)

    # Nachgetragenes Einladen
    age_nachtrag <- read_delim("rds/age_nachtrag.csv",
      delim = ";", escape_double = FALSE, trim_ws = TRUE
    )


    # Nachgetragenes einarbeiten
    for (i in 1:length(age_nachtrag$geb)) {
      df$geb[df$AbgeordnetenID == age_nachtrag$abgeordnetenID[i]] <- age_nachtrag$geb[i]
    }

    # Prüfen, ob alle leerstellen behoben sind
    any(is.na(df$geb))

    df$geb <- as.numeric(df$geb)


    ### Mempry Management
    # rm()

    #---------
    # Saving
    saveRDS(df, file = file.path(paste0()))
  }
}
