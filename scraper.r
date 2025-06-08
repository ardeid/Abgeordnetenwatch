library(rvest)
library(tidyverse)

#####################################################
#### Code zum Abgeordnetenwatch html Webscrapen. ####
#####################################################


##### Funktionen fürs Scraping #####
#### Generieren der Übersichstseiten-Links, auf denen die Abgeordneten aufgelistet werden. ####
parlamente <- c("https://www.abgeordnetenwatch.de/bundestag/wahl-2005",
                "https://www.abgeordnetenwatch.de/bundestag/16",
                "https://www.abgeordnetenwatch.de/bundestag/wahl-2009",
                "https://www.abgeordnetenwatch.de/bundestag/17",
                "https://www.abgeordnetenwatch.de/bundestag/18",
                "https://www.abgeordnetenwatch.de/bundestag/19",
                "https://www.abgeordnetenwatch.de/bundestag/20",
                "https://www.abgeordnetenwatch.de/bundestag",
                "https://www.abgeordnetenwatch.de/bundestag/wahl-2013",
                "https://www.abgeordnetenwatch.de/bundestag/wahl-2017",
                "https://www.abgeordnetenwatch.de/bundestag/wahl-2021",
                "https://www.abgeordnetenwatch.de/bundestag/wahl-2025"
                )

fragenscrapen <- TRUE
#Wie oft soll versucht werden die Websites aufzurufen?
maxretry <- 5
### df Initiieren ###
#leeres Df für Parlamente erstellen
df_parl <- data.frame()


df_all <- data.frame()
### Datensatz über die Parlamente, um die Koalition und weitere Infos zu scrapen ####
for (i in 1:length(parlamente)) {
  html <- read_html(parlamente[i])
  
  #Parlarments ID Scrapen
  parlID <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% html_attr("href") %>% str_extract("\\d+")
  
  #Infos zu Koalition und Legislaturperiode
  parlInfo <- html_elements(html, xpath = "//div[@class='parliament-info p-b-large']/dl") |> html_text()
  if (length(parlInfo) == 0) {
    parlInfo <- NA
  }
  
  #Konstruieren eines Falls
  case <- c(parlamente[i], parlID, parlInfo)
  #Einfügen des Falls ins DF
  df_parl <- rbind(df_parl, case)
  
  ### Variablen Benennen
  names(df_parl) <- c("parllink", "parlID", "parlInfo") 
  
  parlbez <- html_elements(html, xpath = "//nav/ul[@class='links links--horizontal']//span[@class='period-identifier']/..") %>% html_text()
  ### Safety, mache Parlamente haben keine normale Parlbez
  if (length(parlbez) == 0) {
    html_parlbez <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% html_attr("href")
    html_parlbez <- read_html(str_c("https://www.abgeordnetenwatch.de", html_parlbez))
    parlbez <- html_elements(html_parlbez, xpath = "//div[@class='page-title l-container']/h1") %>% html_text() %>% str_extract("^.+(?= \\()")
  }
  ##### Übersichtsseiten scrapen ######
  #Links zu den Abgeordneten Übersichtsseiten zusammenfügen
  #Wahlperioden haben 
  parlamente_abg <- if(str_detect(parlbez, pattern = " Wahl ")){
    str_c(parlamente[i], "/kandidierende")
  }else{str_c(parlamente[i], "/abgeordnete")}
  
  ### leeren Vector initiieren
  paths <- vector()
  ### Übersichtsseiten links scapen
  html <- read_html(parlamente_abg)
  ### Auslesen der letzten Profil Übersichtsseite eines Parlaments
  num <- html_elements(html, xpath = "//ul/li[@class='pager__item pager__item--last']/a[@class='pager__link']") %>% html_attr("href") %>% str_split("=") %>% unlist()
  ### Konstruieren der Übersichtsseiten Links
  paths <- append(paths, str_c(parlamente_abg, "?page=", 0:as.numeric(num[2]), sep = ""))
  
  rm(num)
  writeLines(paste("\n", parlbez, "Übersichtsseiten gefunden:", length(paths)))
  
  

  #### Schleife zum Extrahieren von Profil-Links aus den Übersichtsseiten ####
  #leeres Df für Fälle erstellen
  df <- data.frame()

  for(j in 1:length(paths)){
    html <- read_html(paths[j])
    
    #Extrahieren der Abgeordneten Profil-Link-Endung
    profil <- html_elements(html, xpath = "//li/a[text() = 'Zum Profil']") %>% html_attr("href")
    
    #Zusammenfügen der Profil-Links aus dem Basislink und der Profilendung
    profile_links <- str_c("https://www.abgeordnetenwatch.de", profil, sep = "")
    
    #Parlamets ID´s Sammeln um das Scrapen von Fragen zu erleichtern.
    parlID <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% html_attr("href") %>% str_extract("\\d+")
    
    #Parlaments Bezeichnung
    parlbez <- html_elements(html, xpath = "//nav/ul[@class='links links--horizontal']//span[@class='period-identifier']/..") %>% html_text()
    
    ### Safety, mache Parlamente haben keine normale Parlbez
    if (length(parlbez) == 0) {
      html_parlbez <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% html_attr("href")
      html_parlbez <- read_html(str_c("https://www.abgeordnetenwatch.de", html_parlbez))
      parlbez <- html_elements(html_parlbez, xpath = "//div[@class='page-title l-container']/h1") %>% html_text() %>% str_extract("^.+(?= \\()")
    }
    #### Schleife zum Scrapen der Profilseiten ####
    for(k in 1:length(profile_links)){ 
      html <- read_html(profile_links[k])
      
      ###Name des Abgeordneten
      name <- html_elements(html, xpath = "//div[@class='profile-header__overview']/h1") %>% 
        html_text()
      
      ###Geschlecht
      sex <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]")) %>%
        html_text() |> str_trim() |> str_extract(pattern = "\\S*\\b")
      #Umgang mit fehlenden Werten
      sex <- ifelse(length(sex) > 0, sex, NA)
      
      
      ### Informationen über diese Amtszeit
      #Fraktion, Wahlkreis, Wahlkreisergebnis, Wahlliste, Listenposition
      
      #Fraktion
      if (str_detect(parlbez, pattern = " Wahl ")) {
        frak <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='tooltip__text' and contains(text(), 'Angetreten für')]/a")) %>%
          html_text()
      }else{
      frak <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='tooltip__text' and contains(text(), 'Fraktion')]/a")) %>%
        html_text()
      }
      
      #Fraktionslink
      if (str_detect(parlbez, pattern = " Wahl ")) {
        fraklink <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='tooltip__text' and contains(text(), 'Angetreten für')]/a")) %>%
          html_attr("href")
      }else{
      fraklink <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='tooltip__text' and contains(text(), 'Fraktion')]/a")) %>%
        html_attr("href")
      }
      
      #Wahlliste oder Wahlkreis
      gewonnenüber <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='visually-hidden js-tooltip-text']",
                                                        #xpath für die spezifische Information im Info Tooltip
                                                        "//div[@class='field field--inline field--list_string field--mandate-won']")) %>% html_text2() %>% 
        #Info ohne Text davor
        str_extract(pattern = "(?<=\\n).+")
      #Umgang mit fehlenden Werten
      gewonnenüber <- ifelse(length(gewonnenüber) > 0, gewonnenüber, NA)
      
      #Wahlkreis
      wkreis <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='visually-hidden js-tooltip-text']",
                                                  #xpath für die spezifische Information im Info Tooltip
                                                  "//div[@class='field field--inline field--entity_reference field--constituency']")) %>% html_text2() %>% 
        #Info ohne Text davor
        str_extract(pattern = "(?<=\\n).+")
      #Umgang mit fehlenden Werten
      wkreis <- ifelse(length(wkreis) > 0, wkreis, NA)
      
      wkreisP <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='visually-hidden js-tooltip-text']",
                                                   #xpath für die spezifische Information im Info Tooltip
                                                   "//div[@class='field field--inline field--decimal field--constituency-result']")) %>% html_text2() %>% 
        #Info ohne Text davor
        str_extract(pattern = "(?<=\\n)\\d+,\\d+") %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
      #Umgang mit fehlenden Werten
      wkreisP <- ifelse(length(wkreisP) > 0, wkreisP, NA)
      
      #Wahlliste
      wliste <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='visually-hidden js-tooltip-text']",
                                                  #xpath für die spezifische Information im Info Tooltip
                                                  "//div[@class='field field--inline field--entity_reference field--ctoral-list']")) %>% html_text2() %>% 
        #Info ohne Text davor
        str_extract(pattern = "(?<=\\n).+")
      #Umgang mit fehlenden Werten
      wliste <- ifelse(length(wliste) > 0, wliste, NA)
      
      #Listenposition
        listpos <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/../..//div[@class='visually-hidden js-tooltip-text']",
                                                   #xpath für die spezifische Information im Info Tooltip
                                                   "//div[@class='field field--inline field--integer field--st-position']")) %>% html_text2() %>% 
        #Info ohne Text davor
        str_extract(pattern = "(?<=\\n).+")

      #Umgang mit fehlenden Werten
      listpos <- ifelse(length(listpos) > 0, listpos, NA)
      
      #Umgang mit Nachrückern
      if (!is.na(gewonnenüber)) {
        if (gewonnenüber == "Nachgerückt") {
          wkreis <- NA
          wkreisP <- NA
          wliste <- NA
          listpos <- NA
        }}
      
      ### Jahre die die Person dabei war
      qittr_years <- html_elements(html, xpath = str_c("//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and contains(text(),'", parlbez,"')]/div[@class='accordion__subline']")) |> html_text() %>% 
        str_extract("\\d... - ....")
      
      if (length(qittr_years) < 1){
        qittr_years <- NA
      }
      
      ### Nicht die volle Amtszeit
      qittr <- ifelse(!is.na(qittr_years), TRUE, FALSE)
      
      ###Geburtsjahr des Abgeordneten
      geb <- html_text(html_elements(html, xpath = "//div[@class='field field--inline field--integer field--year-of-birth']")) %>% 
        str_split(" ") %>% 
        unlist()
      #NA Filter
      geb <- ifelse(length(geb) > 0, geb[2], NA)
      
      
      
      ### Parlamentarische Ämter des Abgeordneten
      aemter <- html_elements(html, xpath = "//div/h3[@class='accordion__title']/div[@class='accordion__title__text' and starts-with(text(), ' Abg')]") %>% 
        html_text() %>% str_flatten(collapse = "#") %>% str_remove_all("Abgeordnet\\S*")
      
      
      ###Berufliche Qualifikation
      edu <- html_text(html_elements(html, xpath = "//div[@class='field field--inline field--string field--ucation']")) %>% 
        str_remove("Berufliche Qualifikation") %>% 
        str_trim() 
      #Failsave
      edu <- ifelse(length(edu) > 0, edu, NA)
      
      #Abgeordnetenwatch Abgeordneten ID
      id <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% 
        html_attr("href") %>% str_extract("\\d+")
      
      #Fragen-Antworten Link
      flink <- html_elements(html, xpath = "//div[@class='tabs-mobile-select']//a/span[text()='Fragen und Antworten']/..") %>% 
        html_attr("href") %>% str_c("https://www.abgeordnetenwatch.de",., "?parliament_period=", parlID) %>% unique()
      
      flink <- ifelse(length(flink) > 0, flink, NA)
      
      ##Ausschussmitgliedschaften
      #Ausschuss Link erstellen
      aus_link <- html_elements(html, xpath = "//div[@class='tabs-mobile-select']//a/span[text()='Ausschuss-Mitgliedschaften']/..") %>% 
        html_attr("href") %>% str_c("https://www.abgeordnetenwatch.de",., "?parliament_period=", parlID) %>% unique() 
      
      #Check, ob Abgeordneter in Ausschüssen sitzt.
      if (length(aus_link)!=0 & !str_detect(parlbez, pattern = " Wahl ")) {
        
        #HTML Anfragen  
        html_aus <- read_html(aus_link)
        #Ausschüsse Scrapen
        aussch <- html_elements(html_aus, xpath = "//div[@class='l-grid l-grid--medium-gutter']/article[@class='tile tile--vote l-grid__col l-grid__col--1/2@xs l-grid__col--1/4@lg']") %>%
          html_text() %>% str_flatten(collapse = "%>%")
        #memory Management
        rm(html_aus)
        }else{
            aussch <- NA
          }
      aussch <- ifelse(str_length(aussch) > 0, aussch, NA)
      #Konstruieren eines Falls
      case <- c(profile_links[k], parlID, parlbez, id, name, sex, geb, frak, fraklink, aemter, qittr, qittr_years, gewonnenüber, wkreis, wkreisP, wliste, listpos, aussch, edu, flink, Sys.time())
      #Einfügen des Falls ins DF
      df <- rbind(df, case)
      
      #Progress Bar
      cat("\rÜbersichtsseiten: ") 
      cat("(",j,"/", length(paths),") Profil: (",k, "/", length(profile_links), ")   ", sep = " ")
    }
  }
  
  #Memory Management
  rm(parlID, parlbez, id, name, sex, geb, frak, fraklink, aemter, qittr, qittr_years, gewonnenüber, wkreis, wkreisP, wliste, listpos, aussch, edu, flink, aus_link,
     parlamente_abg, profil, case)
  
  ### 
  names(df) <- c("proflink", "parlID", "parlbez", "AbgeordnetenID", "name", "sex", "geb", "frak", "fraklink", "aemter", "teilamtszeit", "teileamtszeit", 
                 "wkreisOwliste", "wkreis", "wkreisP", "wliste", "listpos", "ausschüsse", "bildung", "fragenlink", "systime")
  
  ##### Fragen Scrapen
  if (fragenscrapen) {
    
    ### Loop zum Scrapen der Fragen-Übersichts-Seiten. ####
    #Diese beinhalten je bis zu sechs Fragen und werden gebraucht, um die Links zu den einzelnen Fragen zu scrapen.
    
    qpages <- vector()
    
    message("\r\n", "Scraping Profile Pages", "\n")
    #
    for (j in 1:length(df$fragenlink)) {
      #Überspringen von Abgeordneten ohne Fragenlink
      if (is.na(df$fragenlink[j])) {next}
      
      #For loop mit nested trycatch, um mit nicht erreichbaren URLS umzugehen
      for (attempt in 1:maxretry) {
        tryCatch({
          #Link konstruieren:
          link <- str_c(df$fragenlink[j], "&topics=All&answered=All", sep = "")
          #Website Ansteuern
          html <- read_html(link)
          
          #Check ob mehr als eine Fragen Seite besteht (Dann gibt es unten auf der Seite ein Seiten Menü was gescraped wird)
          if (length(html_elements(html, xpath = "//li[@class='pager__item is-active']")) > 0){
            #Letzte Frageseite finden, um alle zwischen der ersten und der letzten einzubeziehen
            num <- html_elements(html, xpath = "//ul/li[@class='pager__item pager__item--last']/a[@class='pager__link']") %>% 
              html_attr("href") %>% 
              str_split("=") %>% 
              unlist()
            ##Anhängen der Frageseitenlinks an die Liste der Frageseitenlinks
            qpages <- append(qpages, c(link, str_c(link, "&page=", 1:as.numeric(last(num)), sep = "")))
          } else {
            qpages <- append(qpages, link)
          }
          #Progress Bar
          cat("\r Scraping Question Pages: ") 
          cat(df$name[j] ," (",j,"/", nrow(df),")",str_dup(" ", times = 30), sep = "")
          break
        },
        error = function(e){
          Sys.sleep(10)
          if (attempt == maxretry-1) {message(cat("Can´t reach: ", qpages[j]),"\n reason: ", e)}
        },
        #Aus der Trycatch schleife herausspringen
        if (attempt == maxretry) {
          message(cat("Cant reach: ",  link, "\n"))
          break}
        )
      }
    }
    
    message("\n", "Now Scraping Questions", "\n")
    message("Question Pages Found ", length(qpages))
    
    df_q <- data.frame()
    #Loop der Links zu Fragenseiten sammelt
    #
    for(j in 1:length(qpages)){
      #For loop mit nested trycatch, um mit nicht erreichbaren URLS umzugehen
      for (attempt in 1:maxretry) {
        tryCatch({
          html <- read_html(qpages[j])
          
          #Fragelinks Scrapen. Ist Zeitaufwendiger aber weniger Fehleranfällig
          qlinks <- html_elements(html, xpath = "//article/div/div/div/div/div/a") %>% html_attr("href") %>% str_c("https://www.abgeordnetenwatch.de", ., sep = "")
          
          #Wenn ein Profil keine 
          if (length(qlinks) < 1) {break}
          #Abgeordneten ID (Bleibt für jede qpages Seite gleich)
          id <- html_elements(html, xpath = "//a[@class='use-ajax button button--secondary']") %>% html_attr("href") %>% str_extract("\\d+")
          
          #Loop der die Fragenseiten Scraped
          break
        },
        #Delay, damit die Verbindung beim nächsten mal hoffentlich besser ist
        error = function(e){
          Sys.sleep(10)
          if (attempt == maxretry-1) {message(cat("Can´t reach: ", qpages[j]),"\n reason: ", e)}
        },
        #Aus der Trycatch schleife herausspringen
        if (attempt == maxretry) {
          message(cat("Couldn´t reach: ",  qpages[j]))
          break}
        )
      }  
      #Wenn ein Profil keine Fragen hat muss die Seite übersprungen werden
      if (length(qlinks) < 1) {next}
      
      for(k in 1:length(qlinks)){
        #For loop mit nested trycatch, um mit nicht erreichbaren URLS umzugehen
#        if (length(qlinks[k]==0)) {
          
#        }
        
        for (attempt2 in 1:maxretry) {
          tryCatch({
            html <- read_html(qlinks[k])
            #Reagiert (Beantwortet ist Reagiert - Fragen die mit Standardantworten Beantwortet wurden) 
            reag <-  html_elements(html, xpath = "//div[@class='l-grid__col l-grid__col--2/3@lg']/article//span[@class='tile__politician__label']/strong[text()='Antwort']/..") %>% 
              html_text() %>% str_detect("ausstehend", negate = TRUE)
            #Umgehen mit Sonderfällen
            if (length(reag)==0) {reag <- FALSE}
            
            
            #Test ob mehrere Antworten auf eine Frage gegeben wurden
            multia <- ifelse(length(reag) > 1 ,TRUE, FALSE)
            #Fragende Person; Aus der Fragenüberschrift auf der Frageseite
            qpers <- html_elements(html, xpath = "//div[@class='tile__politician__info']/span[@class='tile__politician__label']/span/..") %>% html_text() %>% 
              str_extract("(?<=von).+") %>% str_extract(".+(?=•)") %>% str_trim()
            
            #Fragetext
            qtext <- html_elements(html, xpath = "//div[@class='tile__question-text']/div[@class='field field--text_long field--text']") %>% 
              html_text2()
            #Manchmal gibt es keinen Fragetext in der Textbox sondern die ganze Frage steht in der Überschrift
            if (length(qtext) < 1) {qtext <- NA}
            
            qhead <- html_elements(html, xpath = "//h1[@class='tile__question__teaser']") %>% html_text2()
            
            #Fragedatum
            qdate <- html_elements(html, xpath = "//div[@class='tile__politician__info']/span[@class='tile__politician__label']/span") %>% html_text()
            
            
            #Antworttext; if-else um mit nicht beantworteten Fragen umgehen zu können.
            if (isTRUE(reag[1])) {
              atext <- html_elements(html, xpath = "//div[@class='question-answer__text']") %>% 
                html_text2()
            } else{atext <- NA}
            #Umgang mit mehreren Antworttexten
            if (length(atext)>1) {atext <- str_flatten(atext, collapse = " %>% ")}
            #Antwortdatum; if-else um mit nicht beantworteten Fragen umgehen zu können.
            if (isTRUE(reag[1])) {
              adate <- html_elements(html, xpath = "//div[@class='tile__politician__info']/span[@class='tile__politician__suffix']/span[not(@class='tile__politician__party')]") %>% 
                html_attr("content")
            } else{adate <- NA}
            #Umgang mit mehreren Antwortdaten
            if (length(adate)>1) {adate <- str_flatten(adate, collapse = " %>% ")}
            
            #Fragen Tags (Thematische Einordnung)
            tags <- html_elements(html, xpath = "//div[@class='question-body']//div[@class='tile__question-meta']/ul[@class='list-inline']/li/a") %>% 
              html_text() %>% str_flatten(collapse = " %>% ")
            
            
            #Text der Anmerkung, zur Prüfung, ob wirklich alle Anmerkungen wegen Vorgefertigten Antworten gemacht wurden.
            mod_ann_text <- html_elements(html, xpath = "//div[@data-component-id='aw:mod_annotation']//div[@class='tile__question-text']") %>% html_text2() 
            
            #Mit Leeren Anmerkungen umgehen
            mod_ann_text <- mod_ann_text[!str_length(mod_ann_text) == 0]
            if (length(mod_ann_text) > 1) {
              #zusammenfügen von mehreren Antworten zu einem String
              mod_ann_text <- mod_ann_text %>% str_flatten(collapse = " %>% ")
              
              #Kodieren dass mehrere Anmerkungen bestehen
              multi_mod_ann <- TRUE
            } else{
              #Umgang mit fehlendem Text
              mod_ann_text <- ifelse(length(mod_ann_text) > 0, mod_ann_text, NA)
              
              #Kodieren, dass nur eine Antwort besteht
              multi_mod_ann <- FALSE
            }
            
            #Kodieren, ob es eine Annmerkung der Moderation gibt
            mod_ann <- !is.na(mod_ann_text)
            
            
            #Case Bauen
            case <- c(qlinks[k], id, reag[1], qpers, qdate, qhead, qtext, adate, multia, atext, tags, mod_ann, mod_ann_text, multi_mod_ann, Sys.time())
            #Einfügen des Falls ins DF
            df_q <- rbind(df_q, case)
            
            #Progress Bar
            cat("\r Scraping Questions Pages: ") 
            cat("(",j,"/", length(qpages),") Question: (", k, "/", length(qlinks), ") ", sep = "")
            break
            },
            #Was soll passieren, wenn ein Error auftritt? 10sek warten.
            error = function(e){
              Sys.sleep(10)
              if (attempt2 == maxretry-1) {message(cat("Can´t reach: ", qlinks[k]),"\n reason: ", e)}
            },
            #Aus der Trycatch schleife herausspringen
            if (attempt2 == maxretry) {
              message(cat("Couldn´t reach: ",  qlinks[k]))
              break})
          
        }
      }
    }
    rm(case, qlinks, id, reag, qpers, qdate, qhead, qtext, adate, multia, atext, tags, qpages, 
       k, j, link, html, num, mod_ann_text, mod_ann, multi_mod_ann, attempt, attempt2)
    names(df_q) <- c("URL", "AbgeordnetenID", "reagiert", "person", "fragedate", "frageteaser", "fragetext", "antworttime", "mehrereantworten", "antworttext", "tags", "anmerkung", "anmerkung_text", "mehrereanmerkungen", "Uhrzeit")
    
    df <- full_join(df, df_q, by = "AbgeordnetenID", suffix = c(".x", ".y"))
    
  }
  if (length(df_all)==0) {
    df_all <- df 
    next
    }
  if (length(df_all) != 0) {
    df_all <- rbind(df_all, df)
    }
}


saveRDS(df_all, file = "BT.rds")