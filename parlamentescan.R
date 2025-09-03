library(tidyverse)
library(rvest)

### Alle Parlamentslinks scrapen
#html einlesen
html <- read_html("https://www.abgeordnetenwatch.de/parlamente-archiv")

#links scrapen
parlamente <- html_elements(html, xpath = "//main//a") %>% html_attr("href")

#links vervollständigen
parlamente <- str_c("https://www.abgeordnetenwatch.de", parlamente)

#NA Entfernen
parlamente <- parlamente[!is.na(parlamente)]

#speichern
saveRDS(parlamente, file = "parlamente.rds")

