#### Pakker ####
library(rvest)
library(httr)
library(tidyverse)

#### Bilbasen Scrape ####

## Headers ##
headers <- add_headers(
  `:authority` = "www.bilbasen.dk",
  `:method` = "GET",
  `:path` = "/brugt/bil/tesla/model_3?includeengroscvr=true&includeleasing=false",
  `:scheme` = "https",
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

## DF Header
carheader <- c("price","property","model","details","desciption","location","link","carid","scrapedate")

## URL
starturl <- "https://www.bilbasen.dk/brugt/bil/tesla/model_3?includeengroscvr=true&includeleasing=false"
nexturl <- "https://www.bilbasen.dk/brugt/bil/tesla/model_3?includeengroscvr=true&includeleasing=false&page="

# tag liste
ptag=".Listing_price__6B3kE"
proptag <-".Listing_properties___ptWv" 
mmtag <- "[class^='Listing_makeModel']"
dettag <- "[class^='Listing_detail']"
dettagitem <- "[class^='ListingDetails_listItem']"
desctag <- "[class^='Listing_description']"
loctag <- "[class^='Listing_location']"


# dataframe til opsamling
carheader <- c("price","property","model","detailitems","desciption","location","link","carid","scrapedate")
colldf <- as.data.frame(matrix(data=NA,nrow=0,ncol=9))
colnames(colldf)=carheader

########## Loop ##########
for (i in 1:19) { # Antal sider, du vil iterere over (her 19)
  # Generer URL for den aktuelle side
  url <- ifelse(i == 1, starturl, paste0(nexturl, i))
  
  # Send GET-request til den aktuelle URL
  rawres <- GET(url = url, headers)
  
  # Kontroller statuskode
  if (rawres$status_code != 200) {
    message(paste("Failed to fetch page:", i, "with status:", rawres$status_code))
    next # Gå videre til næste iteration, hvis der opstod en fejl
  }
  
  rawcontent <- httr::content(rawres, as = "text")
  
  # Transformér tekst til html-nodes
  page <- read_html(rawcontent)
  
  # Ekstrahér bil-elementer fra siden
  carlist <- page %>% html_elements("article")
  
  # Loop gennem hver bil på siden
  for (car in carlist) {
    tryCatch({
      price <- car %>% html_element(ptag) %>% html_text()
      property <- car %>% html_element(proptag) %>% html_text()
      model <- car %>% html_element(mmtag) %>% html_text()
      details <- car %>% html_elements(dettagitem) %>% html_text() %>% paste0(collapse = "_")
      description <- car %>% html_elements(desctag) %>% html_text()
      location <- car %>% html_elements(loctag) %>% html_text()
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      
      # Lav en midlertidig data.frame for den aktuelle bil
      tmpdf <- data.frame(price, property, model, details, description, location, link, carid, Sys.time(), stringsAsFactors = FALSE)
      colnames(tmpdf) <- colnames(colldf) # Sørg for, at kolonnenavne stemmer overens
      
      # Tilføj til det samlede data.frame
      colldf <- rbind(colldf, tmpdf)
    },
    error = function(cond) {
      message(paste("Error processing car:", cond))
    })
  }
  
  # Meddelelse om færdig side
  message(paste("Finished scraping page", i))
  
  # Introducer en pause for at undgå blokering
  Sys.sleep(runif(1, 2, 5)) # Pauser mellem 2 og 5 sekunder
}