#### Pakker ####
library(rvest)
library(httr)
library(tidyverse)

#### Load clean dataset ####
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA4")
tesla_clean <- readRDS("tesla_clean.rds")

tesla_links <- as.data.frame(tesla_clean$link)
colnames(tesla_links) <- "link"

## Tom df
carlink.df <- data.frame()

## Loop script
for (i in 1:nrow(tesla_links)) {
  startlink <- paste(tesla_links$link[i])
  rawres.link <- tryCatch({
    GET(url = startlink)  
  }, error = function(cond) {
    NULL
  })
  
  if (is.null(rawres.link) || rawres.link$status_code != 200) {
    next
  }
  
  rawcontent.link <- content(rawres.link, as = "text") 
  pagecontenthtml.link <- read_html(rawcontent.link)
  
  carlist.link <- pagecontenthtml.link %>% html_nodes("article")
  
  if (length(carlist.link) != 1) {
    carlink.temp.df <- data.frame(
      name = "sold",
      address = "sold",
      CVR = "sold",
      startlink,
      Sys.time(),
      stringsAsFactors = FALSE
    )
    carlink.df <- rbind(carlink.df, carlink.temp.df)
    next
  }
  
  for (car in carlist.link) {
    name <- car %>% html_element("h2[class='bas-MuiTypography-root bas-MuiTypography-h3']") %>% html_text()
    address <- car %>% html_element(".bas-MuiTypography-root.bas-MuiLink-root.bas-MuiLink-underlineNone.bas-MuiSellerInfoComponent-address.bas-MuiTypography-colorPrimary") %>% html_text()
    CVR <- car %>% html_element(".bas-MuiSellerInfoComponent-cvr") %>% html_text()
    
    carlink.temp.df <- data.frame(
      name, address, CVR, startlink, Sys.time(),
      stringsAsFactors = FALSE
    )
    
    carlink.df <- rbind(carlink.df, carlink.temp.df)
    Sys.sleep(runif(1, min = 1, max = 5))
    
    cat("Er i gang med URL nr: ", i, "med linket: ", startlink, "\n")
    cat("Antal biler fundet på siden:", length(carlist.link), "\n")
  }
}
