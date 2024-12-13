#### Pakker ####
library(httr)
library(rvest)
library(DBI)
library(RMariaDB)
library(tidyverse)
library(logger)

#### Opgave 2.4 - Scrape & SQL med Miljødata ####

#### Starter logger ####
log_appender(appender_file("miljo_log"))

#Headers
headers <- add_headers(
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

## Liste med URL'er til de forskellige sider
urls <- list(
  "HCAB" = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
  "ANHO" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
  "AARH3" = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
  "RISOE" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"
)

#Funktion til at hente data fra en URL
fetch_data <- function(url) {
  log_info(paste("Fetching data from URL:", url))
  
  # Send GET-anmodning for at hente CSRF-token
  get_response <- GET(url, headers)
  log_info(paste("GET request returned status code:", get_response$status_code))
  
  # Kontroller om GET-anmodningen var succesfuld
  if (get_response$status_code == 200) {
    # Parse HTML
    page_content <- content(get_response, as = "text")
    page <- read_html(page_content)
    
    # Udtræk CSRF-token
    csrf_token <- page %>%
      html_element("input[name='__RequestVerificationToken']") %>%
      html_attr("value")
    
    # Fejlhåndtering hvis CSRF-token ikke er udtrukket
    if (is.na(csrf_token)) {
      stop("Failed to extract CSRF-token for URL: ", url)
    } else {
      print(paste("CSRF-token extracted for", url, ":", csrf_token))
    }
  } else {
    stop("Failed to fetch the page. Status code: ", get_response$status_code)
  }
  
  # Brug URL-encode til at sikre korrekt formatering af tokenet
  encoded_csrf_token <- URLencode(csrf_token)
  
  # Forbered POST-anmodningen (payload)
  payload <- list(
    `__RequestVerificationToken` = encoded_csrf_token  # Den korrekte CSRF-token
  )
  
  # Cookies 
  cookies <- set_cookies(
    CookieScriptConsent = '{"bannershown":1,"action":"accept","consenttime":1718103120,"categories":"[\"targeting\",\"functionality\",\"performance\",\"unclassified\"]","key":"c414c3ce-0f5d-45f7-9258-0e3eb062b385"}',
    `__RequestVerificationToken_L0x1ZnRkYXRhL1ByZXNlbnRhdGlvbg2` = "vGZmXU8znQRoPge8zmnonE-UF08FjDOMu2TY_sQ3Zvdz98-8n2j2yrCvva-pnPoBm262cJMWEq85s9eEuvObNwJM5SGksjnmZOfY8Yo8tFE1"
  )
  
  # Send POST-anmodning
  post_url <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/MainTable/", gsub(".*table/", "", url))
  post_response <- POST(
    url = post_url,
    body = payload,
    encode = "form",
    headers = headers,
    cookies = cookies
  )
  
  # Hent data efter POST-anmodning
  if (post_response$status_code == 200) {
    log_info(paste("POST request successful for", url))
    
    # Parse HTML-indholdet fra POST-anmodningen
    post_page <- read_html(content(post_response, as = "text"))
    
    # Ekstrahér tabellen
    table <- post_page %>%
      html_element("table.table-bordered") %>%
      html_table(header = TRUE)
    
  } else {
    log_error(paste("GET request failed for URL:", url, "with status code:", get_response$status_code))
    stop("GET request failed")
  }
}

#### Hent data fra alle URL'er og gem i separate dataframes ####
log_info("Fetching data from all URL's")
data_hcab <- data.frame(fetch_data(urls$HCAB))
data_anho <- data.frame(fetch_data(urls$ANHO))
data_aarh3 <- data.frame(fetch_data(urls$AARH3))
data_risoe <- data.frame(fetch_data(urls$RISOE))
log_info("Data hentet")


#### Opsætning af datatyper, alt undtagen tid til numeric, måletidspunkt til POSIXct ####
## Hcab
data_hcab[,-1] <- lapply(data_hcab[,-1], function(x) as.numeric(gsub(",", ".", x)))
data_hcab$scrapedate <- Sys.time()
data_hcab$Målt..starttid. <- as.POSIXct(data_hcab$Målt..starttid., format="%d-%m-%Y %H:%M")
data_hcab[,c(1,9)] <- data_hcab[,c(1,9)] + 3600 # SQL trækker én time fra, ligegyldigt hvad jeg gør :')

## Anho
data_anho[,-1] <- lapply(data_anho[,-1], function(x) as.numeric(gsub(",", ".", x)))
data_anho$scrapedate <- Sys.time()
data_anho$Målt..starttid. <- as.POSIXct(data_anho$Målt..starttid., format="%d-%m-%Y %H:%M")
data_anho[,c(1,4)] <- data_anho[,c(1,4)] + 3600 # SQL trækker én time fra, ligegyldigt hvad jeg gør :')

## Aarh3
data_aarh3[,-1] <- lapply(data_aarh3[,-1], function(x) as.numeric(gsub(",", ".", x)))
data_aarh3$scrapedate <- Sys.time()
data_aarh3$Målt..starttid. <- as.POSIXct(data_aarh3$Målt..starttid., format="%d-%m-%Y %H:%M")
data_aarh3[,c(1,6)] <- data_aarh3[,c(1,6)] + 3600 # SQL trækker én time fra, ligegyldigt hvad jeg gør :')

## Risoe
data_risoe[,-1] <- lapply(data_risoe[,-1], function(x) as.numeric(gsub(",", ".", x)))
data_risoe$scrapedate <- Sys.time()
data_risoe$Målt..starttid. <- as.POSIXct(data_risoe$Målt..starttid., format="%d-%m-%Y %H:%M")
data_risoe[,c(1,7)] <- data_risoe[,c(1,7)] + 3600 # SQL trækker én time fra, ligegyldigt hvad jeg gør :')

#### Importer til SQL ####
##### Connection #####
log_info("Opretter forbindelse til database")
con <- dbConnect(MariaDB(),
                 host="localhost",
                 db="miljo",
                 user="root",
                 password = Sys.getenv("password"))

##### Henter eksisterende data til anti-join #####
hcab_old <- dbReadTable(con, "hcab")
anho_old <- dbReadTable(con, "anho")
aarh3_old <- dbReadTable(con, "aarh3")
risoe_old <- dbReadTable(con, "risoe")

##### Anti-join for at finde forskellene #####
## Måletidspunktet bliver brugt som unik identifier
hcab_new <- anti_join(data_hcab, hcab_old, by = "Målt..starttid.")
anho_new <- anti_join(data_anho, anho_old, by = "Målt..starttid.")
risoe_new <- anti_join(data_risoe, risoe_old, by = "Målt..starttid.")
aarh3_new <- anti_join(data_aarh3, aarh3_old, by = "Målt..starttid.")

##### WriteTable #####
log_info(paste0("Opdaterer databasen med ", nrow(hcab_new), " nye rækker"))
dbWriteTable(con, "hcab", hcab_new, append = T)
dbWriteTable(con, "anho", anho_new, append = T)
dbWriteTable(con, "risoe", risoe_new, append = T)
dbWriteTable(con, "aarh3", aarh3_new, append = T)

## Disconnect
dbDisconnect(con)
