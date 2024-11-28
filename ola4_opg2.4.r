library(httr)
library(rvest)

####Opgave 2.4 - Scrape & SQL med Miljødata ####

#Headers
headers <- add_headers(
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

#Liste med URL'er til de forskellige sider
urls <- list(
  "HCAB" = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
  "ANHO" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
  "AARH3" = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
  "RISOE" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"
)

#Funktion til at hente data fra en URL
fetch_data <- function(url) {
  # Send GET-anmodning for at hente CSRF-token
  get_response <- GET(url, headers)
  
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
    print(paste("POST request successful for", url))
    
    # Parse HTML-indholdet fra POST-anmodningen
    post_page <- read_html(content(post_response, as = "text"))
    
    # Ekstrahér tabellen
    table <- post_page %>%
      html_element("table.table-bordered") %>%
      html_table(header = TRUE)
    
  } else {
    stop("Failed to fetch data with POST request for URL:", url, "Status code: ", post_response$status_code)
  }
}

#Hent data fra alle URL'er og gem i separate dataframes
data_hcab <- fetch_data(urls$HCAB)
data_anho <- fetch_data(urls$ANHO)
data_aarh3 <- fetch_data(urls$AARH3)
data_risoe <- fetch_data(urls$RISOE)


# Gem som RDS fil 1 gang
saveRDS(data_hcab, "Documents/Dataanlyse/1_semester/R/OLA4/HCAB_table.rds")
saveRDS(data_anho, "Documents/Dataanlyse/1_semester/R/OLA4/ANHO_table.rds")
saveRDS(data_aarh3, "Documents/Dataanlyse/1_semester/R/OLA4/AARH3_table.rds")
saveRDS(data_risoe, "Documents/Dataanlyse/1_semester/R/OLA4/RISOE_table.rds")

# Gem som RDS fil 2 gang
saveRDS(data_hcab, "Documents/Dataanlyse/1_semester/R/OLA4/HCAB_table_v2.rds")
saveRDS(data_anho, "Documents/Dataanlyse/1_semester/R/OLA4/ANHO_table_v2.rds")
saveRDS(data_aarh3, "Documents/Dataanlyse/1_semester/R/OLA4/AARH3_table_v2.rds")
saveRDS(data_risoe, "Documents/Dataanlyse/1_semester/R/OLA4/RISOE_table_v2.rds")


#Hent RDS 1 scrape
data_hcab_1_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/HCAB_table.rds")
data_anho_1_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/ANHO_table.rds")
data_aarh3_1_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/AARH3_table.rds")
data_risoe_1_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/RISOE_table.rds")

#Hent RDS 2 scrape
data_hcab_2_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/HCAB_table_v2.rds")
data_anho_2_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/ANHO_table_v2.rds")
data_aarh3_2_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/AARH3_table_v2.rds")
data_risoe_2_scrape <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/RISOE_table_v2.rds")


# Funktion til at hente miljødata 
#Skal lave en funktion hvor jeg sætter linket ind og så scraper funktionend på ny
data_hcab1 <-fetch_data("https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB")


#få over i sql som database

#Hent librarys 
install.packages("DBI")
install.packages("RMariaDB") 
library(DBI)
library(RMariaDB)
library(keyring)

key_set("my_password", "MySQL")
password <- key_get("my_password", "MySQL")

#Lav en connection 
con=dbConnect(MariaDB(),
               db="miljodata",
               host="localhost",
               port=3306,
               user="root",
               password=password
)

#Vi bruger dbWriteTable til at skrive en dataframe og få lavet den som en tabel
dbWriteTable(con,"hcab",data_hcab_1_scrape, overwrite = T)
dbWriteTable(con, "anho", data_anho_1_scrape, overwrite = TRUE)
dbWriteTable(con, "aarh3", data_aarh3_1_scrape, overwrite = TRUE)
dbWriteTable(con, "risoe", data_risoe_1_scrape, overwrite = TRUE)

#hcab, risoe, anho og aarh3

# Funktion til at tilføje ny data til SQL database for 'hcab', 'anho', 'risoe' og 'aarh3'
update_data <- function(con, table_name, new_data) {
  # 1. Hent det seneste tidsstempel fra databasen
  latest_timestamp <- dbGetQuery(con, paste0("SELECT MAX(`Målt (starttid)`) AS max_time FROM ", table_name, ";"))
  
  # Hvis der ikke findes data i databasen (første kørsel)
  if (is.na(latest_timestamp$max_time[1])) {
    print("Ingen eksisterende data i databasen. Indsætter hele datasættet.")
    dbWriteTable(con, table_name, new_data, append = TRUE, row.names = FALSE)
    return()
  }
  
  # Konverter seneste tidsstempel
  latest_timestamp <- as.POSIXct(latest_timestamp$max_time[1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  print(paste("Seneste tidsstempel i databasen:", latest_timestamp))
  
  # 2. Konverter tidsstempler i det nye datasæt
  new_data$`Målt (starttid)` <- as.POSIXct(new_data$`Målt (starttid)`, format = "%d-%m-%Y %H:%M", tz = "UTC")
  
  # 3. Filtrér kun rækker med tidsstempler nyere end det seneste
  new_rows <- new_data[new_data$`Målt (starttid)` > latest_timestamp, ]
  print(paste("Antal nye rækker fundet:", nrow(new_rows)))
  
  # 4. Tilføj kun nye rækker til databasen
  if (nrow(new_rows) > 0) {
    dbWriteTable(con, table_name, new_rows, append = TRUE, row.names = FALSE)
    print(paste(nrow(new_rows), "nye rækker tilføjet til tabellen", table_name))
  } else {
    print("Ingen nye rækker at tilføje.")
  }
}

# Kør funktionen for at få data ind i databasen for 'hcab', 'anho', 'risoe', og 'aarh3'
update_data(con, "hcab", data_hcab_2_scrape)
update_data(con, "anho", data_anho_2_scrape)
update_data(con, "aarh3", data_aarh3_2_scrape)
update_data(con, "risoe", data_risoe_2_scrape)