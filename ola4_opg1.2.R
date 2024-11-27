## Pakker
library(tidyverse)
library(mapDK)

#### Tesla DF cleaning ####
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA4")
tesla_df <- readRDS("tesla_df.rds")

## Stavefejl i description
colnames(tesla_df)[5] <- "description"

#### Engros/CVR ####
## 6 entries i alt er enten engros eller cvr, så de fjerne fra dataset
tesla_df <- tesla_df[-c(377, 435, 457, 474, 489, 490),]
rownames(tesla_df) <- NULL

#### Rækkevidde ####
rækkekm <- str_extract(tesla_df$detailitems, pattern = "[0-9]+\\s*km rækkevidde")
tesla_df$rækkevide_km <- rækkekm
tesla_df$rækkevide_km <- gsub("\\s*km\\s*rækkevidde", "", tesla_df$rækkevide_km)
tesla_df$rækkevide_km <- as.numeric(tesla_df$rækkevide_km)

#### Måned og årstal ####
rækkeår <- str_extract(tesla_df$detailitems, pattern = "[0-9]+\\/[0-9]{4}")
tesla_df$årgang <- rækkeår

#### Kørte km ####
kørtekm <- str_extract(tesla_df$detailitems, pattern = "[0-9]+\\.[0-9]+")
tesla_df$kørtekm <- kørtekm
tesla_df$kørtekm <- gsub("\\.", "", tesla_df$kørtekm)
tesla_df$kørtekm <- as.numeric(tesla_df$kørtekm)

#### Fjern details og property kolonne ####
## Alt dataen i disse er extracted og er dermed overflødige
tesla_df <- tesla_df[,-c(2,4)]

#### Pris ####
tesla_df$price <- gsub("\\s*kr\\.", "", tesla_df$price)
tesla_df$price <- str_trim(tesla_df$price, side = "right")
tesla_df$price <- gsub("\\.", "", tesla_df$price)
tesla_df$price <- as.numeric(tesla_df$price)

#### Description ####
## newlines
tesla_df$description <- gsub("\n", ".", tesla_df$description)

## Emojis
tesla_df$description <- stringi::stri_replace_all_regex(
  tesla_df$description,
  "[\\p{So}\\p{Cn}]", # Unicode-egenskaber: Symboler og ukendte tegn
  "",
  vectorize_all = FALSE
)

## Spaces
tesla_df$description <- gsub("\\s+", " ", tesla_df$description)

## Understreger
tesla_df$description <- gsub("\\_+", ".", tesla_df$description)

## Punktummer
tesla_df$description <- gsub("\\.+", ".", tesla_df$description)

## Trim
tesla_df$description <- trimws(tesla_df$description)

## Opret ny df
tesla_clean <- tesla_df


#### TIlføj forhandlere ####
## Tilføjer 3 manuelt, da formålet ikke er at scrape forhandlerdata, men at kunne oprette relationel DB
tesla_clean$cvr <- NA
tesla_clean[1,11] <- "28859309"
tesla_clean[2,11] <- "34897085"
tesla_clean[3,11] <- "44934442"

#### DK MAP ####
tesla_clean$location <- tolower(tesla_clean$location)

## Erstat bogstaver
tesla_clean$location <- gsub("ø", "oe", tesla_clean$location)
tesla_clean$location <- gsub("æ", "ae", tesla_clean$location)
tesla_clean$location <- gsub("å", "aa", tesla_clean$location)

## Extract kun by, fjern region
by <- str_extract(tesla_clean$location, pattern = ".+\\s*\\,")
tesla_clean$location <- by
tesla_clean$location <- gsub(",", "", tesla_clean$location)

## DK Map
freqbyer <- data.frame(table(tesla_clean$location))
colnames(freqbyer) <- c("By", "Antal")

bilmap <- mapDK(values = "Antal", id = "By", data = freqbyer) +
  labs(caption = "Data hentet fra Bilbasen.dk",
       title = "Flest Tesla Model 3 til salg i Silkeborg")

bilmap
