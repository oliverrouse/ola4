#### Opgave 1.4 - Hente tyske data ####
#12gebrauchtwagen scrape
library(rvest)
library(httr)
library(tidyverse)

# Headers til HTTP-anmodning
de_headers <- add_headers(
  `:authority` = "www.12gebrauchtwagen.de",
  `:method` = "GET",
  `:scheme` = "https",
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

# URLs til scraping
de_starturl <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmd%5D=1874&s%5Bmk%5D=69&s%5Bpr_max%5D=500000&s%5Bpr_min%5D=500"
de_nexturl <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmd%5D=1874&s%5Bmk%5D=69&s%5Bpr_max%5D=500000&s%5Bpr_min%5D=500&page="

# CSS-selectors for elementerne
car_container <- "div.columns.car-ad.offers-gap"    # Hele containeren
title_tag <- "h3.h4.truncate div.font-bold"         # Titel
price_tag <- "div.purchase-price"                   # Pris
location_tag <- "div.text-md.mt-half.location a"    # By
km_tag <- "div.text-md.mt-half.mileage"             # Kilometerstand
power_tag <- "div.text-md.mt-half.power"            # Effekt
reg_date_tag <- "div.text-md.mt-half.reg_year"      # Registreringsdato
fuel_type_tag <- "div.text-md.mt-half.fuel_type"    # Brændstoftype
co2_tag <- "div.text-md.mt-half.co2_emissions"      # CO2-emissioner

# Dataframe til resultater
de_data <- data.frame(
  title = character(),
  price = character(),
  location = character(),
  km = character(),
  power = character(),
  reg_date = character(),
  fuel_type = character(),
  co2_emissions = character(),
  scrapedate = as.Date(character()),
  stringsAsFactors = FALSE
)

# Scraping-loop
for (de_page in 1:74) {
  de_url <- ifelse(de_page == 1, de_starturl, paste0(de_nexturl, de_page))
  de_rawres <- GET(url = de_url, de_headers)
  
  if (de_rawres$status_code != 200) {
    message(paste("Failed to fetch page:", de_page, "with status:", de_rawres$status_code))
    next
  }
  
  de_rawcontent <- httr::content(de_rawres, as = "text", encoding = "UTF-8")
  de_page_content <- read_html(de_rawcontent)
  
  # Scrape hver container
  car_cards <- de_page_content %>% html_elements(car_container)
  
  for (card in car_cards) {
    title <- card %>% html_element(title_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    price <- card %>% html_element(price_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    location <- card %>% html_element(location_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    km <- card %>% html_element(km_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    power <- card %>% html_element(power_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    reg_date <- card %>% html_element(reg_date_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    fuel_type <- card %>% html_element(fuel_type_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    co2_emissions <- card %>% html_element(co2_tag) %>% html_text(trim = TRUE) %>% ifelse(is.null(.), NA, .)
    
    # Tilføj data til dataframen
    temp_data <- data.frame(
      title = title,
      price = price,
      location = location,
      km = km,
      power = power,
      reg_date = reg_date,
      fuel_type = fuel_type,
      co2_emissions = co2_emissions,
      scrapedate = Sys.Date(),
      stringsAsFactors = FALSE
    )
    
    de_data <- bind_rows(de_data, temp_data)
  }
  
  message(paste("Finished scraping page", de_page))
  Sys.sleep(runif(1, 2, 5))  # Tilføj pause for at undgå blokering
}

# Fjerner duplikerede data
de_data <- distinct(de_data)

#Tjek hvor mange rækker der er i datasættet 
message("Scraping complete. Number of rows in dataset: ", nrow(de_data))

#Gem som RDS fil
saveRDS(de_data, "de_gebrauchtwagen_data.rds")









# Indlæs Tesla-data
tesla_df <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/tesla_df.RDS")
tesla_df_cleaned <- readRDS("Documents/Dataanlyse/1_semester/R/OLA4/tesla_clean.RDS")
de_data <- readRDS("de_gebrauchtwagen_data.rds")



#metode: med afbalanceret data

# Saml data frames med rette kolonner
tesla_df$country <- "Denmark"
de_data$country <- "Germany"

# Vælg og rens kolonner fra tesla_df
tesla_df_cleaned <- tesla_df %>%
  select(price, km, reg_date, location, country) %>%  # Vælg kun relevante kolonner
  mutate(
    price = as.numeric(gsub("[^0-9]", "", price)),  # Fjern specialtegn fra price og konverter til numerisk
    km = as.numeric(km),                           # Konverter km til numerisk
    reg_date = as.character(reg_date)              # Konverter reg_date til karakter
  )

# Vælg og rens kolonner fra de_data
de_data_cleaned <- de_data %>%
  select(price, km, reg_date, location, country) %>%
  mutate(
    price = as.numeric(gsub("[^0-9]", "", price)),  # Fjern specialtegn fra price
    km = as.numeric(gsub("[^0-9]", "", km)),       # Fjern specialtegn fra km
    reg_date = as.character(reg_date)              # Konverter reg_date til karakter
  )

# Kombiner datasæt og konverter tyske priser til DKK
exchange_rate <- 7.45
combined_data <- bind_rows(tesla_df_cleaned, de_data_cleaned) %>%
  mutate(
    price = ifelse(country == "Germany", price * exchange_rate, price),  # Omregn tyske priser
    reg_year = as.numeric(sub(".*/", "", reg_date))  # Ekstrahér registreringsår
  )

# Fjern outliers
combined_data_cleaned <- combined_data %>%
  filter(
    price >= 10000 & price <= 420000,  # Fjern biler med ekstreme priser
    km >= 1000 & km <= 200000          # Fjern biler med ekstreme km-tal
  )

# Afbalancer datasæt pr. år
balanced_data_filtered <- combined_data_cleaned %>%
  group_by(reg_year) %>%
  group_modify(~ {
    dk_cars <- .x %>% filter(country == "Denmark")
    de_cars <- .x %>% filter(country == "Germany")
    
    n <- min(nrow(dk_cars), nrow(de_cars))  # Mindste antal biler pr. år
    
    bind_rows(
      dk_cars %>% sample_n(n),  # Tag en tilfældig prøve på n fra Danmark
      de_cars %>% sample_n(n)   # Tag en tilfældig prøve på n fra Tyskland
    )
  }) %>%
  ungroup()

# Beregn gennemsnitlige priser pr. år og land
avg_price_by_year_filtered <- balanced_data_filtered %>%
  group_by(country, reg_year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(country, reg_year)

# Beregn prisforskel pr. år
price_comparison <- balanced_data_filtered %>%
  group_by(reg_year) %>%
  summarise(
    avg_price_dk = round(mean(price[country == "Denmark"], na.rm = TRUE)),  # Gennemsnit DK
    avg_price_de = round(mean(price[country == "Germany"], na.rm = TRUE)),  # Gennemsnit DE
    price_diff = abs(avg_price_dk - avg_price_de),  # Absolut prisforskel
    direction = ifelse(avg_price_dk > avg_price_de, "Danmark dyrere", "Tyskland dyrere")  # Retning
  )


# Visualisering: Gennemsnitlig pris pr. år og land
library(ggplot2)
library(scales)
ggplot(avg_price_by_year_filtered, aes(x = reg_year, y = avg_price, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
  labs(
    title = "Prisen på elbiler i Danmark og Tyskland stiger parallelt over tid",
    subtitle = "Sammenligning af gennemsnitlige priser for elbiler (kun fælles år, balanceret antal biler)",
    x = "Registreringsår",
    y = "Gennemsnitlig pris (DKK i tusinder)",
    color = "Land"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +  # Formatér y-aksen i tusinder
  scale_x_continuous(breaks = seq(min(avg_price_by_year_filtered$reg_year), 
                                  max(avg_price_by_year_filtered$reg_year), by = 1)) +  # Sørg for intervaller pr. år
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Visualisering: Prisforskel pr. år
ggplot(price_comparison, aes(x = reg_year, y = price_diff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Prisforskel mellem danske og tyske elbiler over tid (afbalanceret data)",
    x = "Registreringsår",
    y = "Prisforskel (DKK)",
    caption = "Positive værdier indikerer, at danske biler er dyrere"
  ) +
  theme_minimal(base_size = 14)

