#### Opgave 1.3 ####
#### Load clean dataset ####
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA4")
tesla_clean <- readRDS("tesla_clean.rds")
tesla_sim <- tesla_clean

#### Opdater scrapedate ####
## kører class og kan se scrapedate er i POSIXct, så kan blot lægge 86400 sekunder til (ét døgn)
tesla_sim$scrapedate <- tesla_sim$scrapedate + 86400

#### Slet 5 rækker ####
set.seed(10)
tilf_rækker <- sample(1:nrow(tesla_sim), size = 5, replace = F)
tesla_sim <- tesla_sim[-c(tilf_rækker),]
rownames(tesla_sim) <- NULL

#### Ændr 3 priser ####
set.seed(20)
tilf_pris <- sample(1:nrow(tesla_sim), size = 3, replace = F)
tesla_sim$price[c(tilf_pris)] <- tesla_sim$price[c(tilf_pris)] * 0.95 ## Prisreduktion på 5%

#### Tilføj 2 nye biler ####
## Tager gennemsnitsprisen og gennemsnits kørte km  for 2 forskellige modeller
model1_pris <- round(mean(tesla_sim$price[tesla_sim$model == "Tesla Model 3RWD 4d"]))
model1_km <- round(mean(tesla_sim$kørtekm[tesla_sim$model == "Tesla Model 3RWD 4d"]))

model2_pris <- round(mean(tesla_sim$price[tesla_sim$model == "Tesla Model 3Performance AWD 4d"]))
model2_km <- round(mean(tesla_sim$kørtekm[tesla_sim$model == "Tesla Model 3Performance AWD 4d"]))

## carid til numeric så der kan lægges +1 til når jeg laver nyt id
tesla_sim$carid <- as.numeric(tesla_sim$carid)

## Gemmer sidste scrapedate
scrape <- as.POSIXct(tesla_sim[531, 7], origin = "1970-01-01", tz = "CET")

## Bil 1
bil1 <- c(model1_pris, 
          "Tesla Model 3RWD 4d", 
          tesla_sim[4,3], 
          tesla_sim[25,4], 
          0, 
          max(tesla_sim$carid)+1,
          scrape, 
          491, 
          "6/2022", 
          model1_km,
          0)

## Bil 2
bil2 <- c(model2_pris, 
          "Tesla Model 3Performance AWD 4d", 
          tesla_sim[1,3], 
          tesla_sim[19,4], 
          0, 
          max(tesla_sim$carid)+2,
          scrape, 
          567, 
          "8/2021", 
          model2_km,
          0)

#### Ny dataframe med to nye biler ####
nyebiler <- data.frame(matrix(nrow = 0, ncol = 11))
nyebiler[1,] <- bil1
nyebiler[2,] <- bil2
colnames(nyebiler) <- colnames(tesla_sim)

#### Opdater scrapedate til POSIXct ####
nyebiler$scrapedate <- as.POSIXct(tesla_sim[531, 7], origin = "1970-01-01", tz = "CET")

#### Opdater øvrige datatyper ####
nyebiler$price <- as.numeric(nyebiler$price)
nyebiler$carid <- as.numeric(nyebiler$carid)
nyebiler$rækkevide_km <- as.numeric(nyebiler$rækkevide_km)
nyebiler$kørtekm <- as.numeric(nyebiler$kørtekm)

str(nyebiler)
#### Rbind ####
tesla_sim <- rbind(tesla_sim, nyebiler)
